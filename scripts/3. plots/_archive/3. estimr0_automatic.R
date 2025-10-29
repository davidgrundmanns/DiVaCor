# All code for this project developed by:
# (c) Benjamin G. Engst & David M. Grundmanns

# Clear matrix #
#--------------#
rm(list = ls())
# Set working-directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# packages
library(dplyr)
library(EpiEstim)

# https://www.repidemicsconsortium.org/earlyR/

#  meansi <- readline(prompt="Welche durchschnittliche serielle Intervall (in Tagen) nehmen Sie an? ")
#    meansi <- as.numeric(meansi)
#  sdsi <- readline(prompt="Welche durchschnittliche Abweichung (in Tagen) vom serielle Intervall nehmen Sie an? ")
#    sdsi <- as.numeric(sdsi)

# Welche durchschnittliche serielle Intervall (in Tagen) nehmen Sie an?
meansi <- 4 # Quelle: https://www.medrxiv.org/content/10.1101/2020.02.19.20025452v4 
# Welche durchschnittliche Abweichung (in Tagen) vom serielle Intervall nehmen Sie an?
sdsi <- 4.75 # Quelle: https://www.medrxiv.org/content/10.1101/2020.02.19.20025452v4 
bundesland <- "Brandenburg"


  
#-----------------------#
# set working directory #
setwd("../../data/analyses/") # for this chunk open data/analyses/ folder in relative path
# rki <- read.csv2("DataAnalysisRKI.csv") # With Policy Measures
rki <- read.csv2("DataAnalysis_rki.csv") # "Pure" cases
rki$state <- gsub("Ã¼", "ue", as.character(rki$state)) # fix umlaute in state names
rki$state <- gsub("ü", "ue", as.character(rki$state)) # fix umlaute in state names

rki_statenuts <- unique(rki[,c("nuts","state")])

  # Make report date a real date  
  rki$reportdate <- as.Date(rki$reportdate, "%Y-%m-%d")

# Data per state (daily cases and cumulative number)
rki_states <- rki %>%
  group_by(state,reportdate) %>%
  summarize(cases = sum(cases, na.rm = TRUE))

# Data per county (daily cases and cumulative number)
rki_county <- rki %>%
  group_by(nuts,reportdate) %>%
  summarize(cases = sum(cases, na.rm = TRUE))

# Data on federal level
rki_fed <- rki %>%
  group_by(reportdate) %>%
  summarize(cases = sum(cases, na.rm = TRUE))
rki_fed$state <- "federal"

# federal cumulative cases
rki_fed$cumcases <- 1
for(i in 2:nrow(rki_fed)){
  rki_fed$cumcases[i] <- rki_fed$cases[i] + rki_fed$cumcases[i-1] 
}
rki_fed$state <- "federal"
rki_fed$dayssincefirst <- min(rki_fed$reportdate)
rki_fed$dayssinceten <- min(rki_fed$reportdate[rki_fed$cumcases >= 10])

# state cumulative cases
rki_states$cumcases <- 1
for(i in 2:nrow(rki_states)){
  if(rki_states$state[i] == rki_states$state[i-1]){
  rki_states$cumcases[i] <- rki_states$cases[i] + rki_states$cumcases[i-1]
  }
  else {
    rki_states$cumcases[i] <- rki_states$cases[i]
  }
}

# county cumulative cases
rki_county$cumcases <- 1
for(i in 2:nrow(rki_county)){
  if(rki_county$nuts[i] == rki_county$nuts[i-1]){
    rki_county$cumcases[i] <- rki_county$cases[i] + rki_county$cumcases[i-1]
  }
  else {
    rki_county$cumcases[i] <- rki_county$cases[i]
  }
}

# First report date per state 
rki_state_start <- rki_states %>%
  group_by(state) %>%
  summarize(firstreportdate = min(reportdate))
# When first ten cases happened (state)
rki_state_start_10 <- subset(rki_states, rki_states$cumcases >= 10)
rki_state_start_10 <- rki_state_start_10 %>%
  group_by(state) %>%
  summarize(firstsinceten = min(reportdate))
# Merge this information (fist case; first ten cases) to state data
rki_states <- merge(rki_states,rki_state_start, by = "state")
rki_states <- merge(rki_states,rki_state_start_10, by = "state")
rki_states$dayssincefirst <- rki_states$reportdate - rki_states$firstreportdate
rki_states$dayssinceten <- rki_states$reportdate - rki_states$firstsinceten
rki_states$firstreportdate <- NULL
rki_states$firstsinceten <- NULL

# First report date per county 
rki_county_state <- rki_county %>%
  group_by(nuts) %>%
  summarize(firstreportdate = min(reportdate))
# When first ten cases happened (county)
rki_county_start_10 <- subset(rki_county, rki_county$cumcases >= 10)
rki_county_start_10 <- rki_county_start_10 %>%
                          group_by(nuts) %>%
                          summarize(firstsinceten = min(reportdate))
# Merge this information (fist case; first ten cases) to county data
rki_county <- merge(rki_county,rki_county_state, by = "nuts")
rki_county <- merge(rki_county,rki_county_start_10, by = "nuts")
rki_county$dayssincefirst <- rki_county$reportdate - rki_county$firstreportdate
rki_county$dayssinceten <- rki_county$reportdate - rki_county$firstsinceten
rki_county$firstreportdate <- NULL
rki_county$firstsinceten <- NULL

# rowbind state and federal data
rki_data <- rbind(rki_states,rki_fed)

# bind cumulative state cases to county data
statecount <- rki_states[,c("state","reportdate","cumcases")]
colnames(statecount) <- c("state","reportdate","statecumcases")
rki_county <- merge(rki_county, rki_statenuts, by = c("nuts"), all.x = TRUE)
rki_county <- merge(rki_county, statecount, by = c("state","reportdate"), all.x = TRUE)

# Create state data #
#-------------------#
states <- c(unique(rki_data$state))
startdate <- as.Date("2020-03-01", "%Y-%m-%d")

# According to the RKI - 04/08/2020 #
#-----------------------------------#
# https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Steckbrief.html#doc13776792bodyText4
#-----------------------------------------------------------------
# Das serielle Intervall definiert das durchschnittliche Intervall 
# vom Beginn der Erkrankung eines ansteckenden Falles bis zum 
# Erkrankungsbeginn eines von diesem angesteckten Falles. Das 
# serielle Intervall ist meistens länger als die Inkubationszeit, 
# weil die Ansteckung im Allgemeinen erst dann erfolgt, wenn ein 
# Fall symptomatisch geworden ist. Das serielle Intervall lag in 
# einer Studie mit 425 Patienten im Mittel (Median) bei 7,5 (29) 
# und in einer anderen Studie bei geschätzten vier Tagen, 
# basierend auf der Analyse von 28 Infizierenden/Infizierten-Paaren (30).

# parameter values by source
#configr <- make_config(method = "parametric_si", mean_si = 7.5, std_si = 4.75) # RKI
configr <- make_config(method = "parametric_si", mean_si = meansi, std_si = sdsi)    # https://www.medrxiv.org/content/10.1101/2020.02.19.20025452v4 

for(i in 1:length(states)){
  tempstate <- subset(rki_data, rki_data$state %in% states[i])
  tempstate <- select(tempstate, reportdate, cases)
  tempstate <- subset(tempstate, tempstate$reportdate >= startdate)
  
  # Estimate R0 #
  #-------------#
  er <- estimate_R(tempstate[,"cases"]
                   , method = "parametric_si"
                   , config = configr) # one of 4
  # Save Output #
  output <- as.data.frame(er$R)
  output$date <- tempstate$reportdate[2:(nrow(output)+1)]
  write.csv2(output, paste0("../rnull/",states[i],"_r.csv"))
  # Safe dataset in R
  
  states[i] <- gsub("-","",states[i])
  states[i] <- tolower(states[i])
  assign(paste0("dat_",states[i]), output )
}


# The plot is developed for Germmany and the state of Brandenburg
#----------------------------------------------------------------
ger <- dat_federal
  colnames(ger) <- gsub("\\(R\\)", "", colnames(ger))
  
  # Transorm the state name
  staat <- tolower(bundesland)
  staat <- gsub("-","", staat)
  staat <- gsub("ü","ue", staat)
  staat <- gsub("[^a-z]", "", staat)
state <- mget(ls(envir=.GlobalEnv, pattern=paste0("dat_",staat)))[[1]]
    colnames(state) <- gsub("\\(R\\)", "", colnames(state))

# We seek to plot the mean and the 95% comfidence intervall
# of R0 for each week (focusing on the day a mean was estimated)
pdf(paste0("../../outcome/",staat,"_rnull.pdf"), width=10, height=5)
par(mar=c(5.5, 5, 1, 0)) 
plot(seq(1, nrow(ger), 1)
     , las = 1
     , type="n"
     , ylim=c(.5,(max(ger$Quantile.0.975, na.rm = T) + 1))  
     , xlim=c(1,nrow(ger))
     , xlab=""
     , xaxt="n" 
     , ylab="Durschnittliche Reproduktionszahl [R0]"
     , main= "Gleitende durchschnittliche Reproduktionszahl von COVID-19 (pro Woche)"
     , cex.lab = .9
     , cex.main = .9
     , frame.plot=F)

segments(0, 1, nrow(ger), 1, col = "gray", lty = 2, lwd = 2)

polygon(c(((nrow(ger) - nrow(state)) + seq(1, nrow(state), 1))
          ,rev(((nrow(ger) - nrow(state)) + seq(1, nrow(state), 1))))
        ,c(state$Quantile.0.975,rev(state$Quantile.0.025))
        ,col= adjustcolor( "darkgray", alpha.f = .2), border = adjustcolor( "darkgray", alpha.f = .2), cex = 1.75)  
points((state$t_start-1 + nrow(ger) - nrow(state)), state$Mean, pch=20, cex=1, col="black", type = "b")   

polygon(c(seq(1, nrow(ger), 1),rev(seq(1, nrow(ger), 1)))
        ,c(ger$Quantile.0.975,rev(ger$Quantile.0.025))
        ,col=adjustcolor( "darkgray", alpha.f = .4), border = adjustcolor( "darkgray", alpha.f = .4), cex = 1.75)  
points(ger$t_start-1, ger$Mean, pch=18, cex=1, col="black", type = "b")   

# For the x-Axis
upperdate <- as.Date(ger$date, "%Y-%m-%d") + 7
upperdate <- gsub("2020-", "", upperdate)
upperdate <- ifelse(substr(upperdate, 1,2) == "03"
                    , gsub("03-", "03/", upperdate)
                    , upperdate)
upperdate <- ifelse(substr(upperdate, 1,2) == "04"
                    , gsub("04-", "04/", upperdate)
                    , upperdate)
upperdate <- ifelse(substr(upperdate, 1,2) == "05"
                    , gsub("05-", "05/", upperdate)
                    , upperdate)

date <- gsub("2020-", "", ger$date)
date <- ifelse(substr(date, 1,2) == "03"
               , gsub("03-", "03/", date)
               , date)
date <- ifelse(substr(date, 1,2) == "04"
               , gsub("04-", "04/", date)
               , date)
date <- ifelse(substr(date, 1,2) == "05"
               , gsub("05-", "05/", date)
               , date)

weeks <- paste(date,"-",upperdate)  

axis(1
     , at = seq(1, nrow(ger), 1)
     , labels = weeks
     , las=2, tck=-.02
     , cex.axis = .8) # D

text(median(seq(1, nrow(ger), 1)), (max(c(max(ger$Quantile.0.975, na.rm = T), max(state$Quantile.0.975, na.rm = T))))
     , paste("Unter der Annahme eines durchschnittlichen seriellen Intervalls von",round(meansi),"Tagen \n und einer Standardabweichung von", sdsi, "Tagen \n mit R - EpiEstim 2.2-1."), pos = 1,  srt=0, xpd = TRUE, cex = .8,  col = "black")   
text(8, (min(c(min(ger$Quantile.0.975, na.rm = T), min(state$Quantile.0.975, na.rm = T))))
     , "Grafikcode von Dr. Benjamin G. Engst & David M. Grundmanns", pos = 1,  srt=0, xpd = TRUE, cex = .7,  col = "black")   

legend("right"
       , legend = c("Deutschlandweit", paste(bundesland), "95% Konfidenzintervall")
       , pch = c(18, 20, 15) # Kästchen mit den vergebenen Farben
       , col = c("black","black","gray80" )
       , pt.cex = c(1,1,1.5)
       , text.col = c("black")
       , cex = .8  # Schriftgröße
       , bg = NULL # Keine Hintergrungfarbe
       , bty = "n"
       , horiz = FALSE)
dev.off()

# All code for this project developed by: 
# (c) Benjamin G. Engst & David M. Grundmanns 

# Landkreis einpflegen

# BW #
# -- #

# Daten #
# ----- #
# Krankenhaus Qualitätsberichte/Statistik
# privat vs öffentlich
# Fallzahlen an Geokoordinaten

# Karte #
# ----- #

# Hackathon Europe Karte?
