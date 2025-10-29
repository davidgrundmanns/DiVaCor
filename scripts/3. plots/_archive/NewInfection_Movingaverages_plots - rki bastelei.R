# All code for this project developed by:
# (c) Benjamin G. Engst & David M. Grundmanns

# Clear matrix #
#--------------#
rm(list = ls())
# Set working-directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# packages
library(dplyr)
options(warn=-1) # less noise pls
library(tidyr)
library(EpiEstim)

# https://www.repidemicsconsortium.org/earlyR/

#  meansi <- readline(prompt="Welche durchschnittliche serielle Intervall (in Tagen) nehmen Sie an? ")
#    meansi <- as.numeric(meansi)
#  sdsi <- readline(prompt="Welche durchschnittliche Abweichung (in Tagen) vom serielle Intervall nehmen Sie an? ")
#    sdsi <- as.numeric(sdsi)

#------------------------------------------------------------------------------#
# User Input
#------------------------------------------------------------------------------#
# Welche durchschnittliche serielle Intervall (in Tagen) nehmen Sie an?
meansi <- 4 # Quelle: https://www.medrxiv.org/content/10.1101/2020.02.19.20025452v4 
# Welche durchschnittliche Abweichung (in Tagen) vom serielle Intervall nehmen Sie an?
sdsi <- 4.75 # Quelle: https://www.medrxiv.org/content/10.1101/2020.02.19.20025452v4 
# Auf wieviele Tage soll R0 berechnet werden? (mind. 2!)
windowr0 <- 7
# Welches Bundesland?
bundesland <- "Brandenburg"
# Welcher Landkreis?
kommune <- "DE401"
#------------------------------------------------------------------------------#

setwd("../../data/corona/rki")
library(readxl)
nowcast <- read_excel("Nowcasting_Zahlen.xlsx", sheet = "Nowcast_R")
write.csv2(nowcast,"nowcast.csv")
nowcastcsv <- read.csv2("nowcast.csv")
nowcastcsv$X <- NULL
colnames(nowcastcsv) <- c("Datum",  "NeuErkr",  "lb_NeuErkr",  "ub_NeuErkr", "NeuErkr_ma4",  "lb_NeuErkr_ma4",  "ub_NeuErkr_ma4",  "R"  ,  "lb_R",  "ub_R", "R_7Tage",  "lb_R_7Tage",  "ub_R_7Tage")

#---------------------#
# population by nuts3 #
# set working directory #
setwd("../../socio/") # for this chunk open data/analyses/ folder in relative path
pop <- read.csv2("Data_population_nuts.csv") # "Pure" cases

# population data management #
pop$NUTS3 <- as.character(pop$NUTS3) # as character
pop <- subset(pop, !(pop$NUTS3 == "")) # omit empty
pop$statenuts <- substr(pop$NUTS3, 1, 3) # create state identifier

# rbind state pop to pop data as sum of NUTS3 pops
temp <- aggregate(pop$Einwohnerzahl, by=list(statenuts=pop$statenuts), FUN=sum)
colnames(temp) <- c("NUTS3","Einwohnerzahl")
pop$statenuts <- NULL
pop <- rbind(pop, temp)
pop$Einwohnerzahl <- as.numeric(pop$Einwohnerzahl)
pop <- rbind(pop, c(sum(pop$Einwohnerzahl[nchar(pop$NUTS3) == 3]), "federal"))
rm(temp) # remove temp data

#-----------------------#
# set working directory #
setwd("../../data/analyses/") # for this chunk open data/analyses/ folder in relative path
# rki <- read.csv2("DataAnalysisRKI.csv") # With Policy Measures
rki <- read.csv2("DataAnalysis_rki.csv") # "Pure" cases
enddate <- max(as.Date(rki$reportdate, "%Y-%m-%d"))
rki$state <- gsub("Ã¼", "ue", as.character(rki$state)) # fix umlaute in state names
rki$state <- gsub("ü", "ue", as.character(rki$state)) # fix umlaute in state names

berlin_nuts <- rki[grepl("Berlin",rki$county) == TRUE,]
berlin_nuts <- distinct(berlin_nuts, county, nuts)

rki_statenuts <- unique(rki[,c("nuts","state")])
states_nuts <- rki_statenuts
states_nuts$statenuts <- substr(states_nuts$nuts, 1, 3)
states_nuts <- distinct(states_nuts, state, statenuts)

rki_countynuts <- unique(rki[,c("nuts","county")])

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


#rki_county <- subset(rki_county, rki_county$)

# Data on federal level
rki_fed <- rki %>%
  group_by(reportdate) %>%
  summarize(cases = sum(cases, na.rm = TRUE))
rki_fed$state <- "federal"

nowcastcsv$Datum <- as.Date(nowcastcsv$Datum, format = "%Y-%m-%d")
fed_nowcast <- merge(rki_fed, nowcastcsv, by.x = "reportdate", by.y = "Datum")
fed_nowcast$diff <- fed_nowcast$cases - fed_nowcast$NeuErkr_ma4

# federal cumulative cases
rki_fed$cumcases <- 1
for(i in 2:nrow(rki_fed)){
  rki_fed$cumcases[i] <- rki_fed$cases[i] + rki_fed$cumcases[i-1]
}
rki_fed$state <- "federal"
rki_fed$dayssincefirst <- min(rki_fed$reportdate)
rki_fed$dayssinceten <- min(rki_fed$reportdate[rki_fed$cumcases >= 10])
rki_fed$dayssincefirst <- rki_fed$reportdate - rki_fed$dayssincefirst
rki_fed$dayssinceten <- rki_fed$reportdate - rki_fed$dayssinceten


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
rki_county$day <- 1
rki_county$mainf <- NA
for(i in 2:nrow(rki_county)){
  if(rki_county$nuts[i] == rki_county$nuts[i-1]){
    rki_county$cumcases[i] <- rki_county$cases[i] + rki_county$cumcases[i-1]
    rki_county$day[i] <- rki_county$day[i] + rki_county$day[i-1]
    if(rki_county$day[i] >= 7){
      rki_county$mainf[i] <- (sum(rki_county$cases[(i-6):i])/7)
    }
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
rki_states$day <- NULL

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
rki_county$day <- NULL

# rowbind state and federal data
rki_data <- rbind(rki_states,rki_fed)

# federal + states (merge to results data)
rki_data_ma <- select(rki_data, state, reportdate, cases, dayssincefirst, dayssinceten)
rki_data_ma <- subset(rki_data_ma, rki_data_ma$reportdate >= as.Date("2020-02-22", "%Y-%m-%d"))
rki_data_ma <- tidyr::complete(rki_data_ma, tidyr::nesting(state), reportdate)
rki_data_ma <- rki_data_ma %>% group_by(state) %>% tidyr::complete(reportdate) %>% ungroup()

# merge statenuts
rki_data_ma <- merge(rki_data_ma, states_nuts, by = "state", all.x = TRUE)
rki_data_ma$statenuts <- ifelse(rki_data_ma$state == "federal", "federal", rki_data_ma$statenuts)
# merge population
rki_data_ma <- merge(rki_data_ma, pop, by.x = "statenuts", by.y = "NUTS3", all.x = TRUE)
rki_data_ma$infectper <- (rki_data_ma$cases / as.numeric(rki_data_ma$Einwohnerzahl)) * 100
# reorder
rki_data_ma <- arrange(rki_data_ma, statenuts,  reportdate)


rki_data_ma$day <- 1
rki_data_ma$cases[is.na(rki_data_ma$cases) == TRUE] <- 0
rki_data_ma$infectper[is.na(rki_data_ma$infectper) == TRUE] <- 0
rki_data_ma$cumcases <- NA
rki_data_ma$mainf <- NA
rki_data_ma$mainf_sd <- NA
rki_data_ma$mainfper <- NA
rki_data_ma$mainfper_sd <- NA
# loop over federal and state data
rki_data_ma$cumcases[1] <- rki_data_ma$cases[1]
for(i in 2:nrow(rki_data_ma)){
  if(rki_data_ma$state[i] == rki_data_ma$state[i-1]){
    rki_data_ma$cumcases[i] <- rki_data_ma$cases[i] + rki_data_ma$cumcases[i-1]
    if(is.na(rki_data_ma$cumcases[i]) == TRUE){
      rki_data_ma$cumcases[i] <- rki_data_ma$cumcases[i-1]
    }
    rki_data_ma$day[i] <- rki_data_ma$day[i] + rki_data_ma$day[i-1]
    if(rki_data_ma$day[i] >= 7){
      rki_data_ma$mainf[i] <- (sum(rki_data_ma$cases[(i-6):i])/7)
      rki_data_ma$mainf_sd[i] <- sd(rki_data_ma$cases[(i-6):i])
      rki_data_ma$mainfper[i] <- (sum(rki_data_ma$infectper[(i-6):i])/7)
      rki_data_ma$mainfper_sd[i] <- sd(rki_data_ma$infectper[(i-6):i])
    }
  }
  # if start with new state, then start cumcases from initial casenumber
  else {
    rki_data_ma$cumcases[i] <- rki_data_ma$cases[i]
  }
}
rki_data_ma$day <- NULL
rki_data_ma$cuminfectper <- (rki_data_ma$cumcases / as.numeric(rki_data_ma$Einwohnerzahl)) * 100

# county (merge to results data)
rki_county_ma <- select(rki_county, nuts, reportdate, cases, dayssincefirst, dayssinceten)
rki_county_ma <- subset(rki_county_ma, rki_county_ma$reportdate >= as.Date("2020-02-22", "%Y-%m-%d"))
rki_county_ma <- tidyr::complete(rki_county_ma, tidyr::nesting(nuts), reportdate)
rki_county_ma <- rki_county_ma %>% group_by(nuts) %>% tidyr::complete(reportdate) %>% ungroup()

# merge pop data
rki_county_ma <- merge(rki_county_ma, pop, by.x = "nuts", by.y = "NUTS3", all.x=TRUE)
rki_county_ma$infectper <- (rki_county_ma$cases / as.numeric(rki_county_ma$Einwohnerzahl)) * 100
# reorder
rki_county_ma <- arrange(rki_county_ma, nuts, reportdate)

rki_county_ma$day <- 1
rki_county_ma$cases[is.na(rki_county_ma$cases) == TRUE] <- 0
rki_county_ma$infectper[is.na(rki_county_ma$infectper) == TRUE] <- 0
rki_county_ma$cumcases <- NA
rki_county_ma$mainf <- NA
rki_county_ma$mainf_sd <- NA
rki_county_ma$mainfper <- NA
rki_county_ma$mainfper_sd <- NA
rki_county_ma$cumcases[1] <- rki_county_ma$cases[1]
# loop over county data 
for(i in 2:nrow(rki_county_ma)){
  if(rki_county_ma$nuts[i] == rki_county_ma$nuts[i-1]){
    rki_county_ma$cumcases[i] <- rki_county_ma$cases[i] + rki_county_ma$cumcases[i-1]
    if(is.na(rki_county_ma$cumcases[i]) == TRUE){
      rki_county_ma$cumcases[i] <- rki_county_ma$cumcases[i-1]
    }
    rki_county_ma$day[i] <- rki_county_ma$day[i] + rki_county_ma$day[i-1]
    if(rki_county_ma$day[i] >= 7){
      rki_county_ma$mainf[i] <- (sum(rki_county_ma$cases[(i-6):i])/7)
      rki_county_ma$mainf_sd[i] <- sd(rki_county_ma$cases[(i-6):i])
      rki_county_ma$mainfper[i] <- (sum(rki_county_ma$infectper[(i-6):i])/7)
      rki_county_ma$mainfper_sd[i] <- sd(rki_county_ma$infectper[(i-6):i])
    }
  }
  # if start with new state, then start cumcases from initial casenumber
  else {
    rki_county_ma$cumcases[i] <- rki_county_ma$cases[i]
  }
}
rki_county_ma$day <- NULL
# cumulative infected percent
rki_county_ma$cuminfectper <- (rki_county_ma$cumcases / as.numeric(rki_county_ma$Einwohnerzahl)) * 100

# bind cumulative state cases to county data
statecount <- rki_states[,c("state","reportdate","cumcases")]
colnames(statecount) <- c("state","reportdate","statecumcases")
rki_county <- merge(rki_county, rki_statenuts, by = c("nuts"), all.x = TRUE)
rki_county <- merge(rki_county, statecount, by = c("state","reportdate"), all.x = TRUE)

# Create state data #
#-------------------#
states <- c(unique(rki_data$state))
counties <- c(unique(as.character(rki_county$nuts)))
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
#configr <- make_config(method = "parametric_si", mean_si = meansi, std_si = sdsi)    # https://www.medrxiv.org/content/10.1101/2020.02.19.20025452v4 

for(i in 1:length(states)){
  tempstate <- subset(rki_data_ma, rki_data_ma$state %in% states[i])
  statestart <- as.Date(tempstate$reportdate[tempstate$dayssinceten == min(tempstate$dayssinceten, na.rm = TRUE) & is.na(tempstate$dayssinceten) == FALSE], "%Y-%m-%d")
  #if(statestart < enddate){
  statetime <- as.data.frame(seq(statestart, enddate, by = 1))
  statevec <- rep(paste0(states[i]), length = nrow(statetime))
  statetime <- cbind(statevec,statetime)
  colnames(statetime) <- c("state","reportdate")
  
  tempstate <- select(tempstate, reportdate, cases, mainf)
  tempstate <- merge(statetime, tempstate, by = "reportdate", all.x = TRUE)
  tempstate <- subset(tempstate, tempstate$reportdate >= startdate)
  tempstate$cases <- ifelse(is.na(tempstate$cases) == TRUE, 0 , tempstate$cases)
  
  # Estimate R0 #
  #-------------#
  
  tstart <- seq(2, length(sort(tempstate$reportdate[1:(length(tempstate$reportdate) - (windowr0 - 1))])))
  tend  <- tstart + (windowr0 - 1)
  
  tempstate$R_Wert <- NA
  for(j in 11:nrow(tempstate)){
    tempstate$R_Wert[j] <- sum(tempstate$cases[j:(j-6)]) / sum(tempstate$mainf[(j-4):(j-10)])
  }
  
  
  #er <- estimate_R(tempstate[,"mainf"]
  #                 , method = "parametric_si"
  #                 , config = make_config(list(
  #                   mean_si = meansi, std_si = sdsi, 
  #                   t_start = tstart, t_end = tend
  #                 ))) # one of 4
  # Save Output #
  #output <- as.data.frame(er$R)
  #output$date <- tempstate$reportdate[2:(nrow(output)+1)]
  #output$name <- states[i]
  tempstate <- merge(tempstate, rki_data_ma, by.x = c("state","reportdate"), by.y = c("state","reportdate"), all.x = TRUE)
  tempstate <- dplyr::select(tempstate, state, reportdate, R_Wert, cases.x, cumcases, mainf.x, mainf_sd, Einwohnerzahl, infectper, cuminfectper, mainfper, mainfper_sd)
  write.csv2(tempstate, paste0("../rnull/sevenday/",states[i],"_r.csv"))
  # Safe dataset in R
  
  states[i] <- gsub("-","",states[i])
  states[i] <- tolower(states[i])
  assign(paste0("dat_",states[i]), tempstate)
  rm(tempstate,statestart,statetime,statevec)
  #} else{print(i)}
  print(paste0(i,"-",states[i]))
}

#i <- 1

for(i in 1:length(counties)){
  tempcounty <- subset(rki_county_ma, as.character(rki_county_ma$nuts) %in% counties[i])
  countystart <- as.Date(tempcounty$reportdate[tempcounty$dayssinceten == 0 & is.na(tempcounty$dayssinceten) == FALSE], "%Y-%m-%d")
  #if(statestart < enddate){
  countytime <- as.data.frame(seq(countystart, enddate, by = 1))
  countyvec <- rep(paste0(counties[i]), length = nrow(countytime))
  countytime <- cbind(countyvec,countytime)
  colnames(countytime) <- c("state","reportdate")
  
  tempcounty <- select(tempcounty, reportdate, cases)
  tempcounty <- merge(countytime, tempcounty, by = "reportdate", all.x = TRUE)
  tempcounty <- subset(tempcounty, tempcounty$reportdate >= startdate)
  tempcounty$cases <- ifelse(is.na(tempcounty$cases) == TRUE, 0 , tempcounty$cases)
  
  tstart <- seq(2, length(sort(tempcounty$reportdate[1:(length(tempcounty$reportdate) - (windowr0 - 1))])))
  tend  <- tstart + (windowr0 - 1)
  
  tempcounty$R_Wert <- NA
  for(j in 8:nrow(tempcounty)){
    tempcounty$R_Wert[j] <- sum(tempcounty$cases[j:(j-6)]) / sum(tempcounty$mainf[(j-4):(j-10)])
  }
  
  # Estimate R0 #
  #-------------#
  #er <- estimate_R(tempcounty[,"cases"]
  #                 , method = "parametric_si"
  #                 , config = make_config(list(
  #                   mean_si = meansi, std_si = sdsi, 
  #                   t_start = tstart, t_end = tend
  #                 ))) # one of 4
  
  # Save Output #
  #output <- as.data.frame(er$R)
  #output$date <- tempcounty$reportdate[2:(nrow(output)+1)]
  #output$name <- counties[i]
  tempcounty <- merge(tempcounty, rki_county_ma, by.x = c("state","reportdate"), by.y = c("nuts","reportdate"), all.x = TRUE)
  tempcounty <- dplyr::select(tempcounty, state, reportdate, R_Wert, cases.x, cumcases, mainf, mainf_sd, Einwohnerzahl, infectper, cuminfectper, mainfper, mainfper_sd)

  
  write.csv2(tempcounty, paste0("../rnull/sevenday/",counties[i],"_r.csv"))
  # Safe dataset in R
  
  #counties[i] <- gsub("-","",counties[i])
  #counties[i] <- tolower(counties[i])
  assign(paste0("dat_",counties[i]), tempcounty)
}

listofdata <- grep("dat_", names(.GlobalEnv), value=TRUE)
#fulldata <- NA
#for(i in 1:length(listofdata)){
#  fulldata <- rbind(fulldata,get(paste0(listofdata[i])))
#}
#fulldata <- fulldata[complete.cases(fulldata),]

# The plot is developed for Germany and the state of Brandenburg
#----------------------------------------------------------------
ger <- dat_federal
  colnames(ger) <- gsub("\\(R\\)", "", colnames(ger))
  
  # Transform the state name
  staat <- tolower(bundesland)
  staat <- gsub("-","", staat)
  staat <- gsub("ü","ue", staat)
  staat <- gsub("[^a-z]", "", staat)
  
  # Transform the state name
  rki_statenuts$state <- tolower(rki_statenuts$state)
  rki_statenuts$state <- gsub("-","", rki_statenuts$state)
  rki_statenuts$state <- gsub("ü","ue", rki_statenuts$state)
  rki_statenuts$state <- gsub("[^a-z]", "", rki_statenuts$state)
  
  
  
# PLOTS #
# federal: percentage
png(paste0("../../outcome/avginfplots/federal_neuinf.png"), width=10, height=5, bg = NA, units = 'in', res = 300)
par(mar=c(7.5, 5, 2, 0)) 
plot(seq(1, nrow(ger), 1)
     , las = 1
     , type="n"
     , ylim=c(0,(max(c(max(rki_data_ma$mainfper, na.rm = T)))+0.001)) 
     , xlim=c(1,nrow(ger))
     , xlab=""
     , xaxt="n"
     , ylab=""
     , main= "Gleitender durchschnittlicher Anteil der Gesamtbevölkerungen mit Neuinfektionen von COVID-19 in % (pro Woche)"
     , cex.lab = .9
     , cex.main = .9
     , col.axis = "darkgray"
     , col.lab = "darkgray" 
     , fg = "darkgray"
     , frame.plot=F)

# federal
points(ger$t_start-1, ger$mainfper, pch=20, cex=1, col="black", type = "b") 
# state
#points((state$t_start-1 + nrow(ger) - nrow(state)), state$mainfper, pch=20, cex=1, col="black", type = "b")

# For the x-Axis
upperdate <- as.Date(ger$date, "%Y-%m-%d") + 6
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
     , cex.axis = .8
     , col = "darkgray"
     , col.axis = "darkgray"
     , col.lab = "darkgray") 

max_tick <- par("usr")[4]
text(median(seq(1, nrow(ger), 1)), (par("usr")[4] * 1.05)
     , paste("für die Bundesrepublik Deutschland unter Verwendung der offiziellen Meldezahlen des RKI"), pos = 1,  srt=0, xpd = TRUE, cex = .8,  col = "darkgray")   
mtext("Projekt DiVaCor.org | Grafik von Dr. Benjamin G. Engst & David M. Grundmanns"
      ,side=1, line=6, cex= .8, col = "darkgray", adj=-.225)
mtext("Anteil der Neuinfektionen der Gesamtbevölkerung in %"
      ,side=2, line= 4, cex=.8, col ="darkgray")
dev.off()  

  
  
# create empty object to hold nuts IDs  
tempcounties <- NA
states <- subset(states, states != "federal")
# start plot loop
for(i in 1:length(states)){ 
  print(states[i])
  #---------------#
  # by Bundesland #
state <- mget(ls(envir=.GlobalEnv, pattern=paste0("dat_",states[i])))[[1]]
colnames(state) <- gsub("\\(R\\)", "", colnames(state))

# durchschnittliche Neuinfektionen as % by Bundesland
png(paste0("../../outcome/avginfplots/",states[i],"_neuinf.png"), width=10, height=5, bg = NA, units = 'in', res = 300)
par(mar=c(7.5, 5, 2, 0)) 
plot(seq(1, nrow(ger), 1)
     , las = 1
     , type="n"
     , ylim=c(0,(max(c(max(rki_data_ma$mainfper, na.rm = T)))+0.001))   
     , xlim=c(1,nrow(ger))
     , xlab=""
     , xaxt="n"
     , ylab=""
     , main="Gleitender durchschnittlicher Anteil der Gesamtbevölkerungen mit Neuinfektionen von COVID-19 in % (pro Woche)"
     , cex.lab = .9
     , cex.main = .9
     , col.axis = "darkgray"
     , col.lab = "darkgray" 
     , fg = "darkgray"
     , frame.plot=F)

# federal
points(ger$t_start-1, ger$mainfper, pch=18, cex=1, col=adjustcolor( "gray30", alpha.f = .5), type = "b") 
# state
points((state$t_start-1 + nrow(ger) - nrow(state)), state$mainfper, pch=20, cex=1, col="black", type = "b") 

# For the x-Axis
upperdate <- as.Date(ger$date, "%Y-%m-%d") + 6
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
     , cex.axis = .8
     , col = "darkgray"
     , col.axis = "darkgray"
     , col.lab = "darkgray") 

max_tick <- par("usr")[4]
text(median(seq(1, nrow(ger), 1)), (par("usr")[4] * 1.05)
     , paste("für das Bundesland",unique(state$name),"unter Verwendung der offiziellen Meldezahlen des RKI"), pos = 1,  srt=0, xpd = TRUE, cex = .8,  col = "darkgray")   
mtext("Projekt DiVaCor.org | Grafik von Dr. Benjamin G. Engst & David M. Grundmanns"
      ,side=1, line=6, cex= .8, col = "darkgray", adj=-.225)
mtext("Anteil der Neuinfektionen der Gesamtbevölkerung in %"
      ,side=2, line= 4, cex=.8, col ="darkgray")
legend("topleft"
       , legend = c(paste(unique(state$name)), "Deutschlandweit")
       , pch = c(20, 18) # Kästchen mit den vergebenen Farben
       , col = c("black","gray30")
       , pt.cex = c(1.5,1.5)
       , text.col = c("black","gray30")
       , cex = 1  # Schriftgröße
       , bg = NULL # Keine Hintergrungfarbe
       , bty = "n"
       , inset = 0.033
       , horiz = FALSE)
dev.off() # end durchschn. Neuinfektionen by Bundesland

# ---------------------------------------------------------------------------- #
# first filter for counties in state (rki_statenuts)
  tempcounties <- rki_statenuts$nuts[rki_statenuts$state == states[i]]
  # then run loop over counties #
  for(j in 1:length(tempcounties)){
    countydata <- mget(ls(envir=.GlobalEnv, pattern=paste0("dat_",as.character(tempcounties[j]))))[[1]]
      colnames(countydata) <- gsub("\\(R\\)", "", colnames(countydata))
    countyname <- subset(rki_countynuts, rki_countynuts$nuts %in% tempcounties[j])[,c("county")]
    countyname <- gsub("Ã¤", "ae", countyname)
    countyname <- gsub("Ã¶", "oe", countyname)
    countyname <- gsub("Ã¼", "ue", countyname)
    countyname <- gsub("ÃŸ", "ss", countyname)
    countyname <- gsub("^SK*.Berlin.*", "Berlin", countyname)[1]
    
    # We seek to plot the mean and the 95% comfidence intervall
    # of R0 for each week (focusing on the day a mean was estimated)
    
    # let last digit be dividable by 0.2 without adding digits
    ylimvalues <- round(c(0,(max(c(max(ger$Quantile.0.975, na.rm = T)
                             ,max(state$Quantile.0.975, na.rm = T)
                             ,max(countydata$Quantile.0.975, na.rm = T),1)
    ))))
    yaxpvalues <- c(0,round((max(c(max(ger$Quantile.0.975, na.rm = T)
                                   ,max(state$Quantile.0.975, na.rm = T)
                                   ,max(countydata$Quantile.0.975, na.rm = T)
    ))),1), round(round((max(c(max(ger$Quantile.0.975, na.rm = T)
                               ,max(state$Quantile.0.975, na.rm = T)
                               ,max(countydata$Quantile.0.975, na.rm = T)
    ))),1) / 0.2))
    yaxpvalues[2] <- ifelse(grepl("[.]", yaxpvalues[2]) == TRUE & (as.numeric(stringr::str_sub(yaxpvalues[2],-1)) %% 2) == 1, yaxpvalues[2]+0.1, yaxpvalues[2])
    yaxpvalues[3] <- yaxpvalues[2] / 0.2
    ylimvalues[2] <- yaxpvalues[2]
    
    png(paste0("../../outcome/R0plots/",tempcounties[j],"_rnull.png"), width=10, height=5, bg = NA, units = 'in', res = 300)
    par(mar=c(7.5, 5, 1, 0)) 
    plot(seq(1, nrow(ger), 1)
         , las = 1
         , type="n"
         , ylim=ylimvalues
         , xlim=c(1,nrow(ger))
         , yaxp=c(0, yaxpvalues[2], yaxpvalues[3])
         , xlab=""
         , xaxt="n" 
         , yaxt="n"
         , ylab="Durschnittliche Reproduktionszahl [R0]"
         , main= "Gleitende durchschnittliche Reproduktionszahl von COVID-19 (pro Woche)"
         , cex.lab = .9
         , cex.main = .9
         , col.axis = "darkgray"
         , col.lab = "darkgray" 
         , frame.plot=F)
    
    # R0 = 1 line
    segments(0, 1, nrow(ger), 1, col = "gray", lty = 2, lwd = 2)

    # state plot
    polygon(c(((nrow(ger) - nrow(state)) + seq(1, nrow(state), 1))
              ,rev(((nrow(ger) - nrow(state)) + seq(1, nrow(state), 1))))
            ,c(state$Quantile.0.975,rev(state$Quantile.0.025))
            ,col= adjustcolor( "darkgray", alpha.f = .2), border = adjustcolor( "darkgray", alpha.f = .2), cex = 1.75)  
    points((state$t_start-1 + nrow(ger) - nrow(state)), state$Mean, pch=20, cex=1, col="black", type = "b")   

    # federal plot
    polygon(c(seq(1, nrow(ger), 1),rev(seq(1, nrow(ger), 1)))
            ,c(ger$Quantile.0.975,rev(ger$Quantile.0.025))
            ,col=adjustcolor( "darkgray", alpha.f = .4), border = adjustcolor( "darkgray", alpha.f = .4), cex = 1.75)  
    points(ger$t_start-1, ger$Mean, pch=18, cex=1, col="black", type = "b")   

    # county plot
    polygon(c(((nrow(ger) - nrow(countydata)) + seq(1, nrow(countydata), 1))
              ,rev(((nrow(ger) - nrow(countydata)) + seq(1, nrow(countydata), 1))))
            ,c(countydata$Quantile.0.975,rev(countydata$Quantile.0.025))
            ,col= adjustcolor( "darkgray", alpha.f = .2), border = adjustcolor( "darkgray", alpha.f = .2), cex = 1.75)  
    points((countydata$t_start-1 + nrow(ger) - nrow(countydata)), countydata$Mean, pch=17, cex=1, col="black", type = "b")

    # For the x-Axis
    upperdate <- as.Date(ger$date, "%Y-%m-%d") + 6
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
         , cex.axis = .8
         , col = "darkgray"
         , col.axis = "darkgray"
         , col.lab = "darkgray" ) # D
    
    axis(2
         , at = seq(0, yaxpvalues[2], by = 0.2)
         , las=2, tck=-.02
         , cex.axis = .8
         , col = "darkgray"
         , col.axis = "darkgray"
         , col.lab = "darkgray" ) # D

    max_tick <- par("usr")[4]
    text(median(seq(1, nrow(ger), 1)), (par("usr")[4])
         , paste("Unter der Annahme eines durchschnittlichen seriellen Intervalls von",round(meansi),"Tagen \n und einer Standardabweichung von", sdsi, "Tagen \n [unter Verwendung der offiziellen Meldezahlen des RKI \n mit R - EpiEstim 2.2-1.]"), pos = 1,  srt=0, xpd = TRUE, cex = .8,  col = "darkgray")   
    #text(median(seq(1, nrow(ger), 1)), (max(c(max(ger$Quantile.0.975, na.rm = T), max(state$Quantile.0.975, na.rm = T), max(countydata$Quantile.0.975, na.rm = T))))
    #     , paste("Unter der Annahme eines durchschnittlichen seriellen Intervalls von",round(meansi),"Tagen \n und einer Standardabweichung von", sdsi, "Tagen \n mit R - EpiEstim 2.2-1."), pos = 1,  srt=0, xpd = TRUE, cex = .8,  col = "black")   
    mtext("Projekt DiVaCor.org | Grafik von Dr. Benjamin G. Engst & David M. Grundmanns"
          ,side=1, line=6, cex= .8, col = "darkgray", adj=-.225)

    legend("topright"
           , legend = c("Deutschlandweit", paste(unique(state$name)), paste(countyname), "95% Konfidenzintervall")
           , pch = c(18, 20, 17, 15) # Kästchen mit den vergebenen Farben
           , col = c("black","black","black","gray80" )
           , pt.cex = c(1,1,1,1.5)
           , text.col = c("black")
           , cex = .8  # Schriftgröße
           , bg = NULL # Keine Hintergrungfarbe
           , bty = "n"
           , inset = 0.033
           , horiz = FALSE)
    dev.off()
    # END R0 County Plot #
    # ------------------ #
    
    # ----------------------- #
    # START % MOVING AVG PLOT #
    if(tempcounties[j] != "DE600"){
    png(paste0("../../outcome/avginfplots/",tempcounties[j],"_neuinf.png"), width=10, height=5, bg = NA, units = 'in', res = 300)
    par(mar=c(7.5, 5, 2, 0)) 
    plot(seq(1, nrow(ger), 1)
         , las = 1
         , type="n"
         , ylim=c(0,(max(c(max(countydata$mainfper, na.rm = T) + 0.001),max(rki_data_ma$mainfper, na.rm = T))+0.001))  
         , xlim=c(1,nrow(ger))
         , xaxt="n"
         , ylab=""
         , xlab=""
         , main= "Gleitender durchschnittlicher Anteil der Gesamtbevölkerungen mit Neuinfektionen von COVID-19 in % (pro Woche)"
         #, sub= ""
         #, cex.sub = .8
         , cex.lab = .9
         , cex.main = .9
         , col.axis = "darkgray"
         , col.lab = "darkgray" 
         , fg = "darkgray"
         , frame.plot=F)
    
    # county plot
    points((countydata$t_start-1 + nrow(ger) - nrow(countydata)), countydata$mainfper, pch=17, cex=1, col="black", type = "b")
    # state
    points((state$t_start-1 + nrow(ger) - nrow(state)), state$mainfper, pch=20, cex=1, col=adjustcolor( "gray30", alpha.f = .5), type = "b")
    # federal
    points(ger$t_start-1, ger$mainfper, pch=18, cex=1, col=adjustcolor( "gray60", alpha.f = .5), type = "b") 
    
    # For the x-Axis
    upperdate <- as.Date(ger$date, "%Y-%m-%d") + 6
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
         , cex.axis = .8
         , col = "darkgray"
         , col.axis = "darkgray"
         , col.lab = "darkgray") 
    
    max_tick <- par("usr")[4]
    
    text(median(seq(1, nrow(ger), 1)), (par("usr")[4] * 1.05)
         , paste("für",countyname,"unter Verwendung der offiziellen Meldezahlen des RKI","\\n Projekt "), pos = 1,  srt=0, xpd = TRUE, cex = .8,  col = "darkgray")   
    mtext("Projekt DiVaCor.org | Grafik von Dr. Benjamin G. Engst & David M. Grundmanns"
          ,side=1, line=6, cex= .8, col = "darkgray", adj=-.225)
    mtext("Anteil der Neuinfektionen der Gesamtbevölkerung in %"
          ,side=2, line= 4, cex=.8, col ="darkgray")
    legend("topleft"
           , legend = c(paste(countyname), paste(unique(state$name)), "Deutschlandweit")
           , pch = c(17, 20, 18) # Kästchen mit den vergebenen Farben
           , col = c("black","gray30","gray60")
           , pt.cex = c(1.2,1.2,1.2)
           , text.col = c("black")
           , cex = 1  # Schriftgröße
           , bg = NULL # Keine Hintergrungfarbe
           , bty = "n"
           , inset = 0.033
           , horiz = FALSE)
    dev.off()
    } else{
      
      png(paste0("../../outcome/avginfplots/",tempcounties[j],"_neuinf.png"), width=10, height=5, bg = NA, units = 'in', res = 300)
      par(mar=c(7.5, 5, 2, 0)) 
      plot(seq(1, nrow(ger), 1)
           , las = 1
           , type="n"
           , ylim=c(0,(max(c(max(countydata$mainfper, na.rm = T) + 0.001),max(rki_data_ma$mainfper, na.rm = T))+0.001))
           , xlim=c(1,nrow(ger))
           , xlab=""
           , xaxt="n"
           , ylab="Anteil der Neuinfektionen an der Gesamtbevölkerung in %"
           , main= "Gleitender durchschnittlicher Anteil der Gesamtbevölkerungen mit Neuinfektionen von COVID-19 in % (pro Woche)"
           , cex.lab = .9
           , cex.main = .9
           , col.axis = "darkgray"
           , col.lab = "darkgray" 
           , fg = "darkgray"
           , frame.plot=F)
      
      # county plot for DE600
      points((state$t_start-1 + nrow(ger) - nrow(state)), state$mainfper, pch=20, cex=1, col="black", type = "b")
      # state
      #points((state$t_start-1 + nrow(ger) - nrow(state)), state$mainfper, pch=20, cex=1, col=adjustcolor( "gray30", alpha.f = .5), type = "b")
      # federal
      points(ger$t_start-1, ger$mainfper, pch=18, cex=1, col=adjustcolor( "gray60", alpha.f = .5), type = "b") 
      
      # For the x-Axis
      upperdate <- as.Date(ger$date, "%Y-%m-%d") + 6
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
           , cex.axis = .8
           , col = "darkgray"
           , col.axis = "darkgray"
           , col.lab = "darkgray") 
      
      max_tick <- par("usr")[4]
      text(median(seq(1, nrow(ger), 1)), (par("usr")[4] * 1.05)
           , paste("für",countyname,"unter Verwendung der offiziellen Meldezahlen des RKI"), pos = 1,  srt=0, xpd = TRUE, cex = .8,  col = "darkgray")   
      mtext("Projekt DiVaCor.org | Grafik von Dr. Benjamin G. Engst & David M. Grundmanns"
            ,side=1, line=6, cex= .8, col = "darkgray", adj=-.225)
      mtext("Anteil der Neuinfektionen der Gesamtbevölkerung in %"
            ,side=2, line= 4, cex=.8, col ="darkgray")
      legend("topleft"
             , legend = paste(unique(state$name), "Deutschlandweit")
             , pch = c(16,18) # Kästchen mit den vergebenen Farben
             , col = c("black","gray60")
             , pt.cex = c(1.2,1.2)
             , text.col = c("black")
             , cex = 1  # Schriftgröße
             , bg = NULL # Keine Hintergrungfarbe
             , bty = "n"
             , inset = 0.033
             , horiz = FALSE)
      dev.off()
      
    }
    # END % MOVING AVG PLOT #
    # --------------------- #
    
    
    print(tempcounties[j])
  } # end counties loop
  tempcounties <- NA
  
} # end states loop
