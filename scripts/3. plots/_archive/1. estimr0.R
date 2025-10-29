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

#-----------------------#
# set working directory #
setwd("../../data/analyses/") # for this chunk open data/analyses/ folder in relative path
# rki <- read.csv2("DataAnalysisRKI.csv") # With Policy Measures
rki <- read.csv2("DataAnalysisRKI.csv") # "Pure" cases
rki$state <- gsub("Ã¼", "ue", as.character(rki$state)) # fix umlaute in state names

  # Make report date a real date  
  rki$reportdate <- as.Date(rki$reportdate, "%Y-%m-%d")

# Data per state (daily cases and cummulative number)
rki_states <- rki %>%
  group_by(state,reportdate) %>%
  summarize(cases = sum(cases, na.rm = TRUE))

rki_fed <- rki %>%
  group_by(reportdate) %>%
  summarize(cases = sum(cases, na.rm = TRUE))
rki_fed$state <- "federal"

rki_fed$cumcases <- 1
for(i in 2:nrow(rki_fed)){
  rki_fed$cumcases[i] <- rki_fed$cases[i] + rki_fed$cumcases[i-1] 
}
rki_fed$state <- "federal"
rki_fed$dayssincefirst <- min(rki_fed$reportdate)
rki_fed$dayssinceten <- min(rki_fed$reportdate[rki_fed$cumcases >= 10])

rki_states$cumcases <- 1
for(i in 2:nrow(rki_states)){
  if(rki_states$state[i] == rki_states$state[i-1]){
  rki_states$cumcases[i] <- rki_states$cases[i] + rki_states$cumcases[i-1]
  }
  else {
    rki_states$cumcases[i] <- rki_states$cases[i]
  }
}

# First report date per state 
rki_state_start <- rki_states %>%
  group_by(state) %>%
  summarize(firstreportdate = min(reportdate))

# When first ten cases happened 
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


rki_data <- rbind(rki_states,rki_fed)

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
configr <- make_config(method = "parametric_si", mean_si = 4, std_si = 4.75)    # https://www.medrxiv.org/content/10.1101/2020.02.19.20025452v4 

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
}
