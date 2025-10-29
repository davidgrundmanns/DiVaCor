library(dplyr)

# Clear matrix
rm(list = ls())

setwd('../../data/')

# Load the RKI analyze data
rki <- read.csv2("analyses/DataAnalysis_rki.csv")
  rki$X <- NULL
  sum(rki$cases)
  # In the RKI data the nuts region for Berlin is missing
  # this region is 
  
# Hospital information
hospitals <- read.csv2("analyses/DataNUTS_hospitals.csv")
  hospitals$X.1 <- NULL
  hospitals$nuts <- as.character(hospitals$nuts)
  # 14 Hospitals do not match we exclude those for now
  table(hospitals$nuts[nchar(hospitals$nuts) > 5])
  hospitals <- subset(hospitals, nchar(hospitals$nuts) == 5)
  # Get the count of hospitals in a region
  hospitals_count <- data.frame(table(hospitals$nuts))
    colnames(hospitals_count)[1] <- "nuts"    
    colnames(hospitals_count)[2] <- "hospitals"
# Merge hospital und rki
# If this is true each nuts region is included in both data
length(unique(rki$nuts)) == length(unique(hospitals_count$nuts)) 
# Merge the data
rki <- merge(rki, hospitals_count, by = c("nuts"), all.x = T, all.y = T)
  rm(hospitals, hospitals_count)

# Political measures
policies <- read.csv2("policies/Data_policy_20200325.csv") 
  policies$X <- NULL
policies <- subset(policies, select = c("location", "timestamp", "startdate_action", "action"))

# Get the hollidays out
policies$action <- as.character(policies$action)
policies$action[grepl("ferien", policies$action)] <- "WRONG"
policies$action[grepl("Aufhebung Quarantäne", policies$action)] <- "WRONG"
policies <- subset(policies, policies$action != "WRONG")

policies$timestamp <- as.character(policies$timestamp)
policies$startdate_action <- as.character(policies$startdate_action)

policies$timestamp <- ifelse(policies$timestamp == "", policies$startdate_action, policies$timestamp) 
# Two policy measures do not have a date
policies <- subset(policies, policies$timestamp != "")
policies$startdate_action <- NULL

policies$beobachtung <- 1
policies <- aggregate(policies$beobachtung, by=list(policies$location, policies$timestamp)
          , FUN=sum)

colnames(policies)[1] <- "state"
colnames(policies)[2] <- "policydate"
colnames(policies)[3] <- "policyno"

policies$state <- as.character(policies$state)
policies$state[policies$state == "Baden-Würtemberg"] <- "Baden-Württemberg" 
policies$state[policies$state == "Mecklenburg Vorpommern"] <- "Mecklenburg-Vorpommern" 
policies$state[policies$state == "Niedersachsen\t"] <- "Niedersachsen"
policies$state[policies$state == "NRW"] <- "Nordrhein-Westfalen"

rki$policy <- NA
rki$reportdate <- as.Date(rki$reportdate, "%Y-%m-%d")
policies$policydate <- as.Date(policies$policydate, "%Y-%m-%d")
# To account for the delay in sympthomes
policies$policydate <- as.Date(policies$policydate, "%Y-%m-%d") + 3

rki <- subset(rki, !is.na(rki$state))
states <- unique(as.character(rki$state))
# These are not in the policy list
states <- states[!states == "Rheinland-Pfalz"]
states <- states[!states == "Sachsen-Anhalt"]
states <- states[!states == "Schleswig-Holstein"]


statemeasure <- subset(policies, as.character(policies$state) == as.character(states[1]))  
statemeasure <- statemeasure[order(statemeasure$policydate),]
statemeasure <- statemeasure %>%
  group_by(state) %>%
  mutate(policyno = cumsum(policyno))
  
  rkistate <- subset(rki, as.character(rki$state) == as.character(states[1]))
  rkistate <- rkistate[order(rkistate$reportdate),]
  
  for(i in 1:nrow(rkistate)){
    for(k in 1:nrow(statemeasure)){
      rkistate$policy[i] <- ifelse(rkistate$reportdate[i] >= statemeasure$policydate[k], statemeasure$policyno[k], rkistate$policy[i])
    }
  }
  
  rkistate$policy[is.na(rkistate$policy)] <- 0
  
rki_final <- rkistate


for(l in 2:length(states)) {
statemeasure <- subset(policies, as.character(policies$state) == as.character(states[l]))  
statemeasure <- statemeasure[order(statemeasure$policydate),]
statemeasure <- statemeasure %>%
          group_by(state) %>%
          mutate(policyno = cumsum(policyno))


rkistate <- subset(rki, as.character(rki$state) == as.character(states[l]))
rkistate <- rkistate[order(rkistate$reportdate),]

for(i in 1:nrow(rkistate)){
  for(k in 1:nrow(statemeasure)){
    rkistate$policy[i] <- ifelse(rkistate$reportdate[i] >= statemeasure$policydate[k], statemeasure$policyno[k], rkistate$policy[i])
  }
}

rkistate$policy[is.na(rkistate$policy)] <- 0
rki_final <- rbind(rki_final, rkistate)

}

# Cases missing because of missing data
sum(rki_final$cases) - sum(rki$cases)

write.csv2(rki_final,"analyses/DataAnalysisRKI.csv")
