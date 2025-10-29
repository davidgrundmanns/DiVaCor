# Clear matrix
rm(list = ls())
# Set working-directory #
setwd('../../data/')

# Load Data #
#############
# Get the newest time-trend file on RKI in folder
rki <- max(sort(list.files("corona/rki/kreise/trend")))
  # Load this file
  rki <- read.csv2(paste0("corona/rki/kreise/trend/",rki), sep = ",")
    rki$X <- NULL # Exclude this count variable indicating lines

# Identify number of cases in a county on a certain day #
#########################################################
# Reduce to variables of interest    
rki <- subset(rki, select = c("Bundesland", "Landkreis"
                              , "AnzahlFall", "AnzahlTodesfall"
                              , "Meldedatum"))
  # Shorten the Meldedatum as report comes always at 
  # midnight we only need the day of a report
  rki$Meldedatum <- gsub("T.*","", rki$Meldedatum)
  # There are negative numbers in the data.
  # A comparison to the numbers reported in the 
  # dashboard shows that these need to be positive.
  # Hence, we correct for this.
      # Safe list with negativ regions for control 
      error1 <- subset(rki, rki$AnzahlFall < 0)
      error2 <- subset(rki, rki$AnzahlTodesfall < 0)
      error <- unique(rbind(error1, error2))
      write.csv2(error, "analyses/Error_rkiData.csv")  
  # Correct the error 
  rki$AnzahlFall <- ifelse(rki$AnzahlFall < 0, 0, rki$AnzahlFall)
  rki$AnzahlTodesfall <- ifelse(rki$AnzahlTodesfall < 0, 0, rki$AnzahlTodesfall)

# Now aggregate cases by Bundesland / Landkreis / Meldedatum #
#------------------------------------------------------------#
rki_cases <- aggregate(rki$AnzahlFall, by=list(rki$Bundesland
                                     , rki$Landkreis, rki$Meldedatum)
                       , FUN=sum)
  # Rename the columns to english work-environment
  colnames(rki_cases) <- c("state", "county", "reportdate", "cases")
  # Make reportdate a real date
  # Check the format of the date before conversion
  if(!is.na(table(grepl("-", rki_cases$reportdate))[2])){
    rki_cases$reportdate <- as.Date(rki_cases$reportdate, "%Y-%m-%d")
  }else{
    rki_cases$reportdate <- as.Date(rki_cases$reportdate, "%Y/%m/%d")
  }
  # and order data by the reportdate 
  rki_cases <- rki_cases[order(rki_cases$reportdate),]
  
sum(rki_cases$cases) # This should match the RKI dashboard on a given day  

# Now aggregate death by Bundesland / Landkreis / Meldedatum #
#------------------------------------------------------------#
rki_death <- aggregate(rki$AnzahlTodesfall, by=list(rki$Bundesland
                                               , rki$Landkreis, rki$Meldedatum)
                       , FUN=sum)
  # Rename the columns to english work-environment
  colnames(rki_death) <- c("state", "county", "reportdate", "death")
  # Make reportdate a real date
  # Check the format of the date before conversion
  if(!is.na(table(grepl("-", rki_death$reportdate))[2])){
    rki_death$reportdate <- as.Date(rki_death$reportdate, "%Y-%m-%d")
  }else{
    rki_death$reportdate <- as.Date(rki_death$reportdate, "%Y/%m/%d")
  }
  # Order the data by the reportdate 
  rki_death <- rki_death[order(rki_death$reportdate),]
  
sum(rki_death$death) # This should match the RKI dashboard on a given day  

# Create the dataset with allcases and death in one #
#####################################################
rki_today <- merge(rki_cases, rki_death, by = c("state", "county", "reportdate"), all.x = T, all.y = T)
  rm(rki, rki_cases, rki_death) # this data is not needed anymore

# Merge nuts regions to the data as these become our identifier #
#---------------------------------------------------------------#
# The nuts regions are included in the RKI daily reports 
# Get the newest daily report file on RKI in folder
rki <- sort(list.files("corona/rki/kreise/"))[length(list.files("corona/rki/kreise/"))-1]
# Load this file
rki <- read.csv2(paste0("corona/rki/kreise/",rki))
  # Subset to county and NUTS region [which is NUTS3 code] 
  rki <- subset(rki, select = c("county", "NUTS"))
  rki <- unique(rki)
  
  # For Berlin the NUTS Number is missing.#
  # We add this number manually           #
  #---------------------------------------#
  rki$NUTS <- as.character(rki$NUTS)
  rki$NUTS[rki$NUTS == ""] <- "DE300"
  
  # Kempten is named different in the RKI Data #
  # This is why we correct this manually       #
  #--------------------------------------------#
  rki$county <- as.character(rki$county)
  rki$county[rki$county == "SK Kempten (Allgäu)"] <- "SK Kempten"
  
# Merge Nuts regions to our data
rki_today <- merge(rki_today, rki, by = c("county"), all.x = T, all.y = T)
  colnames(rki_today)[6] <- "nuts" # Rename to lower to be consistent
  
# Some entries have errors in reporting. We 
# exclude those based on the nuts which is missing.  
rki_today <- subset(rki_today, !is.na(rki_today$nuts))  

# Quick information on data in Brandenburg #
############################################
# Data can be compared to dashboard cases  
sum(rki_today$cases[rki_today$state == "Brandenburg"])
sum(rki_today$death[rki_today$state == "Brandenburg"])
  # Brandenburg an der Havel
  sum(rki_today$cases[rki_today$nuts == "DE401"])
  sum(rki_today$death[rki_today$nuts == "DE401"])

# Write the dataset in the analyses folder #
############################################
  sum(rki_today$cases) # This should match the RKI dashboard on a given day  
write.csv2(rki_today, "analyses/DataAnalysis_rki.csv")  
  
  
  
  
  