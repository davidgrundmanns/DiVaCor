# Clear matrix #
#--------------#
rm(list = ls())

#-----------------------#
# set working directory #
setwd("../../data/corona/jhu/")

# Confirmed cases #
###################
  # Get the newest file in the folder
  jhu <- max(sort(list.files("confirmed")))
  # Read that file
  jhu <- read.csv2(paste0("confirmed/",jhu))
  colnames(jhu) <- gsub("X", "", colnames(jhu))

# Reduce to data and variables of interest
jhu <- subset(jhu, jhu$Country.Region == "Germany")  
  # Delete not necessary variables
  jhu[1] <- NULL 
  jhu$Province.State <- NULL
  jhu$Lat <- NULL
  jhu$Long <- NULL 
  jhu$Country.Region <- NULL
  
  # Make real date from columnames
  day <- substr(colnames(jhu)
           ,   1
           ,  nchar(colnames(jhu))-3)
  day <- gsub(".*\\.","", day)  
  day <- ifelse(nchar(day) == 1, paste0("0",day),day)   
  month <- substr(colnames(jhu),1,3)
  month <- gsub("\\..*","", month)    
  month <- ifelse(nchar(month) == 1, paste0("0",month),month) 
  year <- paste0("20",substr(colnames(jhu)
                    ,   nchar(colnames(jhu))-1
                    ,  nchar(colnames(jhu))))
  # Connect to jhu data
  jhu <- rbind(jhu,paste0(year,month,day))
  
  # Turn the dataframe
    cases <- as.matrix(jhu[1,1:ncol(jhu)])
    cases <- c(cases)
    dates <- as.matrix(jhu[2,1:ncol(jhu)])
    dates <- c(dates)
jhu_con <- data.frame(cbind(dates,cases))
  colnames(jhu_con)[2] <- "confirmed"
  
# Recovered cases #
###################
# Get the newest file in the folder
jhu <- max(sort(list.files("recovered")))
  # Read that file
  jhu <- read.csv2(paste0("recovered/",jhu))
  colnames(jhu) <- gsub("X", "", colnames(jhu))
  
# Reduce to data and variables of interest
jhu <- subset(jhu, jhu$Country.Region == "Germany")  
  # Delete not necessary variables
  jhu[1] <- NULL 
  jhu$Province.State <- NULL
  jhu$Lat <- NULL
  jhu$Long <- NULL 
  jhu$Country.Region <- NULL
  
  # Make real date from columnames
  day <- substr(colnames(jhu)
                ,   1
                ,  nchar(colnames(jhu))-3)
  day <- gsub(".*\\.","", day)  
  day <- ifelse(nchar(day) == 1, paste0("0",day),day)   
  month <- substr(colnames(jhu),1,3)
  month <- gsub("\\..*","", month)    
  month <- ifelse(nchar(month) == 1, paste0("0",month),month) 
  year <- paste0("20",substr(colnames(jhu)
                             ,   nchar(colnames(jhu))-1
                             ,  nchar(colnames(jhu))))
# Connect to jhu data
jhu <- rbind(jhu,paste0(year,month,day))
  
  # Turn the dataframe
  cases <- as.matrix(jhu[1,1:ncol(jhu)])
  cases <- c(cases)
  dates <- as.matrix(jhu[2,1:ncol(jhu)])
  dates <- c(dates)
jhu_recov <- data.frame(cbind(dates,cases))
  colnames(jhu_recov)[2] <- "recovered"

# Dead cases #
##############
# Get the newest file in the folder
jhu <- max(sort(list.files("death")))
# Read that file
jhu <- read.csv2(paste0("death/",jhu))
colnames(jhu) <- gsub("X", "", colnames(jhu))

# Reduce to data and variables of interest
jhu <- subset(jhu, jhu$Country.Region == "Germany")  
# Delete not necessary variables
jhu[1] <- NULL 
jhu$Province.State <- NULL
jhu$Lat <- NULL
jhu$Long <- NULL 
jhu$Country.Region <- NULL

# Make real date from columnames
day <- substr(colnames(jhu)
              ,   1
              ,  nchar(colnames(jhu))-3)
day <- gsub(".*\\.","", day)  
day <- ifelse(nchar(day) == 1, paste0("0",day),day)   
month <- substr(colnames(jhu),1,3)
month <- gsub("\\..*","", month)    
month <- ifelse(nchar(month) == 1, paste0("0",month),month) 
year <- paste0("20",substr(colnames(jhu)
                           ,   nchar(colnames(jhu))-1
                           ,  nchar(colnames(jhu))))
# Connect to jhu data
jhu <- rbind(jhu,paste0(year,month,day))

# Turn the dataframe
cases <- as.matrix(jhu[1,1:ncol(jhu)])
cases <- c(cases)
dates <- as.matrix(jhu[2,1:ncol(jhu)])
dates <- c(dates)
jhu_death <- data.frame(cbind(dates,cases))
  colnames(jhu_death)[2] <- "death"

# Connect all data #
####################
jhu <- merge(jhu_con, jhu_death, by = "dates"
             , all.x = T, all.y = T)

jhu <- merge(jhu, jhu_recov, by = "dates"
             , all.x = T, all.y = T)
  
# Calculate recovery_rate #
###########################
jhu$death <- as.numeric(as.character(jhu$death))
jhu$confirmed <- as.numeric(as.character(jhu$confirmed))
jhu$recovered <- as.numeric(as.character(jhu$recovered))

# Calucaltion of the recovery rate #
#----------------------------------#
jhu$recor[jhu$recovered > 0] <- jhu$recovered[jhu$recovered > 0] / jhu$confirmed[jhu$recovered > 0]
  jhu$recor[is.na(jhu$recor)] <- 0
  # Average recovery rate
  mean(as.numeric(as.character(jhu$recor)))  
  hist(as.numeric(as.character(jhu$recor)))  
  
# Make weekly means #
#-------------------#
jhu$date <- as.Date(paste0(substr(jhu$dates, 1, 4),"-",
                           substr(jhu$dates, 5, 6),"-",
                           substr(jhu$dates, 7, 8)),
                    "%Y-%m-%d")
  jhu$dates <- NULL # not needed then anymore
jhu$weekday <- weekdays(as.Date(jhu$date))
jhu$weekno <- strftime(jhu$date, format = "%V")

# Calculate the average recovery rate per week
week <- unique(jhu$weekno)
jhu$recor_week <- NA
for (i in 1:length(week)) {
  jhu$recor_week[jhu$weekno == week[i]] <- mean(jhu$recor[jhu$weekno == week[i]])
}

# Save the data #
#################
write.csv2(jhu, "../../analyses/DataAnalysisJHU.csv")  
