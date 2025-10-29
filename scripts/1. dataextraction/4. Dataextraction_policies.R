# Clear matrix
rm(list = ls())

# Packages necessary # 
#--------------------#
# install.packages("httr")
library(httr)

setwd('../../data/policies')

# Get the policy measures #
#-------------------------#
# Source: https://github.com/DFscript/Covid_19_data_visualisation
policy <- read.csv("https://raw.githubusercontent.com/DFscript/Covid_19_data_visualisation/master/data-actions/policymeasures%20-%20measures_taken.csv")
  # Some lines where empty
  policy <- subset(policy, policy$geographic_level != "")
  # In order to save the daily data the following lines 
  # automatically create a .csv with the date an safe this
  # data locally 
  dataname <- paste0("Data_policy_",Sys.Date())
  dataname <- gsub("-","", dataname)
  # Save the daily update of the data
  write.csv2(policy, paste0(dataname,".csv"))
  
# Get the holidays measures #
#---------------------------#
# Source: https://github.com/DFscript/Covid_19_data_visualisation
holidays <- read.csv("https://raw.githubusercontent.com/DFscript/Covid_19_data_visualisation/master/data-actions/Winterferien2019-20.csv")
  # In order to save the daily data the following lines 
  # automatically create a .csv with the date an safe this
  # data locally 
  dataname <- paste0("Data_holidays_",Sys.Date())
  dataname <- gsub("-","", dataname)
  # Save the daily update of the data
  write.csv2(holidays, paste0(dataname,".csv"))