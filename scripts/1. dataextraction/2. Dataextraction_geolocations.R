# Clear matrix
rm(list = ls())

# Packages necessary # 
#--------------------#
# install.packages("httr")
library(httr)

setwd('../../data/geolocations')

# Data on German hospitals #
#--------------------------#
# https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/348b643c8b234cdc8b1b345210975b87_0?geometry=-31.661%2C46.261%2C52.714%2C55.880
# Das OpenStreetMap© ist ein Crowd Sourced Projekt. 
# D.h. alle enthaltenen Elemente werden von Teilnehmenden 
# selbstständig kartiert. Die Qualitätskontrolle unterliegt 
# der OpenStreetMap© Gemeinschaft in Eigenregie. 

hospital <- read.csv("https://opendata.arcgis.com/datasets/348b643c8b234cdc8b1b345210975b87_0.csv")
  # In order to save the daily data the following lines 
  # automatically create a .csv with the date an safe this
  # data locally 
  dataname <- paste0("Data_hospitals_",Sys.Date())
  dataname <- gsub("-","", dataname)
  # Save the daily update of the data
  write.csv2(hospital, paste0(dataname,".csv"))

# Data on German hospitals #
#--------------------------#
# https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/2a1d004868664b56944fba3564d7ae26_0?geometry=-31.716%2C46.314%2C52.659%2C55.922
pharmacy <- read.csv("https://opendata.arcgis.com/datasets/2a1d004868664b56944fba3564d7ae26_0.csv")
  # In order to save the daily data the following lines 
  # automatically create a .csv with the date an safe this
  # data locally 
  dataname <- paste0("Data_pharmacy_",Sys.Date())
  dataname <- gsub("-","", dataname)
  # Save the daily update of the data
  write.csv2(pharmacy, paste0(dataname,".csv"))
  
# Data on German laboratories #
#-----------------------------#
# https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/d32b83755ae14721a1f96ba6c7cc3ad9_0/data
laboratories <- read.csv("https://opendata.arcgis.com/datasets/d32b83755ae14721a1f96ba6c7cc3ad9_0.csv")
  # In order to save the daily data the following lines 
  # automatically create a .csv with the date an safe this
  # data locally 
  dataname <- paste0("Data_laboratories_",Sys.Date())
  dataname <- gsub("-","", dataname)
  # Save the daily update of the data
  write.csv2(laboratories, paste0(dataname,".csv"))
  
# Data on German hotels #
#-----------------------#
# https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/3c1abcb9e7c143ecbdb3b8dc76aa380d_0?geometry=-31.729%2C46.291%2C52.646%2C55.904
hotels <- read.csv("https://opendata.arcgis.com/datasets/3c1abcb9e7c143ecbdb3b8dc76aa380d_0.csv")
  # In order to save the daily data the following lines 
  # automatically create a .csv with the date an safe this
  # data locally 
  dataname <- paste0("Data_hotels_",Sys.Date())
  dataname <- gsub("-","", dataname)
  # Save the daily update of the data
  write.csv2(hotels, paste0(dataname,".csv"))
  
# Data on German nursering #
#--------------------------#
# https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/b1dc0d7ed9114bf4bbc2a6aeec00130b_0?geometry=-10.592%2C48.802%2C31.596%2C53.614
nursering <- read.csv("https://opendata.arcgis.com/datasets/b1dc0d7ed9114bf4bbc2a6aeec00130b_0.csv")
  # In order to save the daily data the following lines 
  # automatically create a .csv with the date an safe this
  # data locally 
  dataname <- paste0("Data_nursering_",Sys.Date())
  dataname <- gsub("-","", dataname)
  # Save the daily update of the data
  write.csv2(nursering, paste0(dataname,".csv"))
  
# Data on German clinic #
#-----------------------#
# https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/7f66d6899d06486d8bc18b7e77761970_0?geometry=-31.703%2C46.174%2C52.672%2C55.809
clinic <- read.csv("https://opendata.arcgis.com/datasets/7f66d6899d06486d8bc18b7e77761970_0.csv")
  # In order to save the daily data the following lines 
  # automatically create a .csv with the date an safe this
  # data locally 
  dataname <- paste0("Data_clinic_",Sys.Date())
  dataname <- gsub("-","", dataname)
  # Save the daily update of the data
  write.csv2(clinic, paste0(dataname,".csv"))  
  
# Data on German doctors #
#------------------------#
# https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/301c2526cc6146d98bdb1f2b906f0e9b_0?geometry=-31.735%2C46.334%2C52.640%2C55.939
doctors <- read.csv("https://opendata.arcgis.com/datasets/301c2526cc6146d98bdb1f2b906f0e9b_0.csv")
  # In order to save the daily data the following lines 
  # automatically create a .csv with the date an safe this
  # data locally 
  dataname <- paste0("Data_doctors_",Sys.Date())
  dataname <- gsub("-","", dataname)
  # Save the daily update of the data
  write.csv2(doctors, paste0(dataname,".csv"))  
  
# Data on German DBNetz #
#------------------------#
# https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/esri-de-content::deutsche-bahn-streckennetz?geometry=-31.661%2C46.244%2C52.714%2C55.866
dbnetz <- read.csv("https://opendata.arcgis.com/datasets/3de5d6d8f9ea43d7a243b6beee0a2e2b_0.csv")
  # In order to save the daily data the following lines 
  # automatically create a .csv with the date an safe this
  # data locally 
  dataname <- paste0("Data_dbnetz_",Sys.Date())
  dataname <- gsub("-","", dataname)
  # Save the daily update of the data
  write.csv2(dbnetz, paste0(dataname,".csv"))  
  
# Data on German DBStop #
#-----------------------#
# https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/esri-de-content::deutsche-bahn-haltestellen?geometry=-31.663%2C46.250%2C52.712%2C55.871
dbstop <- read.csv("https://opendata.arcgis.com/datasets/dda46d26a9a84bc3aa9dc75cc3a1eaae_0.csv")
  # In order to save the daily data the following lines 
  # automatically create a .csv with the date an safe this
  # data locally 
  dataname <- paste0("Data_dbstop_",Sys.Date())
  dataname <- gsub("-","", dataname)
  # Save the daily update of the data
  write.csv2(dbstop, paste0(dataname,".csv"))   
  
# Data on German DBCenter #
#-------------------------#
# https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/esri-de-content::deutsche-bahn-reisezentren?geometry=-31.653%2C46.149%2C52.722%2C55.789
dbcenter <- read.csv("https://opendata.arcgis.com/datasets/2bd9b42865db441d82a94e698922bbcf_0.csv")
  # In order to save the daily data the following lines 
  # automatically create a .csv with the date an safe this
  # data locally 
  dataname <- paste0("Data_dbcenter_",Sys.Date())
  dataname <- gsub("-","", dataname)
  # Save the daily update of the data
  write.csv2(dbcenter, paste0(dataname,".csv"))  
  
# Data on World Port Index #
#--------------------------#
# https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/esri-de-content::world-port-index?geometry=46.000%2C-76.055%2C-44.000%2C77.277
portindex <- read.csv("https://opendata.arcgis.com/datasets/b04b76b94059436e93757c301c10026c_0.csv")
  # In order to save the daily data the following lines 
  # automatically create a .csv with the date an safe this
  # data locally 
  dataname <- paste0("Data_portindex_",Sys.Date())
  dataname <- gsub("-","", dataname)
  # Save the daily update of the data
  write.csv2(portindex, paste0(dataname,".csv"))   
  