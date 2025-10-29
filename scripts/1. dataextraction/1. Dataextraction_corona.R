# Clear matrix
rm(list = ls())

# Packages necessary # 
#--------------------#
require(rio)
#  packageVersion("rio") # 0.5.16
require(readxl)
#  packageVersion("readxl") # 1.1.3

# Session info for documentation of system 
# on which this script was designed
sessionInfo()

# R version 3.5.1 (2018-07-02)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS  10.14.6
#----------------------------------------------
#locale:
#  [1] de_DE.UTF-8/de_DE.UTF-8/de_DE.UTF-8/C/de_DE.UTF-8/de_DE.UTF-8
#
#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     
#
#loaded via a namespace (and not attached):
#  [1] compiler_3.5.1 tools_3.5.1   

# Set working directory
setwd('../../data/corona')

# Get JHU data from JHU github. #
#################################
# Terms of use:
# This GitHub repo and its contents herein, including all data, mapping, 
# and analysis, copyright 2020 Johns Hopkins University, all rights reserved, 
# is provided to the public strictly for educational and academic research 
# purposes. The Website relies upon publicly available data from multiple 
# sources, that do not always agree. The Johns Hopkins University hereby 
# disclaims any and all representations and warranties with respect to the 
# Website, including accuracy, fitness for use, and merchantability. Reliance 
# on the Website for medical guidance or use of the Website 
# in commerce is strictly prohibited.

jhu <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  # Some colnames look nasty so we correct this
  colnames(jhu) <- gsub("X", "", colnames(jhu))
  # In order to save the daily data the following lines 
  # automatically create a .csv with the date and safe the data locally 
  dataname <- paste0("Data_JHUCSSEGithubConfirmed_",Sys.Date())
  dataname <- gsub("-","", dataname)
  # Save the daily update of the data
  write.csv2(jhu, paste0("jhu/confirmed/",dataname,".csv"))
  
jhu <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  # Some colnames look nasty so we correct this
  colnames(jhu) <- gsub("X", "", colnames(jhu))
  # In order to save the daily data the following lines 
  # automatically create a .csv with the date and safe the data locally 
  dataname <- paste0("Data_JHUCSSEGithubDeath_",Sys.Date())
  dataname <- gsub("-","", dataname)
  # Save the daily update of the data
  write.csv2(jhu, paste0("jhu/death/",dataname,".csv"))
  
jhu <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
  # Some colnames look nasty so we correct this
  colnames(jhu) <- gsub("X", "", colnames(jhu))
  # In order to save the daily data the following lines 
  # automatically create a .csv with the date and safe the data locally 
  dataname <- paste0("Data_JHUCSSEGithubRecovered_",Sys.Date())
  dataname <- gsub("-","", dataname)
  # Save the daily update of the data
  write.csv2(jhu, paste0("jhu/recovered/",dataname,".csv"))

# Get RKI data #
################
# Source: https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/Coronaf%C3%A4lle_in_den_Bundesl%C3%A4ndern/FeatureServer
# Terms of use apply according to: rki - https://www.rki.de/DE/Home/homepage_node.html
# Terms of use apply according to: bkg - https://www.bkg.bund.de/DE/Home/home.html

# Data on Level of the "Landkreise / Kreisfreie Städte"  
rki <- read.csv("https://opendata.arcgis.com/datasets/917fc37a709542548cc3be077a786c17_0.csv")
  # In order to save the daily data the following lines 
  # automatically create a .csv with the date an safe the data locally 
  dataname <- paste0("Data_RKIKreise_",Sys.Date())
  dataname <- gsub("-","", dataname)
  # Save the daily update of the data
  write.csv2(rki, paste0("rki/kreise/",dataname,".csv")) 

# Data on Level of the "Länder"    
rki <- read.csv("https://opendata.arcgis.com/datasets/ef4b445a53c1406892257fe63129a8ea_0.csv")
# In order to save the daily data the following lines 
# automatically create a .csv with the date and safe the data locally 
  dataname <- paste0("Data_RKILaender_",Sys.Date())
  dataname <- gsub("-","", dataname)
  # Save the daily update of the data
  write.csv2(rki, paste0("rki/laender/",dataname,".csv")) 
  
# Full dataset of the RKI (time-series)  
# https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0  
# rki <- read.csv("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv")
  # In order to save the daily data the following lines 
  # automatically create a .csv with the date an safe this
  # data locally 
  dataname <- paste0("Data_RKIKreiseTrend_",Sys.Date())
  dataname <- gsub("-","", dataname)
  # Save the daily update of the data
  download.file("https://www.arcgis.com/sharing/rest/content/items/f10774f1c63e40168479a1feb6c7ca74/data", destfile = paste0("rki/kreise/trend/",dataname,".csv"))
#    write.csv2(rki, paste0("rki/kreise/trend/",dataname,".csv")) 
  
# RKI R0 Data from Nowcast  
# https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Projekte_RKI/Nowcasting.html  
url <- paste0("https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Projekte_RKI/Nowcasting_Zahlen.xlsx?__blob=publicationFile")  
r0rki <- rio::import(url, which = c(2))
# Rename Variable
names(r0rki) <- c("Datum", "NeuErkr", "lb_NeuErkr", "ub_NeuErkr", "NeuErkr_ma4", "lb_NeuErkr_ma4", "ub_NeuErkr_ma4", "R" , "lb_R", "ub_R", "R_7Tage", "lb_R_7Tage", "ub_R_7Tage")
  
# In order to save the daily data the following lines 
# automatically create a .csv with the date (-4 days, as
# this is there delay in reporting) and safe this data locally 
dataname <- paste0("Data_R0RKI_",(Sys.Date()-4))
dataname <- gsub("-","", dataname)
# Save the daily update of the data
  write.csv2(r0rki, paste0("rki/rnull/",dataname,".csv")) 

# Get ECDC local data #
#######################  
# Source: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide 
# The downloadable data file is updated daily and contains the 
# latest available public data on COVID-19. Public-use data files 
# allows users to manipulate the data in a format appropriate 
# for their analyses. Users of ECDC public-use data files must 
# comply with data use restrictions to ensure that the information 
# will be used solely for statistical analysis or reporting purposes.
  # Challenge: They change the URL daily. 
  # Hence we add the system date earlier as they do,
  # as part of the URL (-1 day, as this is hwo they report)
  url <- paste0("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",(Sys.Date()-1),".xlsx")  
  ecdc <- rio::import(url)
  # In order to save the daily data the following lines 
  # automatically create a .csv with the date (-1 day, as
  # this is there reporting) and safe this data locally 
  dataname <- paste0("Data_ECDC_",(Sys.Date()-1))
  dataname <- gsub("-","", dataname)
  # Save the daily update of the data
  write.csv2(ecdc, paste0("ecdc/",dataname,".csv")) 

# Get the data of everyone counts #
###################################
#  everyonecounts <- read.csv("https://geoserver.everyonecounts.de/everyonecounts/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=everyonecounts%3AEveryoneCounts&maxFeatures=50&outputFormat=csv")
  # In order to save the daily data the following lines 
  # automatically create a .csv with the date an safe this
  # data locally 
 # dataname <- paste0("everyonecounts_",Sys.Date())
 # dataname <- gsub("-","", dataname)
  # Save the daily update of the data
 # write.csv2(everyonecounts, paste0("../everyonecounts/",dataname,".csv")) 
  
  
# All code for this project developed by: 
# (c) Benjamin G. Engst & David M. Grundmanns 
  