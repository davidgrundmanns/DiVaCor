# Clear matrix
rm(list = ls())

# Packages necessary # 
#--------------------#
# install.packages("tidyverse")
library(tidyverse)
# install.packages("readxl")
library(readxl)
# install.packages("httr")
library(httr)

setwd('../../data/additional')

# Get hospital beds #
#-------------------#  
# Source: https://www.landatlas.de/
url <- "https://www.landatlas.de/download/E/Krankenhausbetten.xlsx"
GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
beds2 <- read_excel(tf, sheet = 2)
beds1 <- read_excel(tf, sheet = 1)
# In order to save the daily data the following lines 
# automatically create a .csv with the date an safe this
# data locally 
dataname <- paste0("Data_hospitalbeds_",Sys.Date())
dataname <- gsub("-","", dataname)
# Save the daily update of the data
write.csv2(beds1, paste0(dataname,".csv")) 
dataname <- paste0("Codebook_hospitalbeds_",Sys.Date())
write.csv2(beds2, paste0(dataname,".csv")) 