# Clear matrix
rm(list = ls())

setwd('../../data/')

colnames(clinic)

clinic <- read.csv2("geolocations/Data_hospitals_20200323.csv")
  data <- subset(clinic, select = c("X", "Y", "name", "healthcare"
                                    , "healthcare_speciality", "emergency"
                                    , "rooms", "beds", "capacity", "wheelchair"))
  # Just a check that nothing is double 
  data <- unique(data)
  
data$position <- paste0(data$X,",",data$Y,",",data$X,",",data$Y)  
data$url <- paste0("https://sgx.geodatenzentrum.de/wfs_vg250-ew?service=wfs&version=2.0.0&request=GetFeature&typeNames=vg250:vg250_krs&bbox=",data$position)
data$nuts <- NA  

for (i in 1:nrow(data)) {
  codings <- readLines(data$url[i])
  codings <- gsub(".*FK_S3><vg250-ew:NUTS>", "", codings)  
  data$nuts[i] <- gsub("</vg250-ew:NUTS>.*", "", codings) 
  l <- sample(1.4:3.3, 1)  
  Sys.sleep(l)   
  print(i)
}

data$position <- NULL
data$url <- NULL

write.csv2(data, "analyses/DataNUTS_hospitals.csv")

