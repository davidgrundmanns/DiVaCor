# All code for this project developed by:
# (c) Benjamin G. Engst & David M. Grundmanns
# Clear matrix #
#--------------#
rm(list = ls())
# Set working-directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

  # Read the data from everyone counts
  everyone <- max(sort(list.files("../../data/everyonecounts")))
  # Load this file
  everyone <- read.csv2(paste0("../../data/everyonecounts/",everyone))
    everyone$X <- NULL # Exclude this count variable indicating lines

# Make x and y cordinate to run through website
everyone$geom <- gsub("POINT \\(","", everyone$geom)
everyone$geom <- gsub("\\)","", everyone$geom)

everyone$X <- gsub(" .*","", everyone$geom)
everyone$Y <- gsub(".* ","", everyone$geom)

everyone <- unique(everyone)
everyone$position <- paste0(everyone$X,",",everyone$Y,",",everyone$X,",",everyone$Y)  
everyone$url <- paste0("https://sgx.geodatenzentrum.de/wfs_vg250-ew?service=wfs&version=2.0.0&request=GetFeature&typeNames=vg250:vg250_krs&bbox=",everyone$position)

everyone$nuts <- NA  

for (i in 1:nrow(everyone)) {
  codings <- readLines(everyone$url[i])
  codings <- codings[which(grepl("NUTS", codings))]  
  codings <- gsub("<vg250-ew:NUTS>", "", codings) 
  codings <- gsub("</vg250-ew:NUTS>", "", codings) 
  everyone$nuts[i] <- gsub(" ", "", codings, fixed = TRUE) 
  l <- sample(1.2:3.1, 1)  
  Sys.sleep(l)   
  print(i)
}

everyone$position <- NULL
everyone$url <- NULL

write.csv2(everyone, "../../data/analyses/everyonecounts_nuts.csv" , row.names=FALSE)
