#Skeleton of the project

#Creating Code Directory
dir.create('Code')

#Creating Data Directory
dir.create('Data')

#Creating Images Directory
dir.create('Images')

#Creating Raw Data Directory
dir.create('Raw Data')

#Creating Report Directory
dir.create('Report')

#Downloading Hurricane Data
download.file('ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/hurdat_format/basin/Basin.NA.ibtracs_hurdat.v03r06.hdat', '~/Final Project/Raw Data/Basin.NA.ibtracs_hurdat.v03r06.hdat',  quiet = FALSE, mode = "w",cacheOK = TRUE)
