#Data Processing
#------------------------------------

#Creating storms.csv

#Importing Data
Basin.NA.ibtracs_hurdat.v03r06 <- read.delim("~/Final Project/Raw Data/Basin.NA.ibtracs_hurdat.v03r06.hdat", header=FALSE, stringsAsFactors=FALSE)
x <- Basin.NA.ibtracs_hurdat.v03r06

#Extracting ID of Storm
t <- grepl('SNBR',x[1:16443,])
t <- which(t==TRUE)
id <- substr(x[t,],1,5)

#Extracting Date of Storm
dates <- substr(x[t,],7,16)
date <- as.Date(dates,format = '%m/%d/%Y')

#Extracting Number of Days
days <- substr(x[t,],20,21)
days <- as.numeric(days)

#Extracting Name of Storm
e <- as.character(x[t,])
name <- substr(e,36,nchar(e)-43)
name <- gsub(" ", "", name, fixed = TRUE)

#Binding above columns
storms <- cbind('ID' = id,'Date' = date, 'Days' = days, 'Name' = name)

#Exporting storms to CSV
write.csv(storms,'~/Final Project/Data/storms.csv')

#------------------------------------------
#Creating tracks.csv

s <- t+1

#Extracting IDs
id_t <- rep(id, each = 4)

#Extracting Dates
date_t <- rep(date, each = 4)

#Extracting Period
period <- rep(c('00h','06h','12h','18h'),1777)

#Extracting Stages
stage <- vector(mode="character", length=1777)
symbol <- 'A'
for(i in s)
  symbol <- c(symbol,substr(x[i,],12,12))
symbol = symbol[-1]
for(i in 1:1777)
{
  if(symbol[i] == '*')
    stage[i] <- 'Cyclone'
  else if(symbol[i] == 'S')
    stage[i] <- 'Subtropical'
  else if(symbol[i] == 'E')
    stage[i] <- 'Extratropical'
  else if(symbol[i] == 'W')
    stage[i] <- 'Wavestage'
}
b <- 4 
a <- sapply(stage, function (x) rep(x,b))
a <- as.vector(a)
stage <- a

#Extracting Latitude
lati <- 'A'
for(i in s)
  lati <- c(lati,substr(x[i,],13,15),substr(x[i,],30,32),substr(x[i,],47,49),substr(x[i,],64,66))
lati = lati[-1]
n <- 3
lati <- paste(substr(lati, 1, n-1), ".", substr(lati, n, nchar(lati)), sep = "")
lati <- as.double(lati)

#Extracting Longitude
long <- 'A'
for(i in s)
  long <- c(long,substr(x[i,],17,19),substr(x[i,],34,36),substr(x[i,],51,53),substr(x[i,],68,70))
long = long[-1]
n <- 3
long <- paste(substr(long, 1, n-1), ".", substr(long, n, nchar(long)), sep = "")
long <- as.double(long)
long <- long - 160

#Extracting Wind Speed
wind <- 'A'
for(i in s)
  wind <- c(wind,substr(x[i,],22,23),substr(x[i,],39,40),substr(x[i,],56,57),substr(x[i,],73,74))
wind = wind[-1]
wind <- as.numeric(wind)

#Extracting Pressure
pres <- 'A'
for(i in s)
  pres <- c(pres,substr(x[i,],25,28),substr(x[i,],42,45),substr(x[i,],59,62),substr(x[i,],76,79))
pres = pres[-1]
pres <- as.numeric(pres)

#Binding above columns
tracks <- cbind('ID' = id_t,'Date' = date_t, 'Period' = period, 'Stage' = stage, 'Latitude' = lati, 'Longitude' = long, 'Wind Speed' = wind, 'Pressure' = pres)

#Exporting tracks to CSV
write.csv(tracks,'~/Final Project/Data/tracks.csv')

#Importing tracks
tracks <- read.csv("~/Final Project/Data/tracks.csv", stringsAsFactors=FALSE)

#Removing unwanted entries
u <- which(tracks$Latitude==0 & tracks$Longitude==-160 & tracks$Wind.Speed ==0 & tracks$Pressure ==0)
tracks <- tracks[-u,]

#Exporting tracks to CSV
write.csv(tracks,'~/Final Project/Data/tracks.csv')
