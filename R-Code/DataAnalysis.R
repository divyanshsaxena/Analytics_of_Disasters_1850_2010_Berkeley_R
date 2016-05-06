#Data Analysis

#-------------------------------------------------

#Importing Storms
storms <- read.csv("~/Final Project/Data/storms.csv", stringsAsFactors=FALSE)

#Importing Tracks
tracks <- read.csv("~/Final Project/Data/tracks.csv", stringsAsFactors=FALSE)
tracks <- tracks[,-1]
dates <- as.Date(tracks[,3],origin = '1970-01-01')
tracks <- cbind(tracks, 'Dates' = dates)

#-------------------------------------------------

#Creating column for years
tracks$Year <- sapply(tracks$Dates,function(x) substr(x,1,4))

#Number of Storms per year
table(tracks$Year)

#Number of Storms per year with winds >= 35 Knots
w35 <- tracks[which(tracks$Wind.Speed>=35),]
table(w35$Year)

#Number of Storms per year with winds >= 64 Knots
w64 <- tracks[which(tracks$Wind.Speed>=64),]
table(w64$Year)

#Number of Storms per year with winds >= 96 Knots
w96 <- tracks[which(tracks$Wind.Speed>=96),]
table(w96$Year)

#Creating column for months
tracks$Month <- sapply(tracks$Dates,function(x) substr(x,6,7))

#Number of Storms per month
table(tracks$Month)

#Number of Storms per month with winds >= 35 Knots
w35_m <- tracks[which(tracks$Wind.Speed>=35),]
table(w35_m$Month)

#Number of Storms per month with winds >= 64 Knots
w64_m <- tracks[which(tracks$Wind.Speed>=64),]
table(w64_m$Month)

#Number of Storms per month with winds >= 96 Knots
w96_m <- tracks[which(tracks$Wind.Speed>=96),]
table(w96_m$Month)

#-----------------------------------------------

#Annual Average number of storms
statistics <- data.frame('','Avg','Std Dev','25th','50th','75th',stringsAsFactors = FALSE)
statistics[2,1] <- '35 Knots'
statistics[3,1] <- '64 Knots'
statistics[4,1] <- '96 Knots'
statistics[2,2] <- round(sum(tracks$Wind.Speed>=35)/sum(unique(tracks$Year)!=0),digit = 2)
statistics[3,2] <- round(sum(tracks$Wind.Speed>=64)/sum(unique(tracks$Year)!=0),digit = 2)
statistics[4,2] <- round(sum(tracks$Wind.Speed>=96)/sum(unique(tracks$Year)!=0),digit = 2)
statistics[2,3] <- round(sd(table(w35$Year),na.rm = FALSE),digit = 2)
statistics[3,3] <- round(sd(table(w64$Year),na.rm = FALSE),digit = 2)
statistics[4,3] <- 0
qw_35 <- quantile(table(w35$Year))
qw_64 <- quantile(table(w64$Year))
qw_96 <- quantile(table(w96$Year))
statistics[2,4] <- qw_35[2]
statistics[2,5] <- qw_35[3]
statistics[2,6] <- qw_35[4]
statistics[3,4] <- qw_64[2]
statistics[3,5] <- qw_64[3]
statistics[3,6] <- qw_64[4]
statistics[4,4] <- 0
statistics[4,5] <- 0
statistics[4,6] <- 0

#Printing statistics
print(statistics)

#----------------------------------

#Regression Analysis

#Regression Analysis 1 - Mean Pressure and Mean Wind Speed for each storm
uni <- unique(tracks$ID)
mean_wind <- 0
mean_pressure <- 0
for(i in uni)
{
  a <- which(tracks$ID == i)
  mean_wind <- c(mean_wind,mean(tracks$Wind.Speed[a]))
  mean_pressure <- c(mean_pressure,mean(tracks$Pressure[a]))
}

t1 <- which(mean_pressure == 0)
mean_wind <- mean_wind[-t1]
mean_pressure <- mean_pressure[-t1]

#Regression plot
plot(mean_pressure,mean_wind,ylab = 'Wind Speed',xlab = 'Pressure')
regline <- lm(mean_wind~mean_pressure)   
abline(regline, lwd=2, col=2)

#Regression Analysis 2 - Median Pressure and Median Wind Speed for each storm
uni <- unique(tracks$ID)
median_wind <- 0
median_pressure <- 0
for(i in uni)
{
  a <- which(tracks$ID == i)
  median_wind <- c(median_wind,median(tracks$Wind.Speed[a]))
  median_pressure <- c(median_pressure,median(tracks$Pressure[a]))
}

t1 <- which(median_pressure == 0)
median_wind <- median_wind[-t1]
median_pressure <- median_pressure[-t1]

#Regression plot
plot(median_pressure,median_wind,ylab = 'Wind Speed',xlab = 'Pressure')
regline1 <- lm(median_wind~median_pressure)   
abline(regline1, lwd=2, col=2)
