#Data Visualization

#Importing Packages
library(maps)
library(data.table)
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)

#East Pacific

#Importing Data and preliminary cleaning
Basin.EP.ibtracs_wmo.v03r06 <- read.csv("~/Final Project/Raw Data/Basin.EP.ibtracs_wmo.v03r06.csv", header=FALSE, dec=",", stringsAsFactors=FALSE)
east <- Basin.EP.ibtracs_wmo.v03r06
east <- east[-1,]
names(east) <- east[1,]
east <- east[-1,]
east <- east[-1,]
east[,9] <- as.numeric(east[,9])
east[,10] <- as.numeric(east[,10])

#North Atlantic
#Importing Data and preliminary cleaning
Basin.NA.ibtracs_wmo.v03r06 <- read.csv("~/Final Project/Raw Data/Basin.NA.ibtracs_wmo.v03r06.csv", header=FALSE, dec=",", stringsAsFactors=FALSE)
north <- Basin.NA.ibtracs_wmo.v03r06
north <- north[-1,]
names(north) <- north[1,]
north <- north[-1,]
north <- north[-1,]
north[,9] <- as.numeric(north[,9])
north[,10] <- as.numeric(north[,10])


#------------------------------------------------
#Plot from 1980-2010

#Extracting Year
east$Year <- as.numeric(substr(east[,7],1,4))
north$Year <- as.numeric(substr(north[,7],1,4))

#Setting up the base map
base <- ggplot()
base <- base + geom_polygon(data=map_data("world"),
                            aes(x=long, y=lat, group=group),
                            fill="gray25", colour="gray25", size=0.2)
base <- base + xlim(-138, -20) + ylim(3, 55)
base <- base + coord_map()
base <- base + labs(x=NULL, y=NULL, title=NULL, colour = "Wind (knots)")
base <- base + theme_bw()
base <- base + theme(text=element_text(family="Arial", face="plain", size=rel(5)),
                     panel.background = element_rect(fill = "gray10", colour = "gray30"),
                     panel.margin = unit(c(0,0), "lines"),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     plot.margin = unit(c(0,0,0,0), "lines"),
                     axis.text.x = element_blank(),
                     axis.text.y = element_blank(),
                     axis.ticks = element_blank(),
                     legend.position = c(0.25, 0.1),
                     legend.background = element_rect(fill="gray10", color="gray10"),
                     legend.text = element_text(color="white", size=rel(2)),
                     legend.title = element_text(color="white", size=rel(5)),
                     legend.direction = "horizontal")
a <- which(east$Year>=1980 & east$Year<=2010)
b <- which(north$Year>=1980 & north$Year<=2010)

#Creating the plot
gg <- base
gg <- gg + geom_path(data=east[a,],aes(x=east[a,]$Longitude, y=east[a,]$Latitude),size=0.5, alpha=1/4, color='#0099CC')
gg <- gg + geom_path(data=north[b,],aes(x=north[b,]$Longitude, y=north[b,]$Latitude),size=0.5, alpha=1/4, color='#0099CC')
gg <- gg + geom_text(label='Hurricane Trajectories 1980-2010', aes(x=-130, y=54), size=rel(9), color="white", vjust=1)

#Exporting to png file
png(filename=sprintf("~/Final Project/Images/1980-2010.png"),width=1920, height=1080, type="cairo", bg="gray25")
print(gg)
dev.off()

#---------------------------------------
#Plot Month-wise

#Extracting Month
east$Month <- as.numeric(substr(east[,7],6,7))
new <- east[a,]
north$Month <- as.numeric(substr(north[,7],6,7))
new1 <- north[a,]

#Plotting Month-wise 
for(i in 5:12)
{
  mn <- new[which(new$Month==i & new$Basin==' EP' & new$Longitude<=-20 & new$Longitude>=-138 & new$Latitude>=3 & new$Latitude<=55),]
  mn1 <- new1[which(new1$Month==i & new1$Basin==' NA' & new1$Longitude<=28 & new1$Longitude>=-138 & new1$Latitude>=3 & new1$Latitude<=72.5),]
  gg <- base
  gg <- gg + geom_path(data=mn,aes(x=mn$Longitude, y=mn$Latitude),size=0.5, alpha=1/4, color='#0099CC')
  gg <- gg + geom_path(data=mn1,aes(x=mn1$Longitude, y=mn1$Latitude),size=0.5, alpha=1/4, color='#0099CC')
  gg <- gg + geom_text(label=sprintf('Hurricane Trajectories Month No - %s',i), aes(x=-130, y=54), size=rel(9), color="white", vjust=1)
  
  #Exporting to png file
  png(filename=sprintf("~/Final Project/Images/MonthNumber%s.png",i),width=1920, height=1080, type="cairo", bg="gray25")
  print(gg)
  dev.off()
}

#--------------------------------

#Plotting Decade Wise
d1980 <- east[which(east$Year>=1980 & east$Year<=1989),]
d1990 <- east[which(east$Year>=1990 & east$Year<=1999),]
d2000 <- east[which(east$Year>=2000 & east$Year<=2009),]
d1980_1 <- north[which(north$Year>=1980 & north$Year<=1989),]
d1990_1 <- north[which(north$Year>=1990 & north$Year<=1999),]
d2000_1 <- north[which(north$Year>=2000 & north$Year<=2009),]

new <- d1980
new1 <- d1980_1
mn <- new[which(new$Basin==' EP' & new$Longitude<=-20 & new$Longitude>=-138 & new$Latitude>=3 & new$Latitude<=55),]
mn1 <- new1[which(new1$Basin==' NA' & new1$Longitude<=28 & new1$Longitude>=-138 & new1$Latitude>=3 & new1$Latitude<=72.5),]
gg <- base
gg <- gg + geom_path(data=mn,aes(x=mn$Longitude, y=mn$Latitude),size=0.5, alpha=1/4, color='#0099CC')
gg <- gg + geom_path(data=mn1,aes(x=mn1$Longitude, y=mn1$Latitude),size=0.5, alpha=1/4, color='#0099CC')
gg <- gg + geom_text(label='Hurricane Trajectories 1980s', aes(x=-130, y=54), size=rel(9), color="white", vjust=1)

#Exporting to png file
png(filename="~/Final Project/Images/Decade1980.png",width=1920, height=1080, type="cairo", bg="gray25")
print(gg)
dev.off()

new <- d1990
new1 <- d1990_1
mn <- new[which(new$Basin==' EP' & new$Longitude<=-20 & new$Longitude>=-138 & new$Latitude>=3 & new$Latitude<=55),]
mn1 <- new1[which(new1$Basin==' NA' & new1$Longitude<=28 & new1$Longitude>=-138 & new1$Latitude>=3 & new1$Latitude<=72.5),]
gg <- base
gg <- gg + geom_path(data=mn,aes(x=mn$Longitude, y=mn$Latitude),size=0.5, alpha=1/4, color='#0099CC')
gg <- gg + geom_path(data=mn1,aes(x=mn1$Longitude, y=mn1$Latitude),size=0.5, alpha=1/4, color='#0099CC')
gg <- gg + geom_text(label='Hurricane Trajectories 1990s', aes(x=-130, y=54), size=rel(9), color="white", vjust=1)

#Exporting to png file
png(filename="~/Final Project/Images/Decade1990.png",width=1920, height=1080, type="cairo", bg="gray25")
print(gg)
dev.off()

new <- d2000
new1 <- d2000_1
mn <- new[which(new$Basin==' EP' & new$Longitude<=-20 & new$Longitude>=-138 & new$Latitude>=3 & new$Latitude<=55),]
mn1 <- new1[which(new1$Basin==' NA' & new1$Longitude<=28 & new1$Longitude>=-138 & new1$Latitude>=3 & new1$Latitude<=72.5),]
gg <- base
gg <- gg + geom_path(data=mn,aes(x=mn$Longitude, y=mn$Latitude),size=0.5, alpha=1/4, color='#0099CC')
gg <- gg + geom_path(data=mn1,aes(x=mn1$Longitude, y=mn1$Latitude),size=0.5, alpha=1/4, color='#0099CC')
gg <- gg + geom_text(label='Hurricane Trajectories 2000s', aes(x=-130, y=54), size=rel(9), color="white", vjust=1)

#Exporting to png file
png(filename="~/Final Project/Images/Decade2000.png",width=1920, height=1080, type="cairo", bg="gray25")
print(gg)
dev.off()

#----------------------------------------------
