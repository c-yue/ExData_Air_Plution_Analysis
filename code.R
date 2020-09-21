#library import

install.packages("openxlsx")
library("openxlsx")
library(dplyr)
library(sqldf)
library(ggplot2)

#read
nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

nei$SCC <- as.factor(nei$SCC)
nei$year <- as.factor(nei$year)


#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
year <- nei$year
pm25 <- nei$Emissions

png(filename="plot1.png",width=480, height=480)
boxplot(log10(pm25) ~ year, ylim = c(-10, 5))
dev.off()

#Decreased



#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland from 1999 to 2008?
bc <- nei[nei$fips == "24510", ]
year_bc <- bc$year
pm25_bc <- bc$Emissions

png(filename="plot2.png",width=480, height=480)
boxplot(log10(pm25_bc) ~ year_bc)
dev.off()

#Decreased



#Of the four types of sources indicated by the \color{red}{\verb|type|}type (point, nonpoint, onroad, nonroad) variable
#which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
bc <- nei[nei$fips == "24510", ]

g <- ggplot(bc, aes(x=year, y=log10(Emissions)))
g + geom_boxplot(aes(fill=year)) +
    facet_wrap(~ type)

png(filename="plot3.png",width=480, height=480)
g + geom_boxplot(aes(fill=year)) +
    facet_wrap(~ type)
dev.off()

#Decreased



#how have emissions from coal combustion-related sources changed from 1999–2008?
com <- left_join(nei, scc, by = 'SCC')
com_coal <- com[com$SCC.Level.One %in% 
                        c('External Combustion Boilers',
                         'Internal Combustion Engines',
                         'Stationary Source Fuel Combustion'),]

g <- ggplot(com_coal, aes(x=year, y=log10(Emissions)))
g + geom_boxplot(aes(fill=year))

png(filename="plot4.png",width=480, height=480)
g + geom_boxplot(aes(fill=year))
dev.off()

#Stable



#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
com <- left_join(nei, scc, by = 'SCC')
com_bc_motor <- com[com$SCC.Level.One == 'Mobile Sources' & com$fips == "24510",]

g <- ggplot(com_bc_motor, aes(x=year, y=log10(Emissions)))
g + geom_boxplot(aes(fill=year))

png(filename="plot5.png",width=480, height=480)
g + geom_boxplot(aes(fill=year))
dev.off()

#Decreased from 1999 to 2002, and then stay stable



#Compare emissions from motor vehicle sources in Baltimore City 
#with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?
com <- left_join(nei, scc, by = 'SCC')
bc_los_motor <- com[com$SCC.Level.One == 'Mobile Sources' & com$fips %in% c("24510","06037"),]
bc_los_motor$location <- if_else(bc_los_motor$fips == "24510", "Baltimore City", "Los Angeles County")

g <- ggplot(bc_los_motor, aes(x=year, y=log10(Emissions)))
g + geom_boxplot(aes(fill=year)) +
    facet_wrap(~ location)

png(filename="plot6.png",width=480, height=480)
g + geom_boxplot(aes(fill=year)) +
        facet_wrap(~ location)
dev.off()

#Los Angeles County

