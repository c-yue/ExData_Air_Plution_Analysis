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

year_list <- as.matrix(unique(year))[,1]
median_pm <- as.matrix(tapply(pm25, year, median))[,1]
plot(year_list, median_pm, type='p')

png(filename="plot1.png",width=480, height=480)
plot(year_list, median_pm, type='p')
dev.off()

#Decreased



#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland from 1999 to 2008?
bc <- nei[nei$fips == "24510", ]
year_bc <- bc$year
pm25_bc <- bc$Emissions

year_list_bc <- as.matrix(unique(year_bc))[,1]
sum_pm_bc <- as.matrix(tapply(pm25_bc, year_bc, sum))[,1]
plot(year_list_bc, sum_pm_bc, type='p')

png(filename="plot2.png",width=480, height=480)
plot(year_list_bc, sum_pm_bc, type='p')
dev.off()

#Decreased in a fructuration



#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable
#which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
bc <- nei[nei$fips == "24510", ]

bc_prs <- bc %>%
        group_by(year, type) %>%
        summarise(sum_pm = sum(Emissions))

g <- ggplot(bc_prs, aes(x=year, y=sum_pm))
g + geom_point(aes(fill=year)) +
    facet_wrap(~ type)

png(filename="plot3.png",width=480, height=480)
g + geom_point(aes(fill=year)) +
        facet_wrap(~ type)
dev.off()

#Decreased Except the type point



#how have emissions from coal combustion-related sources changed from 1999–2008?
coal_scc <- scc[grepl("Coal",scc$Short.Name),]
coal_nei <- nei[nei$SCC %in% coal_scc$SCC,]

coal_prs <- coal_nei %>%
        group_by(year) %>%
        summarise(sum_pm = sum(Emissions))

g <- ggplot(coal_prs, aes(x=year, y=sum_pm))
g + geom_point(aes(fill=year))

png(filename="plot4.png",width=480, height=480)
g + geom_point(aes(fill=year))
dev.off()

#Decreased



#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
bc_v <- nei[nei$type == 'ON-ROAD' & nei$fips == "24510",]

bc_v_prs <- bc_v %>%
        group_by(year) %>%
        summarise(sum_pm = sum(Emissions))

g <- ggplot(bc_v_prs, aes(x=year, y=sum_pm))
g + geom_point(aes(fill=year))

png(filename="plot5.png",width=480, height=480)
g + geom_point(aes(fill=year))
dev.off()

#Decreased



#Compare emissions from motor vehicle sources in Baltimore City 
#with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?
bc_los_motor$location <- if_else(bc_los_motor$fips == "24510", "Baltimore City", "Los Angeles County")

bc_los <- nei[nei$type == 'ON-ROAD' & nei$fips %in% c("24510","06037"),]
bc_los$location <- if_else(bc_los$fips == "24510", "Baltimore City", "Los Angeles County")

bc_los_prs <- bc_los %>%
        group_by(year, location) %>%
        summarise(sum_pm = sum(Emissions))

g <- ggplot(bc_los_prs, aes(x=year, y=sum_pm))
g + geom_point(aes(fill=year)) +
    facet_wrap(~ location)

png(filename="plot6.png",width=480, height=480)
g + geom_point(aes(fill=year)) +
        facet_wrap(~ location)
dev.off()

#Los Angeles County

