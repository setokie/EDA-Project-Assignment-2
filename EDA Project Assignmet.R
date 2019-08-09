library(tidyverse)
library(ggplot2)
library(scales)


# setting up data source url and working directory
# to store the downloaded datasets
url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
folder<-paste0(getwd(),"/", "NEI_data.zip")
download.file(url, folder, method = "libcurl")
unzip("NEI_data.zip", exdir = "NEI_data")
setwd(paste0(getwd(), "/", "NEI_data"))


# load data set into R
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


glimpse(NEI)
glimpse(SCC)

# plot 1
# Subsets the data set required for creating plot
NEI.grouped <- NEI %>% 
          group_by(year) %>%
          summarise(Emissions = sum(Emissions))

marks <- c(4000000, 5000000, 6000000, 7000000)
marks <- c(0, 2000000, 4000000, 6000000, 8000000)

# plotting the data
# line plot
par(mar=c(5, 8, 4, 8) + 0.1, mgp = c(3, .5, 0))
with(NEI.grouped, plot(year, Emissions, type = "b", pch = 22, col = "blue", xaxt = "n", yaxt = "n"))
axis(1, at = NEI.grouped$year)
axis(2, at = marks, labels = c("4 million", "5 million", "6 million", "7 million"), las = 2, cex.axis = 0.6)
axis(4, at = NEI.grouped$Emissions, labels = format(round(NEI.grouped$Emissions), big.mark = ","), col.axis = "blue", las = 2, cex.axis = 0.7, tck = -.01)
title(main = list("Total Emissions in the United States from 1999 to 2008",
               cex = 0.75),
      xlab = "Year", ylab = "Emissions in tons")

# barplot
par(mar=c(5, 8, 4, 8) + 0.1, mgp = c(3.5, 0.5, 0))
marks <- c(0, 2000000, 4000000, 6000000, 8000000)
plot1 <- with(NEI.grouped, barplot(Emissions, names = year, 
                                   xlab = "Years", ylab = "Emissions in tons",
                                   yaxt = "n", ylim = c(0 , 8000000),
                                   main = "Total Emissions in the United States from 1999 to 2008"))
axis(2, at = marks, labels = c("0", "2,000,000", "4,000,000", "6,000,000", "8,000,000"), 
     las = 2, cex.axis = 0.6)
text(x = plot1, y = round(NEI.grouped$Emissions,2), label = format(round(NEI.grouped$Emissions,2), big.mark = ","), 
     pos = 3, cex = 0.6, col = "black", font = 4)


# plot 2
NEI.Baltimore <- NEI %>%
        filter(fips == "24510") %>%
        group_by(year) %>%
        summarise(Emissions = sum(Emissions))

marks2 <- c(2000, 2500, 3000, 3500)

# line plot
par(mar=c(5, 8, 4, 8) + 0.1, mgp = c(3, .5, 0))
with(NEI.Baltimore, plot(year, Emissions, type = "b", pch = 22, col = "blue", xaxt = "n", yaxt = "n"))
axis(1, at = NEI.Baltimore$year, cex.axis = 0.8)
axis(2, at = marks2, las = 2, cex.axis = 0.6)
axis(4, at = NEI.Baltimore$Emissions, labels = format(round(NEI.Baltimore$Emissions), big.mark = ","), col.axis = "blue", las = 2, cex.axis = 0.7, tck = -.01)
title(main = list("Total Emissions in Baltimore from 1999 to 2008",
                  cex = 0.75),
      xlab = "Year", ylab = "Emissions in tons")

# barplot
par(mar=c(5, 8, 4, 8) + 0.1, mgp = c(3.5, 0.5, 0))
plot2 <- with(NEI.Baltimore, barplot(Emissions, names = year, 
                                   xlab = "Years", ylab = "Emissions in tons",
                                   yaxt = "n", ylim = c(0,4000),
                                   main = "Total Emissions in Baltimore from 1999 to 2008"))
axis(2, las = 2, cex.axis = 0.7)
text(x = plot2, y = round(NEI.Baltimore$Emissions,2), label = format(round(NEI.Baltimore$Emissions,2), big.mark = ","), 
     pos = 3, cex = 0.6, col = "black", font = 4)


# plot 3
NEI.Baltimore <- NEI %>%
        filter(fips == "24510") %>%
        group_by(year, type) %>%
        summarise(Emissions = sum(Emissions))
NEI.Baltimore$year <- factor(NEI.Baltimore$year)

plot3 <- ggplot(data = NEI.Baltimore, aes(x = year, y = Emissions, col = type))
plot3 + geom_point() + geom_line() + 
  ggtitle(label = "Total Emissions by type of pollution source in Baltimore from 1999 to 2008") +
  scale_x_discrete(labels = unique(NEI.Baltimore$year)) +
  theme(plot.title = element_text(size = 12, hjust = 0.5))

ggplot(data = NEI.Baltimore, 
       aes(x = year, y = Emissions, fill = type, label = round(Emissions, 2))) +
        geom_bar(stat = "identity") +
        facet_grid( . ~ type) +
        ggtitle(label = "Total Emissions by type of pollution source in Baltimore from 1999 to 2008") +
        xlab("Year") +
        ylab("Emissions in tons") +
        theme(plot.title = element_text(size = 12, hjust = 0.5)) +
        geom_label(aes(fill = type), color = "white", label.size = 0.1)

# plot 4
SCC.filtered <- select(SCC, SCC, EI.Sector)

NEI.bySCC <- NEI %>%
        left_join(select(SCC, SCC, EI.Sector), by = c(SCC = "SCC"))

NEI.bySCC <- NEI.bySCC[grep("Coal", NEI.bySCC$EI.Sector),]

NEI.bySCC <- NEI.bySCC %>%
        group_by(year) %>%
        summarise(Emissions = sum(Emissions))

p2 <- ggplot(data = NEI.bySCC, aes(x = factor(year), y = Emissions))
p2 + geom_point() + geom_line(group = 1) +
  ggtitle(label = "Total Emissions from coal in United States from 1999 to 2008") +
  scale_x_discrete(labels = unique(NEI.Baltimore$year)) +
  theme(plot.title = element_text(size = 12, hjust = 0.5))

ggplot(data = NEI.bySCC, aes(x = factor(year), y = Emissions, fill = year, label = format(round(Emissions, 2), big.mark = ","))) + 
        geom_bar(stat = "identity") +
        ggtitle(label = "Total Emissions from coal in United States from 1999 to 2008") +
        xlab("Year") +
        ylab("Emissions in tons") +
        scale_x_discrete(labels = unique(NEI.bySCC$year)) +
        scale_y_continuous(labels = comma) +
        theme(plot.title = element_text(size = 12, hjust = 0.5)) +
        geom_label(aes(fill = year), color = "white", label.size = 0.1)

# plot 5
NEI.Baltimore <- NEI %>%
        filter(fips == "24510", type == "ON-ROAD") %>%
        group_by(year, type) %>%
        summarise(Emissions = sum(Emissions))

ggplot(data = NEI.Baltimore, aes(x = factor(year), y = Emissions, 
                                 fill = year, 
                                 label = format(round(Emissions, 2), big.mark = ","))) + 
        geom_bar(stat = "identity") +
        ggtitle(label = "Total Emissions from motor vehicle in Baltimore from 1999 to 2008") +
        xlab("Year") +  
        ylab("Emissions in tons") +
        scale_x_discrete(labels = unique(NEI.Baltimore$year)) +
        theme(plot.title = element_text(size = 12, hjust = 0.5)) +
        geom_label(aes(fill = year), color = "white", label.size = 0.1)

# plot 6

NEI.Baltimore <- NEI %>%
        filter(fips == "24510", type == "ON-ROAD") %>%
        group_by(year, fips) %>%
        summarise(Emissions = sum(Emissions))

NEI.LosAngeles <- NEI %>%
        filter(fips == "06037", type == "ON-ROAD") %>%
        group_by(year, fips) %>%
        summarise(Emissions = sum(Emissions))

NEI.combined <- rbind(NEI.Baltimore, NEI.LosAngeles)
NEI.combined$fips <- sapply(NEI.combined$fips, 
                            function(x) ifelse(x == "24510", "Baltimore", "Los Angeles"))

ggplot(data = NEI.combined, aes(x = factor(year), y = Emissions, fill = fips, label = round(Emissions, 2))) +
        geom_bar(stat = "identity") +
        facet_grid(fips ~ .) +
        ggtitle(label = "Total Vehicle's Emission in Baltimore and Los Angeles from 1999 to 2008") +
        xlab("Year") +
        ylab("Emissions in tons") +
        theme(plot.title = element_text(size = 12, hjust = 0.5)) +
        geom_label(aes(fill = fips), color = "white", label.size = 0.1) +
        scale_x_discrete(labels = unique(NEI.combined$year))

