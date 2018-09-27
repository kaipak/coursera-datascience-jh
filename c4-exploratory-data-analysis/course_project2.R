library(tidyverse)
# Read in datasets
NEI <- readRDS("data/exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("data/exdata_data_NEI_data/Source_Classification_Code.rds")

pm25.1999 <- NEI[NEI$year == 1999,]
pm25.2002 <- NEI[NEI$year == 2002,]
pm25.2005 <- NEI[NEI$year == 2005,]
pm25.2008 <- NEI[NEI$year == 2008,]

# Plot of total emissions from 1999 - 2008
plot.range <- range(sum(pm25.1999$Emissions), sum(pm25.2008$Emissions))
years <- c(1999, 2002, 2005, 2008)
yearly.emissions <- c(sum(pm25.1999$Emissions), sum(pm25.2002$Emissions),
                      sum(pm25.2005$Emissions), sum(pm25.2008$Emissions))

plot(years, yearly.emissions, xlim = c(1996, 2010), 
     ylim = plot.range, xlab = "Year", ylab = "Emissions")