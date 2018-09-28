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

png(filename = "emissions_by_year.png")
plot(years, yearly.emissions, type="o", col="blue", 
     xlim = c(1996, 2010), ylim = plot.range, xlab = "Year", 
     ylab = "Emissions")
dev.off()

# Plot emissions in Baltimore area over time
baltimore.summary <- filter(NEI, fips == '24510') %>% 
                     select(Emissions, year) %>%
                     group_by(year) %>%
                     summarize(total.emissions = sum(Emissions))
png(filename = "baltimore_emissions_by_year.png")
with(data = baltimore.summary, plot(year, total.emissions, type="o", 
                                    col = "orange", xlab = "Year", 
                                    ylab = "Total Emissions",
                                    main = "Total Emissions, Baltimore"))
dev.off()

# ggplot for Baltimore (point, nonpoint, onroad, nonroad)
baltimore.summary2 <- filter(NEI, fips == '24510') %>%
                      select(Emissions, type, year) %>%
                      group_by(year, type) %>%
                      summarize(total.emissions = sum(Emissions))

png(filename = "baltimore_emissions_types_over_time.png")
ggplot(baltimore.summary2) + geom_line(mapping = aes(x = year, 
                                       y = total.emissions, 
                                       color = type)) + theme_linedraw()
dev.off()

# US Coal combustion emissions summary
coal.SCC <- SCC[grep(pattern = "coal", ignore.case = TRUE, 
                     x = SCC$EI.Sector),]$SCC
pm25.coal <- NEI[NEI$SCC %in% coal.SCC,]
pm25.coal.summary <- select(pm25.coal, Emissions, year) %>%
                     group_by(year) %>%
                     summarize(total.emissions = sum(Emissions))

# Plot
png(filename = "us_coal_emissions_over_time.png")
with(data = pm25.coal.summary, plot(year, total.emissions, type="o",
                                    col = "blue", xlab = "Year",
                                    ylab = "Total Coal Emissions",
                                    main = "US Coal Related Emissions"))
dev.off()

# Emissions from motor sources in Baltimore
vehicles.SCC <- SCC[grep(pattern = "vehicles", ignore.case = TRUE,
                         x = SCC$SCC.Level.Two),]$SCC
pm25.vehicles.baltimore <- NEI[NEI$SCC %in% vehicles.SCC,] %>%
                            filter(fips == '24510') %>%
                            select(Emissions, year) %>% 
                            group_by(year) %>%
                            summarize(total.emissions = sum(Emissions))

#plot
png(filename = "baltimore_vehicle_emissions.png")
with(data = pm25.vehicles.baltimore, plot(year, total.emissions, type="o",
                                    col = "orange", xlab = "Year",
                                    ylab = "Total Vehicle Emissions",
                                    main = "Baltimore Vehicle related Emissions"))
dev.off()

# Compare Baltimore vehicle emissions to Los Angeles
pm25.vehicles.LA <- NEI[NEI$SCC %in% vehicles.SCC,] %>%
                    filter(fips == '06037') %>%
                    select(Emissions, year) %>% 
                    group_by(year) %>%
                    summarize(total.emissions = sum(Emissions))

png(filename = "baltimore_LA_vehicle_emissions.png")
ggplot() + geom_line(data=pm25.vehicles.baltimore, 
                     aes(x=year, y=total.emissions, color = "Baltimore")) + 
           geom_line(data=pm25.vehicles.LA, 
                     aes(x=year, y=total.emissions, color = "LA")) +
           labs(title = 'Vehicle Emissions: Baltimore vs. Los Angeles (1999 - 2008)') +
           scale_color_manual(name = "City", values = c(Baltimore = "orange", LA = "blue"))
dev.off()
