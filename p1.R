# See notes on libraries at end of documents
library(data.table)
library(dplyr)
library(tidyr) 

#load data sets if not already in memory
if (!exists("NEI") | !exists("SCC")) source("loadData.R")

# Create a copy of NEI table for this part one of the assignment, create factors
# filter out data, not present for all years, and average data for specific 
# points within a year

# turn year and obs type into factors
# create columns for grouping
NEI.p1 <- NEI %>% 
    mutate(type = as.factor(type), year = as.factor(year)) %>%
    mutate(obs.id = paste(fips, SCC, type, sep = "-")) %>%
    mutate(obs.id.year = paste(year, obs.id, sep = "-"))

# smaller data set for testing
NEI.p1 <- NEI.p1[sample(dim(NEI.p1)[1], size = 1e6),]

# Find observation types that were measured in all years
good.obs.id <- NEI.p1[year == 1999, obs.id] %>%
    intersect(NEI.p1[year == 2002, obs.id]) %>%
    intersect(NEI.p1[year == 2005, obs.id]) %>%
    intersect(NEI.p1[year == 2008, obs.id])

# only keep observation types measured in all years
NEI.p1.aY <- NEI.p1 %>% filter(obs.id %in% good.obs.id)

# Average multiple measurements of a single type within one year together
NEI.p1.aY <- NEI.p1.aY %>% group_by(obs.id.year)
NEI.p1.aY <- NEI.p1.aY %>% summarise(fips, SCC, Pollutant, type, year, obs.id,  
                               Emissions = mean(Emissions))

# I think summarise is supposed to drop duplicate rows when the 
# .collapse option is set to TRUE, but it doesn't work. Therefore:
NEI.p1.aY = NEI.p1.aY[!duplicated(NEI.p1.aY)]

# Now for the purpose of understanding pairwise changes for specific
# measurements across time periods
NEI.p1.wide <- NEI.p1.aY %>% 
    select(obs.id, year, Emissions) %>% 
    spread(key = year, value = Emissions)
setnames(x = NEI.p1.wide, c("obs.id", "y1999", "y2002", "y2005", "y2008"))

# Then add the changes between years and for the entire period
NEI.p1.wide <- NEI.p1.wide %>% mutate(chg.1999.2002 = y2002 - y1999,
                                      chg.2002.2005 = y2005 - y2002,
                                      chg.2005.2008 = y2008 - y2005,
                                      chg.1999.2008 = y2008 - y1999)

# png(filename = "p1.png",width = 600, height = 400)
# 
# #Have total emissions from PM2.5 decreased in the United States from 1999 to
# #2008? Using the base plotting system, make a plot showing the total PM2.5
# #emission from all sources for each of the years 1999, 2002, 2005, and 2008.
# 
# par(mfrow = c(2,2))
# 
# 
# boxplot(data = NEI.p1.wide, x = NEI.p1.wide$chg.1999.2002)
# title(main = sprintf("Mean change in particulate matter per station \n1999-2002 period: %.2f", 
#               mean(NEI.p1.wide$chg.1999.2002)))
# 
# boxplot(data = NEI.p1.wide, x = NEI.p1.wide$chg.2002.2005)
# title(main = sprintf("Mean change in particulate matter per station \n2002-2005 period: %.2f", 
#               mean(NEI.p1.wide$chg.2002.2005)))
# 
# boxplot(data = NEI.p1.wide, x = NEI.p1.wide$chg.2005.2008)
# title(main = sprintf("Mean change in particulate matter per station \n2005-2008 period: %.2f", 
#               mean(NEI.p1.wide$chg.2005.2008)))
# 
# boxplot(data = NEI.p1.wide, x = NEI.p1.wide$chg.1999.2008)
# title(main = sprintf("Mean change per station \nEntire 1999-2008 period: %.2f", 
#                      mean(NEI.p1.wide$chg.1999.2008)))
# 
# dev.off()

#More in line with the question

p1.data <- NEI.p1.aY %>%
    group_by(year) %>%
    summarise(Emissions = mean(Emissions)) %>%
    mutate(year = as.integer(as.character(year)))

png(filename = "plot1.png",width = 600, height = 400)

plot(p1.data, type = "b")
title(main = "Levels of PM_2.5 1999-2008")

dev.off()





################################################################################
# Hadley Wickham has developed a number of important packages for R includding 
# ggplot2 as well as plyr and reshape for data manipulation. Two of the latest 
# packages for data manipulation are dplyr and tidyr. 
#
# Dplyr provides functionalities analagous to SQL statements like SELECT, JOIN,
# WHERE, ORDER BY. 
#
# Tidyr transforms data analogous to database theory distinctions between
# different normal forms -- melt() makes tables long, spread() makes them wide.
#
# It is also worth noting that the dplyr package imports the magrittr package
# which intorduces the %>% operator for piping data and functions.
#
################################################################################