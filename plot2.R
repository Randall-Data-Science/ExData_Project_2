# See notes on libraries at end of documents
library(data.table)
library(dplyr)
library(tidyr) 

#load data sets if not already in memory
if (!exists("NEI") | !exists("SCC")) source("loadData.R")

NEI.p2 <- NEI %>% 
    mutate(type = as.factor(type), year = as.factor(year)) %>%
    mutate(obs.id = paste(fips, SCC, type, sep = "-")) %>%
    mutate(obs.id.year = paste(year, obs.id, sep = "-"))

# Find observation types that were measured in all years
good.obs.id <- NEI.p2[year == 1999, obs.id] %>%
    intersect(NEI.p2[year == 2002, obs.id]) %>%
    intersect(NEI.p2[year == 2005, obs.id]) %>%
    intersect(NEI.p2[year == 2008, obs.id])

# only keep observation types measured in all years and in Baltimore
NEI.p2.aY <- NEI.p2 %>% 
    filter(obs.id %in% good.obs.id) %>%
    filter(fips == "24510") #FIPS code for Baltimore 

# Average multiple measurements of a single type within one year together
NEI.p2.aY <- NEI.p2.aY %>% group_by(obs.id.year)
NEI.p2.aY <- NEI.p2.aY %>% summarise(fips, SCC, Pollutant, type, year, obs.id,  
                                     Emissions = mean(Emissions))

# I think summarise is supposed to drop duplicate rows when the 
# .collapse option is set to TRUE, but it doesn't work. Therefore:
NEI.p2.aY = NEI.p2.aY[!duplicated(NEI.p2.aY)]

#create tiny table for graph
p2.data <- NEI.p2.aY %>%
    group_by(year) %>%
    summarise(Emissions = mean(Emissions)) %>%
    mutate(year = as.numeric(as.character(year)))

png(filename = "plot2.png",width = 600, height = 400)

plot(p2.data, type = "b", xaxp = c(1999, 2008, 3))
title(main = "Levels of PM_2.5 1999-2008 \nin BALTIMORE")

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