# See notes on libraries at end of documents
library(data.table); library(dplyr); library(tidyr) 

#load data sets if not already in memory
if (!exists("NEI") | !exists("SCC")) source("loadData.R")

# NOTE: I believe it is questionable not to normalize the data by measuring 
# stations. That is, the gross number of measuring stations, or changes in 
# location or type of measuring station could introduce bias. However, such 
# qualitative adjustments are not requested for the purposes of this assignment,
# and introducing such rigor would add confusion to the process of performing
# peer assessments.

# Create a copy of NEI table for this part one of the assignment, create
# factors, and then average data for specific points within a year, turn year
# and obs type into factors, then create columns for grouping
NEI.p1 <- NEI %>% 
    mutate(type = as.factor(type), year = as.factor(year)) %>%
    mutate(obs.id = paste(fips, SCC, type, sep = "-")) %>%
    mutate(obs.id.year = paste(year, obs.id, sep = "-"))

# Average multiple measurements of a single type within one year together
NEI.p1 <- NEI.p1 %>% group_by(obs.id.year)
NEI.p1 <- NEI.p1 %>% summarise(fips, SCC, Pollutant, type, year, obs.id,  
                                     Emissions = mean(Emissions))

# Create input data for plotting -- aggregate data by year, and turn the years
# back into numeric data
p1.data <- NEI.p1 %>%
    group_by(year) %>%
    summarise(Emissions = mean(Emissions)) %>%
    mutate(year = as.numeric(as.character(year)))

png(filename = "plot1.png", width = 600, height = 400)
    
    plot(p1.data, type = "b", xaxp = c(1999, 2008, 3))
    title(main = "Levels of PM_2.5 (1999-2008)")

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
################################################################################