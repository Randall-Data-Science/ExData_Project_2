# See notes on libraries at end of documents
library(data.table)
library(dplyr)
library(tidyr) 
library(ggplot2)

#load data sets if not already in memory
if (!exists("NEI") | !exists("SCC")) source("loadData.R")

NEI.p3 <- NEI[fips == "24510",] %>% 
    mutate(type = as.factor(type), year = as.factor(year)) %>%
    mutate(obs.id = paste(fips, SCC, type, sep = "-")) %>%
    mutate(obs.id.year = paste(year, obs.id, sep = "-"))

# Find observation types that were measured in all years
good.obs.id <- NEI.p3[year == 1999, obs.id] %>%
    intersect(NEI.p3[year == 2002, obs.id]) %>%
    intersect(NEI.p3[year == 2005, obs.id]) %>%
    intersect(NEI.p3[year == 2008, obs.id])

# only keep observation types measured in all years and in Baltimore
NEI.p3.aY <- NEI.p3 %>% 
    filter(obs.id %in% good.obs.id)
    
# Average multiple measurements of a single type within one year together
NEI.p3.aY <- NEI.p3.aY %>% group_by(obs.id.year)
NEI.p3.aY <- NEI.p3.aY %>% summarise(fips, SCC, Pollutant, type, year, obs.id,  
                                     Emissions = mean(Emissions))

# I think summarise is supposed to drop duplicate rows when the 
# .collapse option is set to TRUE, but it doesn't work. Therefore:
NEI.p3.aY = NEI.p3.aY[!duplicated(NEI.p3.aY)]

#create tiny table for graph
p3.data <- NEI.p3.aY %>%
    select(type, year, Emissions) %>%
    group_by(year, type) %>%
    summarise(Emissions=mean(Emissions))


# 
# p3.data2 <- spread(data = p3.data,key = type,value = Emissions)
# setnames(x = p3.data2, c("year", "non.road", "non.point", "point"))
# 
# ggplot(data = p3.data2, aes(y= non.road, x=year)) + 
#     geom_point(aes(color = non.road)) + geom_point(aes(y=non.point)) +
#     geom_point(aes(y = point))
# 

png(filename = "plot3.png",width = 600, height = 400)

ggplot(p3.data, aes(x = year, y = Emissions)) + geom_point(aes(color=type))
     
dev.off()
