# See notes on libraries at end of documents
library(data.table)
library(dplyr)
library(tidyr) 
library(ggplot2)
# library(grid) # for unit() function

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

png(filename = "plot3.png",width = 800, height = 600)

# Customization of plotting for the sake of experimentation

ggplot(p3.data, aes(x = year, y = Emissions)) + 
    labs(title = "PM_2.5 Emissions by Type of Measurement Station", x = "Year") +
    geom_line(mapping = aes(group = type, color = type), size = 1.0) + 
    geom_point(color = hsv(s = 0, v = 0.9), size = 7.5) +
    geom_point(mapping = aes(color = type), size = 5, shape = 16) +
    geom_point(color = hsv(s = 0, v = 0.9), size = 3) +
    xlim("", "1999", "2002", "2005","2008") +
    ylim(-10, 130) +
    # data point labels
    geom_text(aes(label = sprintf("%.2f", Emissions), color = type),
              hjust = -0.5, vjust = -1, size = 3.5) + 
    # measurement type
    geom_text(data = p3.data[year == "1999"], 
              aes(label = type, color = type), 
              hjust = 1.15, vjust = -1,  size = 5, fontface = "bold") +
    # percentage change over time period annotation
    geom_text(data = p3.data[year == "1999"], 
              aes(label = sprintf(
                  "%.1f%%\ndecrease", ( 
                      100 - 
                          100 * p3.data[year=="2008" & type==type]$Emissions /
                          p3.data[year=="1999" & type==type]$Emissions
                  )), 
                  color = type), 
              hjust = 1.25, vjust = 1,  size = 4.5) +
    scale_colour_discrete(l = 50, h.start = 0) +
    theme(legend.position = "none")

dev.off()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                          Notes on positioning (in theory)
#  --------------------------------------------------------------------------
#
#   hjust:  0-left  0.5-center  1-right
#   vjust:  0-bottom  0.5-middle  1-top
#
#   move:   position = position_dodge(width= ,height= )
#
#  --------------------------------------------------------------------------
#
#   However, since the positioning does not seem to work, and hjust/vjust, 
#    allow values outside of [0,1] simply guessing seems to work
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
