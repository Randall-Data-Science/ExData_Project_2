# See notes on libraries at end of documents
library(data.table)
library(dplyr)
library(tidyr) 

#load data sets if not already in memory
if (!exists("NEI") | !exists("SCC")) source("loadData.R")

# Develop a list of SCCs.
scc.good <- SCC %>%
    filter(grepl(pattern = "Highway Vehicles", 
                 perl = TRUE, ignore.case = TRUE, x = SCC.Level.Two)) %>%
    select(SCC)

# convert single column data.table/data.frame factors into array of character strings
scc.good <- as.character(scc.good$SCC) 

NEI.p6 <- NEI %>% 
    filter(SCC == scc.good) %>%
    filter(fips %in% c("24510", "06037")) %>%
    mutate(City = as.factor(ifelse (fips == "24510", "Baltimore", "Los Angeles"))) %>%
    mutate(type = as.factor(type), year = as.factor(year)) %>%
    mutate(obs.id = paste(fips, SCC, type, sep = "-")) %>%
    mutate(obs.id.year = paste(year, obs.id, sep = "-"))


# Reduce data into mean and median values for each year

NEI.p6.plot <- NEI.p6 %>%
    group_by(year, City) %>%
    summarise(Year = as.numeric(as.character(year)), 
              City = City, 
              Mean.Emissions = mean(Emissions), 
              Median.Emissions = median(Emissions))



png(filename = "plot6.png",width = 800, height = 600)

qplot(x = Year, y = Mean.Emissions, data = NEI.p6.plot) + geom_line(aes(color = City)) +
    ggtitle("Substantial Motor Vehicle-Related Pollution in Los Angeles")

dev.off()

