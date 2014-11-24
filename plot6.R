# See notes on libraries at end of documents
library(data.table)
library(dplyr)
library(tidyr) 

#load data sets if not already in memory
if (!exists("NEI") | !exists("SCC")) source("loadData.R")

NEI.p6 <- NEI %>% 
    mutate(type = as.factor(type), year = as.factor(year)) %>%
    mutate(obs.id = paste(fips, SCC, type, sep = "-")) %>%
    mutate(obs.id.year = paste(year, obs.id, sep = "-"))

# This plot:
#
#   fips = "24510" - Baltimore
#
#   SCC = motor vehicle related

# Develop a list of SCCs.

scc.good <- SCC %>%
    # Matching terms
    filter(grepl(pattern = "Internal Combustion Engines", 
                 perl = TRUE, ignore.case = TRUE, x = SCC.Level.One)) %>%
    select(SCC)


# convert single column data.table/data.frame factors into array of character strings
scc.good <- as.character(scc.good$SCC) 

# Filter the data
NEI.p6.balt.motor <- NEI.p6 %>% 
    filter(SCC == scc.good) %>% 
    filter(fips == "24510")

NEI.p6.la.motor <- NEI.p6 %>% 
    filter(SCC == scc.good) %>% 
    filter(fips == "06037")

# Reduce data into mean and median values for each year

NEI.balt <- NEI.p6.balt.motor %>%
    group_by(year) %>%
    summarise(Year = as.numeric(as.character(year)), 
              Mean.Emissions = mean(Emissions), 
              Median.Emissions = median(Emissions))

NEI.la <- NEI.p6.la.motor %>%
    group_by(year) %>%
    summarise(Year = as.numeric(as.character(year)), 
              Mean.Emissions = mean(Emissions), 
              Median.Emissions = median(Emissions))

# close.screen(all.screens = TRUE)
# close.screen(all.screens = TRUE)



png(filename = "plot6.png",width = 800, height = 600)

    par(mfcol=c(2,2))
    
    with(NEI.balt,
        plot(Mean.Emissions ~ Year, type = "l", 
             ylim = c(0, 10), xlim = c(2002, 2008), 
             main = "Baltimore Auto Emissions (Mean)",
             ylab = "MEAN Emissions") 
    )
    
    with(NEI.balt,
         plot(y = Median.Emissions, x = Year, type = "l",
              ylim = c(0, 1.0), xlim = c(2002, 2008), 
              main = "Baltimore Auto Emissions",
              ylab = "MEDIAN Emissions")
    )
    
    with(NEI.la,
         plot(Mean.Emissions ~ Year, type = "l", 
              ylim = c(0, 10), xlim = c(2002, 2008), 
              main = "Los Angelas Auto Emissions (Mean)",
              ylab = "MEAN Emissions") 
    )
    
    with(NEI.la,
         plot(y = Median.Emissions, x = Year, type = "l",
              ylim = c(0, 1.0), xlim = c(2002, 2008), 
              main = "Los Angelas Auto Emissions (Mean)",
              ylab = "MEDIAN Emissions") 
    )
    


dev.off()

