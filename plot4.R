# See notes on libraries at end of documents
library(data.table); library(dplyr); library(tidyr) 

#load data sets if not already in memory
if (!exists("NEI") | !exists("SCC")) source("loadData.R")

NEI.p4 <- NEI %>% 
    mutate(type = as.factor(type), year = as.factor(year)) %>%
    mutate(obs.id = paste(fips, SCC, type, sep = "-")) %>%
    mutate(obs.id.year = paste(year, obs.id, sep = "-"))

# Notes on strategy: 
#
# Develop a list of coal-related SCCs. Find this by source level 3 or source
# level 4 with any of the following words: ("Coal", "Anthracite", 
# "Bituminuous", "Lignite",  "Coke")
#

# Develop a list of SCCs.

scc.good <- SCC %>%
    # Matching terms
    filter(grepl(pattern = "Coal|Anthracite|Bituminuous|Subbituminous|Lignite|Coke|Ext Comb", 
                 perl = TRUE, ignore.case = TRUE, x = SCC.Level.Three)) %>%
    # Exclusion terms
    filter(!grepl(pattern = "By-product|Coal Bed Methane|Cleaning, and Material Handling|Charcoal", 
                  perl = TRUE, ignore.case = TRUE, x = SCC.Level.Three)) %>%
    select(SCC)

scc.good <- union(
    x = scc.good, 
    y =
        SCC %>%
        #Matching terms
        filter(grepl(pattern = "Coal|Anthracite|Bituminuous|Subbituminous|Lignite|Coke|Ext Comb", 
                     perl = TRUE, ignore.case = TRUE, x = SCC.Level.Four)) %>%
        #Excluded Terms
        filter(!grepl(pattern = "By-product|Coal Bed Methane|Cleaning, and Material Handling|Charcoal", 
                      perl = TRUE, ignore.case = TRUE, x = SCC.Level.Four)) %>%
        select(SCC)
)

# convert single column data.table/data.frame factors into array of character strings
scc.good <- as.character(scc.good$SCC) 

# Filter the data
NEI.p4.coal <- NEI.p4 %>% 
    filter(SCC == scc.good)

NEI.p4.small <- NEI.p4.coal %>%
    group_by(year) %>%
    summarise(Year = as.numeric(as.character(year)), 
              Mean.Emissions = mean(Emissions), 
              Median.Emissions = median(Emissions))


png(filename = "plot4.png",width = 800, height = 600)

split.screen(figs = c(1, 2)) # Split into two columns
split.screen(figs = c(1, 1), screen = 1) # One plot in the first column
split.screen(figs = c(2, 1), screen = 2) # Two plots in the second column

screen(3)
with(NEI.p4.coal,
     boxplot((Emissions+1) ~ year, log="y", 
             main = "Distribution of Coal-Related\nEmissions",
             ylab = "Emissions")
)

screen(4)
with(NEI.p4.small,
    plot(Mean.Emissions ~ Year, type = "b",
         main = "Mean Emissions Measured by Year",
         ylab = "Emissions") 
)

screen(5)
with(NEI.p4.small,
     plot(y = Median.Emissions, x = Year, type = "b",
          main = "Median Emissions Measured by Year",
          ylab = "Emissions")
)


dev.off()

close.screen(all.screens = TRUE)

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