# See notes on libraries at end of documents
library(data.table)
library(dplyr)
library(tidyr) 

#load data sets if not already in memory
if (!exists("NEI") | !exists("SCC")) source("loadData.R")

NEI.p5 <- NEI %>% 
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
    # Matching terms (It seems safe to assume that the question is particularly
    # interested street-legal highway vehicles, rather than off-road vehicles
    filter(grepl(pattern = "Highway Vehicles", 
                 perl = TRUE, ignore.case = TRUE, x = SCC.Level.Two)) %>%
    select(SCC)

# convert single column data.table/data.frame factors into array of character strings
scc.good <- as.character(scc.good$SCC) 

# Filter the data
NEI.p5.balt.motor <- NEI.p5 %>% 
    filter(SCC == scc.good) %>% 
    filter(fips == "24510")

# Reduce data into mean and median values for each year

NEI.p5.plot <- NEI.p5.balt.motor %>%
    group_by(year) %>%
    summarise(Year = as.numeric(as.character(year)), 
              Mean.Emissions = mean(Emissions), 
              Median.Emissions = median(Emissions))

png(filename = "plot5.png",width = 800, height = 600)

split.screen(figs = c(1, 2)) # Split into two columns
split.screen(figs = c(1, 1), screen = 1) # One plot in the first column
split.screen(figs = c(2, 1), screen = 2) # Two plots in the second column

screen(3)
with(NEI.p5.balt.motor,
     boxplot((Emissions+1) ~ year, log="y", 
             main = "Baltimore, Motor Vehicle Emissions",
             ylab = "Emissions")
)

screen(4)
with(NEI.p5.plot,
    plot(Mean.Emissions ~ Year, type = "b",
         main = "Mean Emissions Measured by Year",
         ylab = "Emissions") 
)

screen(5)
with(NEI.p5.plot,
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