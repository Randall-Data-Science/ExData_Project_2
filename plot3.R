# See notes on libraries at end of documents
library(data.table); library(dplyr); library(tidyr) 
library(ggplot2)

#load data sets if not already in memory
if (!exists("NEI") | !exists("SCC")) source("loadData.R")

NEI.p3 <- NEI[fips == "24510",] %>% 
    mutate(type = as.factor(type), year = as.factor(year)) %>%
    mutate(obs.id = paste(fips, SCC, type, sep = "-")) %>%
    mutate(obs.id.year = paste(year, obs.id, sep = "-"))


# Average multiple measurements of a single type within one year together
NEI.p3 <- NEI.p3 %>% 
    group_by(obs.id.year) %>%
    summarise(fips, SCC, Pollutant, type, year, obs.id, Emissions = mean(Emissions))

#create tiny table for graph
p3.data <- NEI.p3 %>%
    select(type, year, Emissions) %>%
    group_by(year, type) %>%
    summarise(Emissions=mean(Emissions))


typeDecrease <- function(tp) {
    sprintf(
        "%.1f%%\ndecrease", ( 
            100 - 100 * p3.data[year=="2008" & type==tp]$Emissions /
                p3.data[year=="1999" & type==tp]$Emissions
        )
    )
}


# Yes, this is a little bit ugly:

png(filename = "plot3.png",width = 800, height = 600)

ggplot(p3.data, aes(x = year, y = Emissions)) +
    labs(title = "PM_2.5 Emissions by Type of Measurement Station", x = "Year") +
    geom_line(mapping = aes(group = type, color = type), size = 1.0) +
    geom_point(mapping = aes(color = type), size = 3, shape = 16) +
    xlim("1999", "2002", "2005","2008") +
    geom_text(aes(label = sprintf("%.2f", Emissions), color = type),
              hjust = -0.5, vjust = -1, size = 3.5) + 
    ylim(-50, 100) +

    annotate(geom = "text", x = "1999", y = -30, label = sprintf(
        "POINT\n%.1f%%\ndecrease", ( 
            100 - 100 * p3.data[year=="2008" & type=="POINT"]$Emissions /
                p3.data[year=="1999" & type=="POINT"]$Emissions
        )
    ), fontface = "bold"
    ) +
    annotate(geom = "text", x = "2002", y = -30, label = sprintf(
        "NON-ROAD\n%.1f%%\ndecrease", ( 
            100 - 100 * p3.data[year=="2008" & type=="NON-ROAD"]$Emissions /
                p3.data[year=="1999" & type=="NON-ROAD"]$Emissions
        )
    )
    ) +
    annotate(geom = "text", x = "2005", y = -30, label = sprintf(
        "ON-ROAD\n%.1f%%\ndecrease", ( 
            100 - 100 * p3.data[year=="2008" & type=="ON-ROAD"]$Emissions /
                p3.data[year=="1999" & type=="ON-ROAD"]$Emissions
        )
    )
    ) +
    annotate(geom = "text", x = "2008", y = -30, label = sprintf(
        "NONPOINT\n%.1f%%\ndecrease", ( 
            100 - 100 * p3.data[year=="2008" & type=="NONPOINT"]$Emissions /
                p3.data[year=="1999" & type=="NONPOINT"]$Emissions
        )
    )
    )


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
