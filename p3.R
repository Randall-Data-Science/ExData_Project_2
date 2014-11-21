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