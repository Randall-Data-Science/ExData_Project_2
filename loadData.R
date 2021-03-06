#Check libraries available, and load in order
if (!require("data.table")) install.packages("data.table")
if (!require("dplyr"))      install.packages("dplyr")
if (!require("tidyr"))      install.packages("tidyr")
library(data.table)
library(dplyr)
library(tidyr)

#Confirm existence of data files, download&extact if necessary
if (!file.exists("data/summarySCC_PM25.rds") | !file.exists("data/Source_Classification_Code.rds")) {
    download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", 
                  method = "curl", destfile = "temp.zip", quiet = TRUE)
    unzip(zipfile = "temp.zip", exdir = "data")
    file.remove("temp.zip")
}

#Load data into data.frame, then wrap in data.table, then dplyr's data.table
NEI <- readRDS("data//summarySCC_PM25.rds")
NEI <- data.table(NEI)
NEI <- tbl_dt(NEI)

#Same operation with index of data
SCC <- tbl_dt(data.table(readRDS("data//Source_Classification_Code.rds")))
