## ---- Packages-----------------------------------------------------------
# install.packages("downloader")
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("cowplot")

## loading the packages
library(rmarkdown)
library(downloader)
library(dplyr)
library(lubridate)
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)


## ---- DownloadingData----------------------------------------------------
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download(url, dest="project_dataset.zip", mode="wb") 
unzip("project_dataset.zip", exdir = "./data")

## ---- ReadingDataSet-----------------------------------------------------
## for help:
# ??read.table
file <- list.files("./data", pattern = "txt", full.names = TRUE)
data <- read.table(file, sep = ";", header = TRUE, na.strings = "?")

## ---- SelectingCases-----------------------------------------------------
str(data)
tail(data)
## names to lower
names(data) <- tolower(names(data))
names(data)

data$date <- as.character(data$date)
head(data)
data_selected_1 <- data %>% filter(date == "1/2/2007")
data_selected_2 <- data %>% filter(date == "2/2/2007")

data_twoDays <- rbind(data_selected_1, data_selected_2)
head(data_twoDays)
tail(data_twoDays)

## ------------------------------------------------------------------------
# converting to "character"
data_twoDays$date <- as.character(data_twoDays$date)
data_twoDays$time <- as.character(data_twoDays$time)

# pasting date time together to be able to create a 
# "POSIXlt" class variable 
data_twoDays$date_time <- paste(data_twoDays$date,
                                data_twoDays$time,
                                sep = " ")
head(data_twoDays)

# converting $date_time to class "POSIXlt"
data_twoDays$date_time <- strptime(data_twoDays$date_time,
                               "%d/%m/%Y %H:%M:%S")



## ---- Plot1--------------------------------------------------------------
names(data_twoDays)

## save the file
png(filename = "./images/plot1.png", width = 480, height = 480, units = "px")
hist(data_twoDays$global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", ylab = "Frequency", main = "Global Active Power")
dev.off()  


