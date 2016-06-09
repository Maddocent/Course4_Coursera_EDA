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

data_twoDays <- rbind(data_selected_1,data_selected_2)
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

# inspecting the just created variable "data_twoDays$date_time"
date_time_var <- data_twoDays$date_time
head(date_time_var)
typeof(date_time_var)




## ---- Plot2--------------------------------------------------------------
levels_date <- levels(as.factor(data_twoDays$date))

## plot2.png in ggplot2 syntax 
names(data_twoDays)


plot2 <- ggplot(data_twoDays, aes(date_time, global_active_power)) + 
  geom_line() +
  scale_x_datetime(date_breaks = "1 day", date_labels = c("Sat", "Thu", "Fri")) +
  ylab("Global Active Power (kilowatts)") + 
theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

# saving the plot to disk
png(filename = "./images/plot2.png", width = 480, height = 480, units = "px")
plot2
dev.off()



