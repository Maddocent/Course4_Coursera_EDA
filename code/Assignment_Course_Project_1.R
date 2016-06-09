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

POSIXlt

# The POSIXlt data type is a vector, and the entries in the vector have 
# the following meanings:
# [[1]] seconds
# [[2]] minutes
# [[3]] hours
# [[4]] day of month (1-31)
# [[5]] month of the year (0-11)
# [[6]] years since 1900
# [[7]] day of the week (0-6 where 0 represents Sunday)
# [[8]] day of the year (0-365)
# [[9]] Daylight savings indicator (positive if it is daylight 
#       savings)

## Question what day of the week is 1/2/2007?
date_time_var[1][[7]]

## the answer is four
## making a look-up table df
days_of_the_week <- c("Sunday", "Monday", "Thuesday", "Wednesday",
                    "Thursday", "Friday", "Saturday")
POSIXlt_code <- c(0:6)

dow_df <- cbind(days_of_the_week, POSIXlt_code)
dow_df <- as.data.frame(dow_df)
names(dow_df)
which_day <- dow_df %>% filter(POSIXlt_code == 4)

## The final answer:
which_day[1]


## ---- Plot1--------------------------------------------------------------
names(data_twoDays)

## save the file
png(filename = "./images/plot1.png", width = 480, height = 480, units = "px")
hist(data_twoDays$global_active_power, col = "red", xlab = "Global Active Power", ylab = "Frequency", main = "Global Active Power")
dev.off()  

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

## ---- Plot3--------------------------------------------------------------
# checking the names
names(data_twoDays)

# creating plot3 with ggplot
plot3 <- ggplot(data_twoDays, aes(x = date_time)) + 
  geom_line(aes(y = sub_metering_1, color = "Sub_metering_1")) +
  geom_line(aes(y = sub_metering_2, color = "Sub_metering_2")) +
  geom_line(aes(y = sub_metering_3, color = "Sub_metering_3")) +
  scale_x_datetime(date_breaks = "1 day", date_labels = c("Sat", "Thu", "Fri")) +
  ylab("Energy sub metering") +
  scale_colour_manual("", values = c("Sub_metering_1" = "black", 
                                     "Sub_metering_2" = "red",
                                     "Sub_metering_3" = "blue")) +
theme_bw() +
theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(legend.position=c(0.7, 0.9)) +
  theme(legend.background = element_rect(fill="gray95", size=.3, linetype="dotted"))

# saving the plot to disk
png(filename = "./images/plot3.png", width = 480, height = 480, units = "px")
plot3
dev.off()

## ---- Plot4--------------------------------------------------------------
# checking names
names(data_twoDays)

## plot 4 is a panel of four plots consiting of plot2, plot3 and two new plots
## first create the two additonal plots (plot5 and plot6) that have to go in the panel

## plot5 --> x = date_time, y = voltage
plot5 <- ggplot(data_twoDays, aes(x = date_time)) + 
  geom_line(aes(y = voltage)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = c("Sat", "Thu", "Fri")) +
  ylab("Voltage") +
  xlab("datetime") +
  theme_bw() +
theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
theme(plot.margin = unit(c(1,1,1,1), "cm"))

## plot6 --> x = date_time, y = global_reactive_power
names(data_twoDays)
plot6 <- ggplot(data_twoDays, aes(x = date_time)) + 
  geom_line(aes(y = global_reactive_power)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = c("Sat", "Thu", "Fri")) +
  ylab("Global_reactive_power") +
  xlab("datetime") +
  theme_bw() +
theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

##################################
## CREATING A PANEL ##
##################################

# creating the panel with plot_grid 
plot4 <- plot_grid(plot2, plot5, plot3, plot6,
          labels=c("A", "B", "C", "D"), ncol = 2, nrow = 2)

# saving plot 4 to disk
png(filename = "./images/plot4.png", width = 480, height = 480, units = "px")
plot4
dev.off()


