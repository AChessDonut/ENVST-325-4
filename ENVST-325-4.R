install.packages(c("lubdridate", "dplyr", "ggplot2"))
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
library(lubridate)
library(dplyr)
library(ggplot2)

weather <- read.csv("campus_weather.csv")
#sensor_log <- read.csv("Sensor log.csv")
#meter_weather <- read.csv("meter_weather_metadata.csv")

#Parse functions
weather$dateF <- mdy_hm(weather$Date)
#Ex: 15-minute time interval/increments from previous dataset
interval <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1]
interval
#x refers to something that will be run as x
timeInterval <- function(x) {
  x[-length(x)] %--% x[-1]
  
}
timeInterval(weather$dateF)

