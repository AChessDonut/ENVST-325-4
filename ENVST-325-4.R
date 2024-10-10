library(lubridate)
library(dplyr)
library(ggplot2)
library(terra)

weather <- read.csv("campus_weather.csv", na.strings = "#N/A")
#sensor_log <- read.csv("Sensor log.csv")
#meter_weather <- read.csv("meter_weather_metadata.csv, na.strings = NA")

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

for( i in 1:6) {
  print(paste("example", i))
}

seqEx <- numeric()
seqEx <- c(1, 4, 6)
for (i in seqEx) {
  print(paste("example", i))
}

chEx <- character()
for( i in 1:6) {
 chEx[i] <- paste("example", i)
}
chEx

numEx <- numeric()
for( i in 1:6) {
  numEx[i] <- 6*i
}
numEx

numEx2 <- numeric()
for( i in 2:6) {
  numEx2[i] <- 6*i
}
numEx2

#Use the weather_station.csv data described in the tutorial to answer the following prompts.
#Prompt 1
#Calculate a rolling average of air temperatures over eight 15 min measurements (2 hours) 
#for January of 2022 using a for loop. 
#Make a plot of the 15 minute air temperature and the rolling average.
#isolate january 2022
weather$month <- month(weather$dateF)
weather$year <- year(weather$dateF)
Jan2022 <- weather %>%
  filter(month == 1 & year == 2022)
  
mean(Jan2022$AirTemp[1:8])
rollAveTemp <- numeric()
for (i in 8:nrow(Jan2022)) {
  rollAveTemp[i] <- mean(Jan2022$AirTemp[(i-7):i])
}
Jan2022$rollAveTemp <- rollAveTemp

  
roll_ave_and_air_temp_plot <- plot(rollAveTemp, Jan2022$AirTemp)

#Prompt 2
#You want to see if the solar radiation measurements experienced any issues with build up 
#or accumulation on the sensor in May and June of 2021. Make an assessment with your group.
# parse date
weather$dateF <- mdy_hm(weather$Date)
# create a month column
weather$doy <- yday(weather$dateF)
# create a year column
weather$year <- year(weather$dateF)
ggplot(data=weather[weather$doy > 121 & weather$doy < 274 ,],
       aes(x=dateF,
           y=Precip))+
  geom_col(color="royalblue")+
  theme_classic()

#Prompt 3
#Check for any date time issues using the function created in the tutorial. 
#Investigate instances of date time issues. What happens around daylight savings? 
#Are there issues with the time zone assumption?
# calculate interval times


# set up intervals
intervals <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1] # start date %--% end date
# interval starts with row 1, and row 2
# and ends on second to last row and final row
interval_times <- int_length(intervals)
# check interval times
intervals[interval_times != 900]
#create function for checking for irregular intervals that
# deviate from 900 seconds
# only argument is x, a vector of POSIXct formatted dates to be checked
# with an expected interval of 900 seconds
# the output is all observations that deviate from 900 seconds
timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
  
}
# run on weather data
timeCheck900(weather$dateF)


#Problem 1: You been asked to share precipitation data with the village of Clinton. 
#You want to ensure that there are no issues with the bird excrement or 
#frozen precipitation. You want to exclude any precipitation that occurs when the 
#air temperature is below zero. You also want to check that no precipitation 
#measurements are used if the X and Y level observations are more than 2 degrees.

#Indicate how many missing precipitation values are in your data. 
#Do you think there might be any additional issues with the precipitation data 
#to consider before sending the data to the village. 
#Generally describe (do not code) what you might need to do for further data cleaning.

# examine precipitation using a bar plot
ggplot(data = weather[weather$AirTemp < 0 & 
                        abs(weather$X - weather$Y) > 2, ], 
       aes(x = AirTemp, y = Precip)) +
  geom_point(color = "darkred") +
  theme_classic() +
  labs(title = "Precipitation vs. Air Temperature (Below Zero)",
       x = "Air Temperature (°C)",
       y = "Precipitation")


#Problem 2: Create a data flag that warns a user if the battery voltage falls 
#below 8.5 Volts. Explain how you set up the flag.
weather$BatteryVoltageFlag <- ifelse(weather$BatVolt <= 8500, 1, 0) 

#Problem 3: You should also create a function that checks for observations 
#that are in unrealistic data ranges in air temperature and solar radiation. 
#Explain how your function works.
check_unrealistic_values <- function(data) {
  data$Air_Temp_Flag <- ifelse(data$AirTemp < 0, 1, 0)
  
  # Flag for unrealistic solar radiation values (below 0 or above 2.5 W/m^2)
  data$Sol_Radiation_Flag <- ifelse(data$SolRad < 0 | data$SolRad > 2.5, 1, 0)
}
check_unrealistic_values(weather)

#Problem 4: Make a plot of winter air temperatures in Jan - Mar of 2021. Check 
#for persistence issues that might indicate snow accumulation on the sensor. 
#Describe whether you think this might be an issue.
# Function to check for persistent low temperatures
winter_air_temps <- function(temp, date) {
  # Filter the dates and temperatures for Jan - Mar 2021
  date_filtered <- date[date >= "2021-01-01" & date <= "2021-03-31"]
  temp_filtered <- temp[date >= "2021-01-01" & date <= "2021-03-31"]
  
  # Return a data frame with the filtered temperatures and dates
  data.frame(Date = date_filtered, AirTemp = temp_filtered)
}
winter_data <- winter_air_temps(weather$AirTemp, weather$dateF)

ggplot(winter_data, aes(x = Date, y = AirTemp)) +
  geom_line(color = "#717773") + # Plot the air temperatures
  theme_classic() + # Classic theme
  labs(title = "Winter Air Temperatures (Jan - Mar 2021)",
       x = "Date",
       y = "Air Temperature (°C)") 

#Problem 5: You are asked for total daily precipitation in March and April of 2021. 
#Use a for loop to exclude (convert to NA) any days that include temperatures 
#less than 35 degrees F on that day or the day prior to ensure that measurements 
#are not likely to be affected by snow accumulation on the sensor. 
#How many daily observations have precipitation observations (not a NA) 
#in your final data table?
