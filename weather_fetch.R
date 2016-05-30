
library(checkpoint)
checkpoint("2016-03-01")


library(rvest)
library(selectr)
library(dplyr)
library(lubridate)

setwd("D:/Users/jgibbins/Desktop/Formatting/R/weather")

#############################
#############################


welland_pelham_2010 = read.csv("welland_pelham_2010.csv") 
welland_pelham_2011 = read.csv("welland_pelham_2011.csv") 
#welland_pelham_2012 = read.csv("welland_pelham_2012.csv") 
welland_pelham_2013 = read.csv("welland_pelham_2013.csv") 
welland_pelham_2014 = read.csv("welland_pelham_2014.csv") 

# selecting the columns that we want
welland_pelham_2010 = welland_pelham_2010 %>%
  select(X, Date_Time, Temp, Rel.Hum.Definition.)

welland_pelham_2011 = welland_pelham_2011 %>%
  select(X, Date_Time, Temp, Rel.Hum.Definition.)

# welland_pelham_2012 = welland_pelham_2012 %>%
#   select(X, Date_Time, Temp, Rel.Hum.Definition.) %>%
#   mutate(day = day(Date_Time)) %>%
#   mutate(month = month(Date_Time)) %>%
#   filter(month != 2 | day != 29) %>%
#   select(X, Date_Time, Temp, Rel.Hum.Definition.)

welland_pelham_2013 = welland_pelham_2013 %>%
  select(X, Date_Time, Temp, Rel.Hum.Definition.)

welland_pelham_2014 = welland_pelham_2014 %>%
  select(X, Date_Time, Temp, Rel.Hum.Definition.)

# merge the data.frames together
test = merge(welland_pelham_2010, welland_pelham_2011, all=TRUE)
#test = merge(test, welland_pelham_2012, all = TRUE)
test = merge(test, welland_pelham_2013, all = TRUE)
test = merge(test, welland_pelham_2014, all = TRUE)

test$Temp = as.numeric(levels(test$Temp))[test$Temp]
test$Rel.Hum.Definition. = as.numeric(levels(test$Rel.Hum.Definition.))[test$Rel.Hum.Definition.]

averaged = test %>%
  mutate(day_year = yday(Date_Time)) %>%
  mutate(hour = hour(Date_Time)) %>%
  group_by(day_year, hour) %>%
  summarise(avg_temp = mean(Temp, na.rm=TRUE), avg_humidity = mean(Rel.Hum.Definition., na.rm=TRUE)) 

write.csv(test, file = "data_pre_average.csv")
write.csv(averaged, file = "data_average.csv")

#############################
# Use this section to ONLY check a single day

weather = html("http://climate.weather.gc.ca/climateData/hourlydata_e.html?timeframe=1&Prov=ON&StationID=44283&hlyRange=2005-11-23|2015-08-29&Year=2014&Month=6&Day=22")

test = weather %>%
  html_nodes("table") %>%
  .[[2]] %>%
  html_table()


#############################
# Output to CSV when the data.frames are all fetched

write.csv(welland_pelham_2010, file = "welland_pelham_2010.csv")
write.csv(welland_pelham_2011, file = "welland_pelham_2011.csv")
write.csv(welland_pelham_2012, file = "welland_pelham_2012.csv")
write.csv(welland_pelham_2013, file = "welland_pelham_2013.csv")
write.csv(welland_pelham_2014, file = "welland_pelham_2014.csv")


###############################################
###############################################

# this is the main section to run to fetch the web data from the GC weather website

# enter yesterdays date ... (part of the hyperlink below)

today="2015-08-29"

# Run this line to fetch weather data
welland_pelham_2010= fetch.temperature('44283', '2010-01-01', '2010-12-31')

# need to update to:
# the station id above doesn't do anything. Need to update as required in the function below and rerun the function.
# update the "range" in the function below too


# WELLAND-PELHAM = 44283 
# Kitchener = 48569
# PORT COLBORNE (AUT) = 9005 
# VINELAND STATION RCS = 31367 
  
###############################################
###############################################



fetch.temperature <- function(station, start.date, end.date) {
  date.range <- seq.Date(from=as.Date(start.date), to=as.Date(end.date), by='1 day')
  
  interm_2 <- fetch.temperature.one(station, date.range[1])
  
  # loop over dates, and fetch data
  for(i in seq_along(date.range)) {
    print(date.range[i])
    interm <- fetch.temperature.one(station, date.range[i])
    interm_2 = rbind(interm_2, interm)
  }
  return(interm_2)
}

###############################################

fetch.temperature.one <- function(station, date) {
  w <- fetch.weather.raw(station, date)
  return(w)
}

###############################################

# update the Range and the Station ID in the hyperlink below BEFORE running the above

fetch.weather.raw <- function(station, date) {
  base_url <- 'http://climate.weather.gc.ca/climateData/hourlydata_e.html?timeframe=1&Prov=ON&StationID=44283&hlyRange=2005-11-23|'
  
  date <- as.Date(date)
  # parse date
  m <- as.integer(format(date, '%m'))
  d <- as.integer(format(date, '%d'))
  y <- format(date, '%Y')
  
  # compose final url
  final_url <- paste(base_url,today,
                     '&Year=', y,
                     '&Month=', m,
                     '&Day=', d,
                     sep='')
  
  day_weather = final_url %>%
    html %>%
    html_nodes("table") %>%
    .[[2]] %>%
    html_table()
  
  # added this IF statement to prevent function from crashing when encountering an empty data table.
  
  if(nrow(day_weather) > 1){
    names(day_weather)[2]<-"Temp"
    names(day_weather)[1]<-"Time"
    day_weather = day_weather %>%
      filter(!is.na(Temp))          # remove the top empty row
    day_weather$Date = date
    day_weather$Date_Time = ymd_hm(paste(day_weather$Date, day_weather$Time))
    return(day_weather)
  } else {}
}
