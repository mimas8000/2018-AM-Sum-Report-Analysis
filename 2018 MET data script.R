
### MET data analysis
library(tidyverse)
library(readr)
library(xts)

### load met data
weath2018 <- read_csv("weath2018.txt", col_types = cols(Date_Time = col_datetime(format = 
  "%m/%d/%Y %H:%M:%S")))
View(weath2018)

### filter by site and date
    east <- filter(weath2018, WeatherStation == "East Point", Date_Time > as.POSIXlt("2018-07-17 06:55:00",
        tz = "America/Los_Angeles" ), Date_Time < as.POSIXlt("2018-07-22 07:05:00", 
        tz = "America/Los_Angeles"))
    
    shin <- filter(weath2018, WeatherStation == "Shingle Bay", Date_Time > as.POSIXlt("2018-07-17 06:55:00",
        tz = "America/Los_Angeles" ), Date_Time < as.POSIXlt("2018-07-22 07:05:00", 
        tz = "America/Los_Angeles"))

    ### Create data with mean hourly MET variable
        align.time.down = function(x,n){ index(x) = index(x)-n; align.time(x,n) }
        
        ### East Point Wind Speed
        east.xts <- xts(east$WindSpeed_mph, as.POSIXlt(east$Date_Time), format = "%m/%d/%Y %H:%M:%S")
        means.e <- period.apply(east.xts, endpoints(east.xts, "hours"), mean)
        means.e.rnd <- align.time.down(means.e, 60*60) 
        
        ### Shingle Bay Wind Speed
        shin.xts <- xts(shin$WindSpeed_mph, as.POSIXlt(shin$Date_Time), format = "%m/%d/%Y %H:%M:%S")
        means.s <- period.apply(shin.xts, endpoints(shin.xts, "hours"), mean)
        means.s.rnd <- align.time.down(means.s, 60*60) 

### plot time series of MET variable
    ggplot(fortify(means.e.rnd), aes(x = Index, y = means.e.rnd)) + geom_line() +
       geom_line(data = fortify(means.s.rnd), aes(x=Index, y=means.s.rnd), color="red") +
       annotate("text", x = as.POSIXct("2018-07-22 00:00:00"), y = 12, label = "East Point") + 
       annotate("text", x = as.POSIXct("2018-07-22 00:00:00"), y = 11, label = "Shingle Bay", 
       color = "red") + labs(x = "2018", y = "Mean Hourly Wind Speed (mph)")