set.seed(1234567890)
library(geosphere)
library(chron)

temps = read.csv("temps50k.csv", sep=",", dec=".",
                 header=TRUE, fill=TRUE)
stations = read.csv("stations.csv", sep=",", dec=".", 
                    header=TRUE, fileEncoding="latin1")
st = merge(stations, temps, by="station_number")

# first we reason about reasonable thresholds for distance, day of year, 
# and time of day, then find the corresponding h-value that produces 
# the apropriate distribution

# the general function used by all kernels
gaussian=function(distance, h_value)
{
  return (exp(-(distance^2 / h_value)))
}

# if we reason that around 200km is a good threshold for how close two stations 
# should be, we tried different h-values to find an appropriate one
dist = seq(-400, 400, by=1)
plot(dist, gaussian(dist, 10000), type="l")
# width/h-value for distance kernel seems to be around 10000
h_distance = 10000

# looking at some temperature diagrams, we reason that around 15 days might be a 
# good interval. also, since we have taken the distance threshold to be 200km, 
# the temperature difference of our day-of-year distance should be somewhat similar.
dist = seq(-30, 30, by=0.1)
plot(dist, gaussian(dist, 45), type="l")
# a good h-value/width for the date/day-of-year interval seems to be about 45
h_date = 45

# estimating a good time of day threshold is difficult because of the uneven
# hours of light in sweden depending on time of the year. we choose/guess that
# 4 hours might be reasonable
dist = seq(-12, 12, by=0.01)
plot(dist, gaussian(dist, 4), type="l")
# a good h-value/width for the time-of-day interval seems to be about 4
h_time = 4

# next we need to write our kernel functions
# for the distance kernel we use distHaversine()

distance_kernel=function(lat1, long1, lat2, long2)
{
  location1 = c(lat1, long1)
  location2 = c(lat2, long2)
  distance_km = distHaversine(location1, location2) / 1000
  return (gaussian(distance_km, h_distance))
}

# for the date/day-difference kernel, we need some helper functions to find 
# the day-difference between to dates, which also accounts for leap-years

leaps = rev(c(
  "1932-2-29"
  ,"1936-2-29"
  ,"1940-2-29"
  ,"1944-2-29"
  ,"1948-2-29"
  ,"1952-2-29"
  ,"1956-2-29"
  ,"1960-2-29"
  ,"1964-2-29"
  ,"1968-2-29"
  ,"1972-2-29"
  ,"1976-2-29"
  ,"1980-2-29"
  ,"1984-2-29"
  ,"1988-2-29"
  ,"1992-2-29"
  ,"1996-2-29"
  ,"2000-2-29"
  ,"2004-2-29"
  ,"2008-2-29"
  ,"2012-2-29"
  ,"2016-2-29"
  ))

get_leapyears=function(d){
  for(i in 1:length(leaps)){
    if(as.Date(d) > as.Date(leaps[i])){
      return(i - 1)
    }
  }
}

get_day_diff=function(date1, date2){
  year1 = as.numeric(substr(date1, 0, 4))
  year2 = as.numeric(substr(date2, 0, 4))
  if (year1 > year2)
  {
    diff = difftime(strptime(date1, format="%Y-%m-%d"),
                    strptime(date2, format="%Y-%m-%d"), units="days")
  }
  else
  {
    diff = difftime(strptime(date2, format="%Y-%m-%d"),
                    strptime(date1, format="%Y-%m-%d"), units="days")
  }
  date1.leaps = get_leapyears(date1)
  date2.leaps = get_leapyears(date2)
  diff = diff - abs(date1.leaps - date2.leaps)
  diff = as.numeric(diff) %% 365
  return(min(diff, 365 - diff))
}

date_kernel=function(date1, date2)
{
  day_diff = get_day_diff(date1, date2)
  return (gaussian(day_diff, h_date))
}

# for the time kernel we also need a helper function to 
# find the time difference in hours between two times
get_time_diff=function(t1, t2)
{
  t1 = as.numeric(as.difftime(as.character(t1)), units="hours")
  t2 = as.numeric(as.difftime(as.character(t2)), units="hours")
  diff1 = t1 - t2
  diff2 = t2 - t1
  return (min(diff1 %% 24, diff2 %% 24))
}

time_kernel=function(time1, time2)
{
  time_diff = get_time_diff(time1, time2)
  return (gaussian(time_diff, h_time))
}

# our targets to forecast are the following:
latitude = 58.41
longitude = 15.618
target_date = "2017-12-11"
times = c("02:00:00", "04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00",  
          "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")

# we need to filter out dates that are later than our target for prediction
valid = subset(st, as.Date(date) < as.Date(target_date))

# get the weights from our date and distance kernels for each observation
date_weights = mapply(date_kernel, valid$date, target_date)
distance_weights = mapply(distance_kernel, 
                          valid$latitude, valid$longitude,
                          latitude, longitude)

preds_sum = numeric(length(times)) # the predictions using sum of kernels
preds_prod = numeric(length(times)) # .. using product

for (i in 1:length(times))
{
  # get weights from time kernel for each time
  time_weights = mapply(time_kernel, valid$time, times[i])
  kernel_sum = date_weights + distance_weights + time_weights
  kernel_prod = date_weights * distance_weights * time_weights
  # get the predicted temperature using kernel sum and kernel product
  preds_sum[i] = sum(kernel_sum * valid$air_temperature) / sum(kernel_sum)
  preds_prod[i] = sum(kernel_prod * valid$air_temperature) / sum(kernel_prod)
}

plot(preds_sum, xaxt="n", type="b",
     xlab="Time", ylab="Air temperature",
     main="Predicted air temperature using kernel sum")
axis(1, at=1:length(times), labels=times)

plot(preds_prod, xaxt="n", type="b",
     xlab="Time", ylab="Air temperature", 
     main="Predicted air temperature using kernel product")
axis(1, at=1:length(times), labels=times)