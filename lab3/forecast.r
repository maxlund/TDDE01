set.seed(1234567890)
library(geosphere)

temps = read.csv("temps50k.csv", sep=",", dec=".",
                 header=TRUE, fill=TRUE)
stations = read.csv("stations.csv", sep=",", dec=".", 
                    header=TRUE, fileEncoding="latin1")
st = merge(stations, temps, by="station_number")

# general function used by all kernels
gaussian=function(distance, h_value)
{
  return (exp(-(distance^2 / h_value)))
}

# if we reason that 200km is a good cut-off for how close two stations should be
# we try different h-values to find one with the appropirate cut-off
dist = seq(-600, 600, by=1)
plot(dist, gaussian(dist, 10000), type="l")
# width/h-value for distance kernel seems to be around 10000
h_distance = 10000

# obs/real is c('longitude', 'latitude')
distance_kernel=function(obs, real)
{
  distance_km = distHaversine(obs, real) / 1000
  return (gaussian(distance_km / h_distance))
}

# looking at some temperature diagrams, we reason that 15 days might be a good interval
# also, since we have taken the distance threshold to be 200km, the temperature difference
# of our day-of-year distance should be somewhat similar.
dist = seq(-30, 30, by=1)
plot(dist, gaussian(dist, 45), type="l")

# a good h-value/width for the date/day-of-year interval seems to be about 45
h_date = 1

# helper function to get the difference between two dates, irrespective of their years
day_difference=function(date1, date2)
{
  day_diff = as.Date(as.character(date1), format="%Y-%m-%d") - 
             as.Date(as.character(date2), format="%Y-%m-%d")
  day_diff = abs(as.numeric(day_diff))
  return (day_diff %% 365)
}

# obs/real are dates of YYYY-MM-DD strings
date_kernel=function(obs, real, h_date)
{
  day_diff = day_difference(obs, real)
  return (gaussian(day_diff, h_date))
}

h_time = 1

plot(st$latitude, st$air_temperature)
