;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

set.seed(1234567890)
library(geosphere)
library(chron)

temps = read.csv("temps50k.csv", sep=",", dec=".",
                 header=TRUE, fill=TRUE)
stations = read.csv("stations.csv", sep=",", dec=".", 
                    header=TRUE, fileEncoding="latin1")
st = merge(stations, temps, by="station_number")

# the general function used by all kernels
gaussian=function(distance, h_value)
{
  return (exp(-(distance^2 / h_value)))
}

# if we reason that around 200km is a good cut-off for how close two stations should be,
# we try different h-values to find one with the appropirate cut-off
dist = seq(-400, 400, by=1)
plot(dist, gaussian(dist, 10000), type="l")
# width/h-value for distance kernel seems to be around 10000
h_distance = 10000

# new/obs is c('longitude', 'latitude')
distance_kernel=function(new, obs)
{
  distance_km = distHaversine(new, obs) / 1000
  return (gaussian(distance_km / h_distance))
}

# looking at some temperature diagrams, we reason that around 15 days might be a good interval
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

d1=as.character(st$date[1])
#d2=as.character(st$date[2])
d2 = "2004-05-10"

day_diff = as.Date(as.character(d1), format="%Y-%m-%d") - 
  as.Date(as.character(d2), format="%Y-%m-%d")

print(as.numeric(day_diff) %% 365)

di = day_difference(d2, d1)
print(di)
# new/obs are dates of YYYY-MM-DD strings
date_kernel=function(new, obs)
{
  day_diff = day_difference(new, obs)
  return (gaussian(day_diff, h_date))
}

time_difference=function(time1, time2)
{
  
}

t1 = "01:00:00"
t2 = "10:00:00"

diff1 = as.numeric(as.difftime(t1) - as.difftime(t2))
diff2 = as.numeric(as.difftime(t2) - as.difftime(t1))
print(min(diff1 %% 24, diff2 %% 24)) 

diff = as.difftime(t1) - as.difftime(t2)
print(as.numeric(diff))



d = as.difftime("01:00:00") - as.difftime("23:00:00")
print(d)

h_time = 1


d1 = strptime("1951-05-10", format = "%Y-%m-%d")
d2 = strptime("2004-05-28", format = "%Y-%m-%d")

diff_in_days = difftime(d1, d2, units = "days")
d = as.numeric(diff_in_days)
d = abs(d)
print(d)
a = d %% 365
