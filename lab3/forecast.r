library(geosphere)

temps = read.csv("temps50k.csv", sep=",", dec=".",
                 header=TRUE, fill=TRUE)
stations = read.csv("stations.csv", sep=",", dec=".", 
                    header=TRUE, fileEncoding="latin1")
st = merge(stations, temps, by="station_number")

# stddev of gaussian
h_distance = 1
h_date = 1
h_time = 1




# a <- 58.4274 # The point to predict (up to the students)
# b <- 14.826
# date <- "2013-11-04" # The date to predict (up to the students)
# times <- c("04:00:00", "06:00:00", ..., "24:00:00")
# temp <- vector(length=length(times))
# # Students’ code here
# plot(temp, type="o")