# install package readxl if needed with:
# install.packages("readxl")

library(readxl)

df_machines = read_excel("machines.xlsx")
machines = data.matrix(df_machines)
#head(machines)

probability=function(theta, x) {
  return (theta * exp(-theta * x))
}

log_likelihood=function(theta, x) {
  return (log(probability(theta, x)))
}

# TODO
# fit theta (try different values of theta?) to data in machines
# .. find a value for theta that maximizes the likelihood
# of the data observed in machines
