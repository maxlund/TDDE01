# load data from csv and convert to matrix
data_csv = read.csv("machines.csv", header = TRUE, sep = ",", quote = "\"",
                dec = ",", fill = TRUE, comment.char = "")
machines = as.matrix(data_csv)

# the probability function used to calculate the log-likelihood
probability=function(theta, data) {
  return (theta * exp((-1 * theta) * data))
}

# log-likelihood function, returns sum of the log-likelihoods using all observations
log_likelihood=function(theta) {
  return(sum(log(probability(theta, machines))))
}

# log-likelihood function, returns sum of the log-likelihoods using just the first 6 observations
log_likelihood2=function(theta) {
  return(sum(log(probability(theta, machines[1:6]))))
}

# prior probability for theta for the bayesian model
prior_probability=function(theta) {
  return (10 * exp(-10 * theta))
}

bayesian_log_likelihood=function(theta) {
  return (sum(log(probability(theta, machines)*prior_probability(theta))))
}

# function needs to be vectorized to be accepted by curve.. obviously! (?)
f <- Vectorize(log_likelihood)
# plot function for values of theta from 0 to 3
curve(f, from=0, to=3, xlab="theta", ylab="likelihood")
# find the theta value that maximizes the log-likelihood function with all observations
max_likelihood_all = optimize(f, interval=c(0, 3), maximum=TRUE)
cat("max log-likelihood value using all observations is given by theta =", max_likelihood_all$maximum)

# repeat using only the first six observations in 'machines' 
# (can't figure out how to pass several arguments  when plotting with curve() function)
f2 <- Vectorize(log_likelihood2)
curve(f2, from=0, to=3, xlab="theta", ylab="likelihood")
max_likelihood_first6 = optimize(f2, interval=c(0, 3), maximum=TRUE)
cat("max log-likelihood value using only first 6 observations is given by theta =", max_likelihood_first6$maximum)

# use the log-likelihood function where theta has a prior probability (bayesian model)
f3 <- Vectorize(bayesian_log_likelihood)
curve(f3, from=0, to=3, xlab="theta", ylab="likelihood")
max_likelihood_bayesian = optimize(f3, interval=c(0, 3), maximum=TRUE)
cat("max log-likelihood in p(x, theta) gives optimal theta =", max_likelihood_bayesian$maximum)

# generate 50 new observations from the exponential distribution
for (i in 1:50) {
  new_observations = rexp(50, rate = max_likelihood_all$maximum)
}
# plot histograms with the old and new observations
hist(machines, plot=TRUE)
hist(new_observations, plot=TRUE)

