# load data from csv and convert to matrix
# if csv is using comma separator of decimal points,
# change to data = "," in argument passed to read.csv()

data_csv = read.csv("machines.csv", header = TRUE, sep = ",", quote = "\"",
                    dec = ".", fill = TRUE, comment.char = "")
machines = as.matrix(data_csv)

# the probability function used to calculate the log-likelihood
probability=function(theta, data) {
  return (theta * exp((-theta) * data))
}

# sum of log-likelihoods function, returns sum of the log-likelihoods using all observations
sum_log_likelihood=function(theta) {
  return(sum(log(probability(theta, machines))))
}

# sum of log-likelihoods function, returns sum of the log-likelihoods using just the first 6 observations
sum_log_likelihood2=function(theta) {
  return(sum(log(probability(theta, machines[1:6]))))
}

# prior probability for theta for the bayesian model
prior_probability=function(theta) {
  return (10 * exp(-10 * theta))
}

# sum of log of posterior likelihoods
sum_log_posterior_likelihood=function(theta) {
  return (sum(log(probability(theta, machines)*prior_probability(theta))))
}

# function needs to be vectorized to be accepted by curve.. obviously! (?)
f <- Vectorize(sum_log_likelihood)
# plot function for values of theta from 0 to 3
curve(f, from=0, to=3, xlab="Theta", ylab="Sum of log-likelihoods (all observations)")
# find the theta value that maximizes the sum of log-likelihoods function with all observations
max_likelihood_all = optimize(f, interval=c(0, 3), maximum=TRUE)
cat("max log-likelihood value using all observations is given by theta =", max_likelihood_all$maximum, "\n")

# repeat using only the first six observations in 'machines' 
# (can't figure out how to pass several arguments when plotting with curve() function)
f2 <- Vectorize(sum_log_likelihood2)
curve(f2, from=0, to=3, xlab="Theta", ylab="Sum of log-likelihoods (first six observations)")
max_likelihood_first6 = optimize(f2, interval=c(0, 3), maximum=TRUE)
cat("max log-likelihood value using only first 6 observations is given by theta =", max_likelihood_first6$maximum, "\n")

# use the log-likelihood function where theta has a prior probability (bayesian model)
f3 <- Vectorize(sum_log_posterior_likelihood)
curve(f3, from=0, to=3, xlab="Theta", ylab="Sum of log of posterior likelihoods")
max_likelihood_bayesian = optimize(f3, interval=c(0, 3), maximum=TRUE)
cat("max log-likelihood in p(x, theta) gives optimal theta =", max_likelihood_bayesian$maximum, "\n")

# generate 50 new observations from the exponential distribution
new_observations = rexp(50, rate = max_likelihood_all$maximum)

# plot histograms with the old and new observations
hist(machines, plot=TRUE, main="The original observations")
hist(new_observations, plot=TRUE, main="New observations drawn from the exponential distribution")