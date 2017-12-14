library(neuralnet)
set.seed(1234567890)

# generate 50 random numbers between 0-10
samples = runif(50, 0, 10)

# get the sinus of samples
df = data.frame(Var=samples, Sin=sin(samples))

# split 50/50 training/validation sets
train = df[1:25, ]
validation = df[26:50, ]

# intialial random weights for NN (31 total)
init_weights = runif(31, -1, 1)

errors.valid = numeric(100)
errors.train = numeric(100)
# run NN with threshold i/1000 for i in 1:100
for(i in 1:100) {
  nn <- neuralnet(
    formula = Sin ~ Var,
    data = train,
    startweights = init_weights,
    hidden = 10,
    threshold = i / 1000
  )
  
  # make predictions
  res.train = compute(nn, train[ ,"Var"])
  res.valid = compute(nn, validation[ ,"Var"])
  
  # calculate the SSE for each threshold
  errors.train[i] = sum((res.valid$net.result - train[ ,"Sin"])^2)
  errors.valid[i] = sum((res.valid$net.result - validation[ ,"Sin"])^2)
}

# plot errors as function of i in i/1000 in train and validation sets
plot(errors.valid, type="l", color="red")
points(errors.train, type="l", color="blue")

# the i value that gave least error for threshold i/1000
best.threshold = which.min(errors.valid) / 1000
best.nn <- neuralnet(
  formula = Sin ~ Var,
  data = train,
  startweights = init_weights,
  hidden = 10,
  threshold = best.threshold
)

# plot predictions and true observations
plot(prediction(best.nn)$rep1, col="red")
points(train, col="blue")
# plot NN
plot(best.nn)
