library(tree)
library(boot)

set.seed(12345)
data = read.csv("State.csv", header = TRUE, 
                sep = ";", dec = ",", fill = TRUE)

# sort data by variable MET (desc)
data.ordered = data[order(data$MET),]

# plot EX vs MET
plot(data.ordered$MET, data.ordered$EX)
# fit a tree model, #observations in leaf >= 8
model = tree(formula = EX ~ MET,
             data = data.ordered,
             control = tree.control(nrow(data.ordered), minsize = 8))
# perform cross-validation on model
model.cv = cv.tree(model)
plot(model.cv, type="p")
# select optimal number of leaves
best.size = model.cv$size[which(model.cv$dev==min(model.cv$dev))]
# prune tree using selected num of leaves from CV
model.optimal = prune.tree(model, best = best.size)
# report the selected tree
summary(model.optimal)

#*****
# TODO: plot original and fitted data..
#*****
# like this?
fitted_data = predict(model.optimal, newdata = data.ordered)
plot(data.ordered$MET, data.ordered$EX, col = "green")
points(data.ordered$MET, preds, col = "red")

# histogram of residuals
hist(resid(model.optimal))

# function that produces the statistics for the nonparametric bootstrap function boot()
nonparametric = function(data, index)
{
  sample = data[index, ]
  model = tree(formula = EX ~ MET,
               data = sample,
               control = tree.control(nrow(sample), minsize = 8))
  model.pruned = prune.tree(model, best = best.size)
  prediction = predict(model.pruned, newdata = data.ordered)
  return (prediction)
}

# same for parametric boostrapping
parametric = function(data)
{
  model = tree(formula = EX ~ MET,
               data = data,
               control = tree.control(nrow(data), minsize = 8))
  model.pruned = prune.tree(model, best = best.size)
  prediction = predict(model.pruned, newdata = data.ordered)
  return (prediction)
}

# generate some new data for EX
rng = function(data, mle)
{
  new_data = data.frame(EX = data$EX, MET = data$MET)
  n = length(data$EX)
  new_data$EX = rnorm(n, predict(mle, newdata = data), sd(resid(mle)))
  return(new_data)
}

boot.nonparam = boot(data = data.ordered,
                     statistic = nonparametric,
                     R = 1000)
boot.nonparam.cb = envelope(boot.nonparam, level = 0.95)

#plot(boot.nonparam)

# plot MET vs EX, predictions and
# confidence bands for model's predictions
plot(data.ordered$MET, data.ordered$EX, col = "green", main = "Confidence bands (non-parametric)")
points(data.ordered$MET, preds, col = "red")
lines(data.ordered$MET, boot.nonparam.cb$point[1, ])
lines(data.ordered$MET, boot.nonparam.cb$point[2, ])

boot.param = boot(data = data.ordered,
                  statistic = parametric,
                  R = 1000,
                  mle = model.optimal,
                  ran.gen = rng,
                  sim = "parametric")
boot.param.cb = envelope(boot.param, level = 0.95)

# same for parametric bootstrapping
plot(boot.param)
plot(data.ordered$MET, data.ordered$EX, col = "green", main = "Confidence bands (parametric)")
points(data.ordered$MET, preds, col = "red")
lines(data.ordered$MET, boot.param.cb$point[1, ])
lines(data.ordered$MET, boot.param.cb$point[2, ])

# ********************
# TODO: prediction bands ..?
# ********************

