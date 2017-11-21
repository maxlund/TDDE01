library(tree)
library(boot)

set.seed(12345)
data = read.csv("State.csv", header = TRUE, 
                sep = ";", dec = ",", fill = TRUE)

# sort data by variable MET (desc)
data.ordered = data[order(MET),]

# plot EX vs MET
plot(data.ordered$EX, data.ordered$MET)
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
# TODO: plot original and fitted data.. ??
#*****
# histogram of residuals
hist(resid(model.optimal))

# function that produces the statistics for the bootstrap function boot()
boot.func.nonparam = function(data, index)
{
  sample = data[index, ]
  model = tree(formula = EX ~ MET,
               data = sample,
               control = tree.control(nrow(data.ordered), minsize = 8))
  model.pruned = prune.tree(model, best = best.size)
  prediction = predict(model.pruned, newdata = data.ordered)
  return (prediction)
}

boot.func.param = function(data)
{
  model = tree(formula = EX ~ MET,
               data = data,
               control = tree.control(nrow(data.ordered), minsize = 8))
  model.pruned = prune.tree(model, best = best.size)
  prediction = predict(model.pruned, newdata = data.ordered)
  return (prediction)
}

boot.rng = function(data, mle)
{
  new_data = data.frame(EX = data$EX, MET = data$MET)
  n = length(data$EX)
  new_data$EX = rnorm(n, predict(mle, newdata = new_data), sd(mle$residuals))
  return(new_data)
}

boot.nonparam = boot(data = data.ordered,
                     statistic = boot.func.nonparam,
                     R = 1000)
plot(boot.nonparam)

boot.param = boot(data = data.ordered,
                  statistic = boot.func.param,
                  R = 1000,
                  mle = model.optimal,
                  ran.gen = boot.rng,
                  sim = "parametric")


plot(boot.param)

summary(model.optimal)

# ******
# TODO: plot confidence bands ... ??
# ******

