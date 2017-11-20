library(tree)

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

