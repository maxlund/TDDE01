library(tree)

data = read.csv("creditscoring.csv", header = TRUE, 
                sep = ",", quote = "\"", dec = ".", 
                fill = TRUE, comment.char = "")

# data split into training/validation/test as 50/25/25

# use half data for training
n = length(data[, 1])
set.seed(12345)
id = sample(1:n, floor(n * 0.5))
train = data[id, ]
remainder = data[-id, ]

# split the remaining data into validation/test
n = length(remainder[, 1])
id = sample(1:n, floor(n * 0.5))
validation = remainder[id, ]
test = remainder[-id, ]

# fit one model using deviance (cross-entropy)
model.deviance = tree(formula = good_bad ~ ., 
                      data = train,
                      split = "deviance")

# and another using gini-index
model.gini = tree(formula = good_bad ~ ., 
                  data = train,
                  split = "gini")

get_misclass_rate=function(model, data)
{
  prediction = predict(model, 
                       newdata = data, 
                       type = "class")
  cm = table(prediction, data$good_bad)
  misclass_rate = (cm[1,2] + cm[2,1]) / sum(cm)
  return (misclass_rate)
}

cat("misclassification using 'deviance' for test data: ",
    get_misclass_rate(model.deviance, test),
    "\nmisclassification using 'deviance' for train data: ", 
    get_misclass_rate(model.deviance, train),
    "\nmisclassification using 'gini' for test data: ",
    get_misclass_rate(model.gini, test),
    "\nmisclassification using 'gini' for train data: ", 
    get_misclass_rate(model.gini, train))

# deviance provides lower misclassification rate for both tests
# chose model.deviance for pruning

train_score = numeric(9)
validation_score = numeric(9)

# try number of terminal nodes in range 2-9 to find
# optimal depth, i.e tree with lowest deviance
for (i in 2:9)
{
  pruned_tree = prune.tree(model.deviance, best = i)
  prediction = predict(pruned_tree, 
                       newdata = validation, 
                       type = "tree")
  train_score[i] = deviance(pruned_tree)
  validation_score[i] = deviance(prediction)
}

plot(2:9, train_score[2:9], 
     type="b", col="red", ylim=c(250, 590),
     xlab="Number of leaves", ylab="Deviance")

points(2:9, validation_score[2:9], 
       type="b", col="blue")

# since i = 2..9, optimal i is equal to index + 1
optimal.leaves = which.min(validation_score[2:9]) + 1
# prune tree to optimal number of leaves
optimal.tree = prune.tree(model.deviance, 
                          best = optimal_leaves)
# get variables selected of optimal tree
# = savings, duration, history
optimal.variables = subset(optimal.tree$frame, 
                           var != "<leaf>", 
                           select = var)
# print the optimal tree
plot(optimal.tree)
text(optimal.tree)

# get misclassification rate for optimal tree on test data
optimal.misclass = get_misclass_rate(optimal.tree, test)
cat("misclassification of optimal tree:", optimal.misclass)



    