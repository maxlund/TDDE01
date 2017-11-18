library(tree)
library(e1071)

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
  cm = get_confusion_matrix(model, data)
  misclass_rate = (cm[1,2] + cm[2,1]) / sum(cm)
  return (misclass_rate)
}

get_confusion_matrix=function(model, data)
{
  prediction = predict(model, 
                       newdata = data, 
                       type = "class")
  cm = table(prediction, data$good_bad)
  return (cm)
}

cat("\nmisclass rate using 'deviance' for train data: ", 
    get_misclass_rate(model.deviance, train),
    "\nmisclass rate using 'deviance' for test data: ",
    get_misclass_rate(model.deviance, test),
    "\nmisclass rate using 'gini' for train data: ", 
    get_misclass_rate(model.gini, train),
    "\nmisclass rate using 'gini' for test data: ",
    get_misclass_rate(model.gini, test))

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
summary(optimal.tree)
# Variables actually used in tree construction:
# "savings"  "duration" "history" 

# print the optimal tree
# depth = 3 (if root depth = 0)
plot(optimal.tree)
text(optimal.tree)

# get misclassification rate for optimal tree on test data
optimal.misclass = get_misclass_rate(optimal.tree, test)
cat("\nmisclass rate of optimal tree:", optimal.misclass)

# fit a naive bayes model to train data
model.bayes = naiveBayes(formula = good_bad ~ ., 
                         data = train)

# misclass-rate is higher for train than test..
cat("\nmisclass rate for naive bayes model on train data:",
    get_misclass_rate(model.bayes, train),
    "\nmisclass rate for naive bayes model on test data:",
    get_misclass_rate(model.bayes, test))

cat("\n\nCM for naiveBayes (train):")
print(get_confusion_matrix(model.bayes, train))
cat("\nCM for naiveBayes (test):")
print(get_confusion_matrix(model.bayes, test))

# get raw probabilities for both classes from naiveBayes classifier
raw.train = predict(model.bayes, newdata = train, type = "raw")
raw.test = predict(model.bayes, newdata = test, type = "raw")

# predicting 'good' must be 10x more probable than bad
# to make the prediction 'good' with new loss-matrix 
preds.train = (raw.train[, 2] / raw.train[, 1]) > 10
preds.test = (raw.test[, 2] / raw.test[, 1]) > 10

# convert booleans to good/bad labels
preds.train[which(preds.train == TRUE)] = "good"
preds.train[which(preds.train == FALSE)] = "bad"
preds.test[which(preds.test == TRUE)] = "good"
preds.test[which(preds.test == FALSE)] = "bad"

# CM for train and test
cm.train = table(preds.train, train$good_bad)
cm.test = table(preds.test, test$good_bad)

cat("\nCM for naiveBayes + new loss matrix (train):")
print(cm.train)
cat("\nCM for naiveBayes + new loss matrix (test):")
print(cm.test)

# misclass-rate for train and test
misclass.train = (cm.train[1,2] + cm.train[2,1]) / sum(cm.train)
misclass.test = (cm.test[1,2] + cm.test[2,1]) / sum(cm.test)

cat("\nmisclass rate naiveBayes + new loss matrix (train data):",
    misclass.train,
    "\nmisclass rate naiveBayes + new loss matrix (test data):",
    misclass.test)