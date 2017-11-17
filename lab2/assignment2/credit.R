library(tree)

data = read.csv("creditscoring.csv", header = TRUE, sep = ",", quote = "\"",
                dec = ".", fill = TRUE, comment.char = "")

# data split into training/validation/test as 50/25/25

# use half data for training
n = length(data[, 1])
set.seed(12345)
id = sample(1:n, floor(n*0.5))
train = data[id,]
remainder = data[-id,]

# split the remaining data into validation/test
n = length(remainder[, 1])
id = sample(1:n, floor(n*0.5))
validation = remainder[id, ]
test = remainder[-id, ]

model.deviance = tree(formula = good_bad ~ ., 
                      data = train,
                      split = "deviance")
model.gini = tree(formula = good_bad ~ ., 
                  data = train,
                  split = "gini")

# par(mfrow = c(2,1)) # 2 row 1 columns for next plots
# plot(model.deviance)
# text(model.deviance)
# plot(model.gini)
# text(model.gini)

get_misclassification=function(model, data)
{
  prediction = predict(model, newdata=data, type="class")
  cm = table(prediction, data$good_bad)
  misclassification_rate = (cm[1,2] + cm[2,1]) / sum(cm)
  return (misclassification_rate)
}

cat("misclassification using 'deviance' for test data: ",
    get_misclassification(model.deviance, test),
    "\nmisclassification using 'deviance' for train data: ", 
    get_misclassification(model.deviance, train),
    "\nmisclassification using 'gini' for test data: ",
    get_misclassification(model.gini, test),
    "\nmisclassification using 'gini' for train data: ", 
    get_misclassification(model.gini, train))

# deviance provides lower misclassification rate for both tests


    