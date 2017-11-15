library(kknn)

# load data
data = read.csv("spambase.csv", header = TRUE, sep = ",", quote = "\"",
         dec = ",", fill = TRUE, comment.char = "")

# split data in half for train/test portions, 
# selecting documents randomly 
n = dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))
train = data[id,]
test = data[-id,]

knearest=function(train_data, k, test_data) 
{
  # get number of data points in train & test data
  num_rows_train = dim(train_data)[1]
  num_rows_test = dim(test_data)[1]
  # get the classification column
  spam_column = dim(train_data)[2]
  # set up a vector for the classification probabilities
  probabilities = numeric(num_rows_test)
  # set up test and train matrices with the classification column removed
  x_matrix = as.matrix(train_data[, -spam_column])
  y_matrix = as.matrix(test_data[, -spam_column])

  # X-hat and Y-hat
  x_hat = x_matrix / matrix(sqrt(rowSums(x_matrix^2)), nrow=num_rows_train, ncol=spam_column-1)
  y_hat = y_matrix / matrix(sqrt(rowSums(y_matrix^2)), nrow=num_rows_test, ncol=spam_column-1)
  
  # the cosine similarities of Yn with Xn
  cosine_similarities = x_hat %*% t(y_hat)
  
  # the distances expressed as 1 - <the similarity>
  distances = 1 - cosine_similarities

  for (i in 1:num_rows_test) {
    # sort the distances for every instance in the test data 
    # and select the row names of 5 documents that are closest to i
    k_nearest_objects = names(sort(distances[, i])[1:k])
    
    # get the mean of spam classifications (values at 'spam_column') from the 
    # training data that corresponds to the row names in nearest_document
    probabilities[i] = mean(train_data[c(k_nearest_objects), spam_column])
  }
  return (probabilities)
}

get_confusion_matrix=function(classification_probabilities, test_data) {
  # round the classification probabilities to get predicted class
  predicted_classifications = round(classification_probabilities)
  # get the true classifications from the 'Spam' column of the test data 
  true_classifications = test_data$Spam
  # create the confusion matrix for the predicted and true classifications
  return (table(predicted_classifications, true_classifications))
}

# get the fraction of incorrect classifications (one minus sum of the diagonal over the total sum)
missclassification_rate=function(confusion_matrix) {
  return (1 - (sum(diag(confusion_matrix)) / sum(confusion_matrix)))
}

# get probabilites and confusion matrix for our knearest classifier using k=5
# classifying test data
probs_knearest5 = knearest(train, 5, test)
cm_knearest5 = get_confusion_matrix(probs_knearest5, test)
print("CM for our knearest classifier on test data using k=5:")
print(cm_knearest5)
cat("missclassification rate:", missclassification_rate(cm_knearest5), "\n")

# repeat process for k=1
# classifying test data
probs_knearest1 = knearest(train, 1, test) 
cm_knearest1 = get_confusion_matrix(probs_knearest1, test)
print("CM for our knearest classifier on test data using k=1:")
print(cm_knearest1)
cat("missclassification rate:", missclassification_rate(cm_knearest1), "\n")

# get probabilites and confusion matrix for our knearest classifier using k=5
# classifying train data
probs_knearest5_train = knearest(train, 5, train)
cm_knearest5_train = get_confusion_matrix(probs_knearest5_train, train)
print("CM for our knearest classifier on train data using k=5:")
print(cm_knearest5_train)
cat("missclassification rate:", missclassification_rate(cm_knearest5_train), "\n")

# repeat process for k=1
# classifying test data
probs_knearest1_train = knearest(train, 1, train)
cm_knearest1_train = get_confusion_matrix(probs_knearest1_train, train)
print("CM for our knearest classifier on train data using k=1:")
print(cm_knearest1_train)
cat("missclassification rate:", missclassification_rate(cm_knearest1_train), "\n")

# repeat again but using kknn classifier from the kknn package with k=5
# classifying test data
kknn5 = kknn(formula=Spam~., train=train, test=test, k=5)
probs_kknn5 = fitted.values(kknn5)
cm_kknn5 = get_confusion_matrix(probs_kknn5, test)
print("CM for kknn classifier using k=5:")
print(cm_kknn5)
cat("missclassification rate:", missclassification_rate(cm_kknn5), "\n")

# repeat with kknn using k=1
kknn1 = kknn(formula=Spam~., train=train, test=test, k=1)
probs_kknn1 = fitted.values(kknn1)
cm_kknn1 = get_confusion_matrix(probs_kknn1, test)
print("CM for kknn classifier using k=1:")
print(cm_kknn1)
cat("missclassification rate:", missclassification_rate(cm_kknn1), "\n")

# with sequence 'classify_seq' to make spam classifications instead of using round()
# each row in resulting matrix will correspond to classfications using a value from a sequence
classify_seq = seq(0.05, 0.95, by=0.05)
classes_by_seq_knearest5 = sapply(probs_knearest5, function(x) as.numeric(x > classify_seq))
classes_by_seq_kknn5 = sapply(probs_kknn5, function(x) as.numeric(x > classify_seq))

# TPR, the fraction of true positives over (true positives + false negatives)
TPR=function(predicted_classifications, true_classifications) {
  return (sum(predicted_classifications == 1 & true_classifications == 1) / 
            (sum(predicted_classifications == 1 & true_classifications == 1) +
             sum(predicted_classifications == 0 & true_classifications == 1)))
}

# FPR, the fraction of false positives over (false positives + true negatives) 
FPR=function(predicted_classifications, true_classifications) {
  return (sum(predicted_classifications == 1 & true_classifications == 0) / 
            (sum(predicted_classifications == 1 & true_classifications == 0) +
               sum(predicted_classifications == 0 & true_classifications == 0)))
}

# apply FPR and TPR functions to each row in matrix generated by classes
# using the different values from the sequence, using knearest model with k=5
FPR_knearest5 = apply(classes_by_seq_knearest5, 1, FPR, true_classifications=test$Spam)
TPR_knearest5 = apply(classes_by_seq_knearest5, 1, TPR, true_classifications=test$Spam)

# the same using the kknn model with k=5
FPR_kknn5 = apply(classes_by_seq_kknn5, 1, FPR, true_classifications=test$Spam)
TPR_kknn5 = apply(classes_by_seq_kknn5, 1, TPR, true_classifications=test$Spam)

plot(classify_seq, 
     main="ROC-curves for knearest and kknn models", 
     xlab="FPR", ylab="TPR", 
     xlim=c(0.05, 0.95), ylim=c(0.01, 1))
lines(FPR_knearest5, TPR_knearest5, col="Blue")
lines(FPR_kknn5, TPR_kknn5, col="Green")
abline(0, 1, col="Red")