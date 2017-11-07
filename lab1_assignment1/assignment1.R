data = spambase

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
  probabilies = numeric(num_rows_test)
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
    nearest_documents = names(sort(distances[, i])[1:k])
    
    # get the mean of spam classifications (values at 'spam_column') from the 
    # training data that corresponds to the row names in nearest_document
    probabilies[i] = mean(train_data[c(nearest_documents), spam_column])
  }
}

knearest(train, 5, test)

# ROC=function(Y, Yfit, p) {
#   m = length(p)
#   TPR = numeric(m)
#   FPR = numeric(m)
#   for (i in 1:m) {
#     t = table(Yfit>p[i], Y)
#     TPR[i] = #insert formula for TPR
#     FPR[i] = #insert formula for FPR
#   }
#   return (list(TPR=TPR,FPR=FPR))
# }