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
  
  n1 = dim(train_data)[1]
  n2 = dim(test_data)[1]
  p = dim(train_data)[2]
  Prob = numeric(n2)
  X = as.matrix(train_data[,-p])
  Y = as.matrix(test_data[,-p])
  
#  print(row.names(train[c(1, 2, 3), ]))
#  print(train_data[c("10", "12", "126"), ])
#  print(test_data[c("2731", "2527", "1753"), ])
  
  # X-hat and Y-hat
  Xn = X / matrix(sqrt(rowSums(X^2)), nrow=n1, ncol=p-1)
  Yn = Y / matrix(sqrt(rowSums(Y^2)), nrow=n1, ncol=p-1)
  
  # the cosine similarities of Yn with Xn
  C = Xn %*% t(Yn)
  
  # the distances expressed as 1 - <the similarity>
  D = 1 - C

  #for (i in 1:n2)
  for (i in 1:n2) {
    # sort the distances for every instance in the test data 
    # and select the row names of 5 documents that are closest to i
    nearest_documents = names(sort(D[, i])[1:k])
    
    # get the spam classification (column number 'p') from the training data 
    # that corresponds to the row names in nearest_document
    
    #print(train_data[c(nearest_documents), p])
    if (mean(train_data[c(nearest_documents), p]) > 0.5) {
      Prob[i] = 1
    } else {
      Prob[i] = 0
    }
    
  }

  for (i in 35:45)
  {
    print("test data true classification:")
    print(test[i, p])
    print("predicted classification:")
    print(Prob[i])
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