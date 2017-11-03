knearest=function(data,k,newdata) {
  
  n1=dim(data)[1]
  n2=dim(newdata)[1]
  p=dim(data)[2]
  Prob=numeric(n2)
  X=as.matrix(data[,-p])
  print(dim(X))
  Y=as.matrix(newdata[-p])
  print(dim(Y))
  
  # X-hat and Y-hat
  Xn=X/matrix(sqrt(rowSums(X^2)), nrow=n1, ncol=p-1)
  Yn=Y/matrix(sqrt(rowSums(Y^2)), nrow=n1, ncol=p-1)
  
  # the cosine similarities of Yn with Xn
  C = Xn %*% t(Yn)
  
  # the distances expressed as 1 - <the similarity>
  D = 1 - C
  
  #print(data)
  # 
  # for (i in 1:n2 ){
  #   dist = sort(D[i,])[1:5]
  #   
  #   distindex = as.numeric(names(dist))
  #   print(distindex)
  #   classes = data[distindex,p]
  #   print(classes)
  
  #MISSING: use the computed distance matrix to find 
  #which observations are the nearest neighbors to case #i
  #MISSING: derive probability value 'Prob[i]' by using the
  #target values of the nearest neighbors
#}
#  return(Prob)
}



# ROC=function(Y, Yfit, p){
#   m=length(p)
#   TPR=numeric(m)
#   FPR=numeric(m)
#   for(i in 1:m){
#     t=table(Yfit>p[i], Y)
#     TPR[i]=#insert formula for TPR
#     FPR[i]=#insert formula for FPR
#   }
#   return (list(TPR=TPR,FPR=FPR))
# }

data = spambase

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

knearest(train,5,test)
