library(MASS)
library(glmnet)

# read data into dataframe
data = read.csv("tecator.csv", header = TRUE, sep = ",", quote = "\"",
                    dec = ".", fill = TRUE, comment.char = "")

# Question 1:
# Plot protein vs moisture fields
plot(data$Protein, data$Moisture, xlab="Protein", ylab="Moisture", main="Protein vs Moisture")

# Data seems to be correlated in a linear relationship, i.e. as values of one variable rises so does the other at 
# a similar ratio these data can therefore be described well by a linear model where Protein is predicting Moisture

# Question 2:
# A probabilistic model for predicting moisture (M-hat) would be a polynomial function of protein as:
# M-hat = w0 + w1x^1 + w2x^2 + ... + wix^i

# TODO: Why MSE instead of SSE? 
# MSE = SSE/n-m where n=sample size, m=number of parameters (sum(w0..wi)).
# and also:
# MSE = 1/n * sum((Yi - Y-hat)^2)
# MSE will be more dependent on the number of parameters used? 

# Question 3
# Divide data into 50/50 training and validation sets
set.seed(12345)
n = nrow(data)
id = sample(1:n, floor(n*0.5))
train = data[id,]
validation = data[-id,]

train_mse = numeric(6)
validation_mse = numeric(6)

# Fit a linear model using i = 0, 1, .. 6
for (i in 1:6)
{
  linear_model = lm(formula = Moisture ~ poly(Protein, i), data=train)
  validation_predictions = predict(linear_model, validation)
  train_predictions = predict(linear_model, train)
  validation_mse[i] = mean((validation$Moisture - validation_predictions)^2)
  train_mse[i] = mean((train$Moisture - train_predictions)^2)
}

# Plotting we see that i = 1 gives the lowest error in the validation data, while the error
# goes down as i increases in the training data. Since the model was fitted to the training data,
# more parameters will lead to lower error in the training data but will create an overfitted model.

i_values = c(1:6)
y_limits = c(min(train_mse, validation_mse), max(train_mse, validation_mse))
plot(i_values, main="MSE", ylim=y_limits, xlab="i", ylab="MSE")
lines(validation_mse, col="green")
lines(train_mse, col="red")
#text(locator(), labels = c("Validation MSE", "Train MSE"))

# Question 4:
# Perform variable selection of a linear model in which Fat is response and
# Channel1-Channel100 are predictors by using stepAIC.

# Use stepAIC to select features to be used in model
model = lm(Fat ~ . -Sample -Protein -Moisture, data=data)
step_aic = stepAIC(model, direction="both", trace=FALSE)
cat("number of features selected from model by stepAIC:", length(step_aic$coefficients))

# Question 5:
# Fit a Ridge regression model with the same predictor and response variables.

# Response is variable Fat, predictors are variables Channel1-Channel100
response = data$Fat 
predictors = as.matrix(data[,2:101])

# Fit ridge regression model
rr_model = glmnet(predictors, response, alpha = 0, family="gaussian")
# Plot showing how model coefficients depend on the log of the penalty factor lambda
plot(rr_model, main="Ridge regression model", xvar="lambda", label=TRUE)

# Question 6:
# Same as question 5 but using a LASSO regression model
lasso_model = glmnet(predictors, response, alpha = 1, family="gaussian")
# plot showing how model coefficients depend on the log of the penalty factor lambda
plot(lasso_model, main="LASSO regression model", xvar="lambda", label=TRUE)

# Question 7:
# Use cross-validation to find the optimal LASSO model
lasso_model_cv = cv.glmnet(predictors, response, alpha=1, family="gaussian")

# Report optimal lambda
min_lambda = lasso_model_cv$lambda.min
cat("optimal lambda in lasso cv model:", min_lambda)

# Report how many variables were chosen by the model
# Variables included in the model are those with coefficients != 0
coefficients = coef(lasso_model_cv, s="lambda.min")
num_variables = sum(coefficients != 0)
cat("number of variables included in lasso cv model:", num_variables)
