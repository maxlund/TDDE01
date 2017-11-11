# read data into dataframe
data = read.csv("tecator.csv", header = TRUE, sep = ",", quote = "\"",
                    dec = ".", fill = TRUE, comment.char = "")

# plot protein vs moisture fields
plot(data$Protein, data$Moisture, xlab="Protein", ylab="Moisture", main="Protein vs Moisture")

# data seems to be correlated in a linear relationship, i.e. as values of one variable rises so does the other at a similar ratio
# these data can therefore be described well by a linear model

# a probabilistic model for predicting moisture (M-hat) would therefore be a polynomial function of protein as:
# M-hat = w0 + w1x^1 + w2x^2 + ... + wix^i

# TODO: Why MSE instead of SSE? MSE = SSE/n-m where n=sample size, m=number of parameters (sum(w0..wi)).
# MSE will be more dependent on the number of parameters used? 

# divide data into 50/50 training and validation sets
set.seed(12345)
n = nrow(data)
id = sample(1:n, floor(n*0.5))
train = data[id,]
validation = data[-id,]

train_mse = numeric(6)
validation_mse = numeric(6)
# fit a linear model using i = 0, 1, .. 6
for (i in 1:6)
{
  linear_model = lm(formula = Moisture ~ poly(Protein, i), data=train)
  validation_predictions = predict(linear_model, validation)
  train_predictions = predict(linear_model, train)
  
  validation_mse[i] = mean( (validation$Moisture - validation_predictions)^2 )
  train_mse[i] = mean( (train$Moisture - train_predictions)^2 )
}

# plotting we see that i = 1 gives the lowest error in the validation data, while the error
# goes down as i increases in the training data. since the model was fitted to the training data,
# more parameters will lead to lower error in the training data but will create an overfitted model

i_values = c(1:6)
y_limits = c(min(train_mse, validation_mse), max(train_mse, validation_mse))
plot(i_values, main="MSE", ylim=y_limits, xlab="i", ylab="MSE")
lines(validation_mse, col="green")
lines(train_mse, col="red")
text(locator(), labels = c("Validation MSE", "Train MSE"))