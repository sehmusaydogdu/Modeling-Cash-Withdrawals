#install package
#install.packages("neuralnet")

#import neuralnet library
library("neuralnet")

# load library
require(neuralnet)

#Read file training and test dataset
X_train <- read.csv("training_data.csv", header = TRUE)
X_test <- read.csv("test_data.csv", header = TRUE)

# fit neural network
# TRX_COUNT ~ REGION + DAY + MONTH + YEAR + TRX_TYPE,
# (TRX_COUNT) is label and (REGION, DAY, MONTH, YEAR, TRX_TYPE) are features.
nn = neuralnet(TRX_COUNT ~ REGION + DAY + MONTH + YEAR + TRX_TYPE,
              data=X_train,
              hidden=5,      
              linear.output=TRUE,
              likelihood=TRUE)

# plot neural network
plot(nn , sub = "Neural Network", outer = FALSE)


#nn print console in response
print(nn$response)

#train_scores using neural network
train_scores = compute(nn,X_train)

#train_scores in net.result
print(train_scores$net.result)



#prediction_scores using neural network
prediction_scores = compute(nn,X_test)

#prediction_scores in net.result
print(prediction_scores$net.result)

#prediction_scores write a file "test_predictions.csv"
write.table(prediction_scores$net.result, file = "test_predictions.csv", row.names = FALSE, col.names = FALSE)



# mean absolute error for training data
mean_absoulte_error <- mean(abs(train_scores$net.result - X_train$TRX_COUNT))
print(sprintf("MAE mean absolute error = %g", mean_absoulte_error))

# root mean squared error for training data
rmse <- sqrt(mean((train_scores$net.result - X_train$TRX_COUNT)^2))
print(sprintf("RMSE root mean squared error = %g", rmse))
