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
              hidden=3,      
              linear.output=TRUE,
              likelihood=TRUE)

# plot neural network
plot(nn)

#nn in response
print(nn$response)

# Prediction using neural network
prediction = compute(nn,X_test)

#prediction in net.result
print(prediction$net.result)
