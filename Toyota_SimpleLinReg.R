
##----------------------------------------------------------------------------------##
##--- SIMPLE LINEAR REGRESSION MODEL - ACTIVITY-------------------------------------##
##----------------------------------------------------------------------------------##

# steps for modelling
# 
# 5) split the data into test(client data), trained, validation(check the model performing)
# 
# 1) Descriptive analysis
# 2) type conversion
# 3) NA value analysis
# 4) outliers



##--- Step 1: Clear environment variables ------------------------------------------##
rm(list=ls(all=TRUE))
##__________________________________________________________________________________##


##--- Step 2: Set working Directory ------------------------------------------------##

setwd("C:\\personal\\Insofe\\Data\\Week 6-1\\20180505_Batch42_CSE7302c_Lab01_SimpleLinearRegression")

##__________________________________________________________________________________##


##--- Step 3: Read the data from the csv file --------------------------------------##

cars_data = read.csv(file="Toyota_SimpleReg.csv", header=T)

names(cars_data)

str(cars_data)

summary(cars_data)

##__________________________________________________________________________________##


##--- Step 4: Perform Exploratory Data Analysis and Data Pre-processing-------------##
## Drop the Id, Model attributes:

cars_data = cars_data[,-c(1,2)]

## Summary of the data and look for any missing values:
str(cars_data)

summary(cars_data)
#No missing values

sum(is.na(cars_data))

2## Correlation and Covariance between the attributes:
cov(cars_data)

#The covariance of the Age of car and Price is -59136.11. 
#It indicates a negative linear relationship between the two variables. 
#This relation could be observed from the scatter plot also.
plot(cars_data$Age_06_15, cars_data$Price)

plot(cars_data$Age_06_15, cars_data$Price, xlab = "Age of the car", ylab = "Price in ($)", pch=18,col = "blue")

cor(cars_data)

cor(cars_data$Age_06_15, cars_data$Price)
#The correlation coefficient of the Age of car and Price is -0.8765905. 
#Since the value is close to 1 and has a -ve sign, we can conclude that the variables are strongly negatively correlated.
##__________________________________________________________________________________##


##--- Step 5: Split the data into train and test datasets --------------------------##
#Split in (train:test) in (70:30) ratio
rows = seq(1, nrow(cars_data),1)
set.seed(123)
trainRows = sample(rows,(70*nrow(cars_data))/100)

cars_train = cars_data[trainRows,] 
cars_test = cars_data[-trainRows,]
##__________________________________________________________________________________##


##--- Step 6: Linear regression model building--------------------------------------##
LinReg = lm(Price ~ Age_06_15, data = cars_train)

coefficients(LinReg)

## Summary of model:
summary(LinReg)

#Optional for info: 
#To extract the coefficients:
coefficients(LinReg)
coefficients(LinReg)[1]
coefficients(LinReg)[2]
names(coefficients(LinReg))
#To extract the residuals:
LinReg$residuals
#To extract the train predictions:
LinReg$fitted.values
##__________________________________________________________________________________##


##--- Step 7: Check for validity of linear regression assumptions ------------------##
par(mfrow = c(2,2))
plot(LinReg)
par(mfrow = c(1,1))
##__________________________________________________________________________________##


##--- Step 8: Predict on testdata --------------------------------------------------##
test_prediction = predict(LinReg, cars_test)

test_actual = cars_test$Price
##__________________________________________________________________________________##


##--- Step 9: Error Metrics --------------------------------------------------------##
library(DMwR)
#Error verification on train data
regr.eval(cars_train$Price, LinReg$fitted.values)

#Error verification on test data
regr.eval(test_actual, test_prediction)
##__________________________________________________________________________________##


##--- Step 10: Confidence and Prediction Intervals----------------------------------##
# Confidence Intervals talk about the average values intervals
# Prediction Intervals talk about the all individual values intervals

Conf_Pred = data.frame(predict(LinReg, cars_test, interval="confidence",level=0.80))
Pred_Pred = data.frame(predict(LinReg, cars_test, interval="prediction",level=0.80))

names(Conf_Pred)

plot(cars_test$Age_06_15, cars_test$Price, xlab = "Age of the car", ylab = "Price in ($)")

points(cars_test$Age_06_15,Conf_Pred$fit,type="l", col="green", lwd=2)
points(cars_test$Age_06_15,Conf_Pred$lwr,pch="-", col="red", lwd=4)
points(cars_test$Age_06_15,Conf_Pred$upr,pch="-", col="red", lwd=4)
points(cars_test$Age_06_15,Pred_Pred$lwr,pch="-", col="blue", lwd=4)
points(cars_test$Age_06_15,Pred_Pred$upr,pch="-", col="blue", lwd=4)
##__________________________________________________________________________________##
#-----------------------end---------------------------------------------------------##
## Confidence intervals tell you about how well you have determined the mean. Assume
## that the data really are randomly sampled from a Gaussian distribution. If you do 
## this many times, and calculate a confidence interval of the mean from each sample, 
## you'd expect about 95 % of those intervals to include the true value of the 
## population mean. The key point is that the confidence interval tells you about the
## likely location of the true population parameter.

## Prediction intervals tell you where you can expect to see the next data point sampled.
## Assume that the data really are randomly sampled from a Gaussian distribution. Collect
## a sample of data and calculate a prediction interval. Then sample one more value from
## the population. If you do this many times, you'd expect that next value to lie within 
## that prediction interval in 95% of the samples.The key point is that the prediction 
## interval tells you about the distribution of values, not the uncertainty in determining 
## the population mean.




