### MADE BY  ####
# SVR - Neural Network
#Made by:Paraskevi Sifnaiou
###############

####IMPORT DATA######
#Set environment
setwd("C:/Users/")
#Import the packages
library(fpp)
library(MASS)
library(readxl)
library(neuralnet)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(fpp2)
library(e1071)
library(openxlsx)
library(MLmetrics)
#Read the excel file
s <- read_excel("ExchangeUSD.xlsx")
str(s)

######STEP1: DATA COLLECTION#####
# Y1: 3 input vectors
#y_t
x<-s[,3]

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
all_norm1 <- as.data.frame(lapply(x, normalize))

write.xlsx(all_norm1,"ExchangeNorm.xlsx")

all_lag3 <- read_excel("ExchangeLag3.xlsx")
all_lag4 <- read_excel("ExchangeLag4.xlsx")


######## 1.1 MODEL - Y1 ,3 inputs ###########
#Fit the SVR model (is the SVM type: eps-regression)
#1)Normalize the data with function instead of scale()


######Training a SVR model with normalized data#######

set.seed(1234)
exchange_train1 <- all_lag3[1:400, ]
exchange_test1 <- all_lag3[401:497, ]

exchange_model1.1 <- svm( Output  ~ . ,data = exchange_train1 , type="eps-regression",kernel="linear", cost =2) 
#Predicting a new result
pred1.1<-predict(exchange_model1.1,exchange_test1) 

######BEST MODEL 1.1######

svm_tune <- tune(svm, Output ~ Input1+Input2+Input3 ,data = exchange_train1 ,ranges = list(epsilon = seq(0,1,0.01), cost = seq(1,5,1),scale=F))
print(svm_tune)

# extract the original (not normalized) training and testing desired Output
exchange_train_original_s1 <- x[1:400,"USD"]  # the first 400 rows
exchange_test_original_s1 <- x[401:497,"USD"] # the remainining rows

# and find its maximum & minimum value
s_min1 <- min(exchange_train_original_s1)
s_max1 <- max(exchange_train_original_s1)

# display its contents
head(exchange_test_original_s1)

#Create the reverse of normalised function - renormalized
unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}
#Now we renormalize the normalised NN's output
s_renorm_pred1.1 <- unnormalize(pred1.1, s_min1, s_max1)
s_renorm_pred1.1   # this is NN's output renormalized to original ranges
#SUMMARY for 1.1
summary(exchange_model1.1)

##########REAL VALUES-ERRORS 1.1 for Y1##############
##computing the error we will chek the error for SVM model(how close the predicted values are in contrast with actual values Y$x2)
#the best model

#Define RMSE function
RMSE(exp(s_renorm_pred1.1),exchange_test_original_s1)

#MSE
MSE(exp(s_renorm_pred1.1),exchange_test_original_s1)

#Define MAPE
MAPE(exp(s_renorm_pred1.1),exchange_test_original_s1)

# examine the correlation between predicted and actual values
cor(s_renorm_pred1.1,exchange_test_original_s1$USD)

#Fit a SVR model with normalized data
exchange_model1.2 <- svm( Output  ~ . ,data = exchange_test1 , gamma = 0.1, cost = 4, type="eps-regression",kernel="radial") 
#Predicting a new result
pred1.2<-predict(exchange_model1.2,exchange_test1) 
pred1.2


# extract the original (not normalized) training and testing desired Output
exchange_train_original_s1 <- x[1:400,"USD"]  # the first 400 rows
exchange_test_original_s1 <- x[401:497,"USD"] # the remainining rows

# and find its maximum & minimum value
s_min1 <- min(exchange_train_original_s1)
s_max1 <- max(exchange_train_original_s1)

# display its contents
head(exchange_test_original_s1)

######Renormalization##### 
s_renorm_pred1.2 <- unnormalize(pred1.2, s_min1, s_max1)
s_renorm_pred1.2   # output renormalized to original ranges

#SUMMARY for 1.2
summary(exchange_model1.2)

##########REAL VALUES-ERRORS 1.2(Y1,model 2)##############
##computing the error we will chek the error for SVM model(how close the predicted values are in contrast with actual values Y$x2)

#Define RMSE function
RMSE(exp(s_renorm_pred1.2),exchange_test_original_s1)

#MSE
MSE(exp(s_renorm_pred1.2),exchange_test_original_s1)

#Define MAPE
MAPE(exp(s_renorm_pred1.2),exchange_test_original_s1)

# examine the correlation between predicted and actual values
cor(s_renorm_pred1.2,exchange_test_original_s1)


plot(exchange_test_original_s1$USD , ylab = "Predicted vs Expected", type="l", col="red" )
par(new=TRUE)
plot(s_renorm_pred1.2, ylab = " ", yaxt="n", type="l", col="green" )
par(new=TRUE)
plot(s_renorm_pred1.1, ylab = " ", yaxt="n", type="l", col="blue" )


############################################################################
#################      MODEL Y2         #################
#################   t=4 - input vectors #################
############################################################################
####Step 1: Data collection#########

set.seed(1234)
exchange_train2 <- all_lag4[1:400, ]
exchange_test2 <- all_lag4[401:496, ]


######BEST MODEL 2.1######
svm_tune <- tune(svm, Output ~ Input1+Input2+Input3+Input4 ,data = exchange_train2 ,ranges = list(epsilon = seq(0,1,0.01), cost = seq(1,5,1)))
print(svm_tune)

exchange_model2.1 <- svm( Output  ~ . ,data = exchange_train1 , type="eps-regression",kernel="linear", cost =2) 
#Predicting a new result
pred2.1<-predict(exchange_model2.1,exchange_test2) 



# extract the original (not normalized) training and testing desired Output
exchange_train_original_s2 <- x[1:400,"USD"]  # the first 400 rows
exchange_test_original_s2 <- x[401:496,"USD"] # the remainining rows


#Train a SVR model with normalized data
# extract the original (not normalized) training and testing desired Output
exchange_train_original_s2 <- x[1:400,"USD"]  # the first 400 rows
exchange_test_original_s2 <- x[401:496,"USD"] # the remainining rows

#maximum & minimum value
s_min2 <- min(exchange_train_original_s2)
s_max2 <- max(exchange_train_original_s2)

# display its contents
head(exchange_test_original_s2)

#Create the reverse of normalised function - renormalized
unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}
######Renormalization##### 
s_renorm_pred2.1 <- unnormalize(pred2.1, s_min2, s_max2)
s_renorm_pred2.1
#SUMMARY for 2.1
summary(exchange_model2.1)

##########REAL VALUES-ERRORS 2.1(Y2,model 2)##############
##computing the error we will chek the error for SVM model(how close the predicted values are in contrast with actual values Y$x2)

#Define RMSE function
RMSE(exp(s_renorm_pred2.1),exchange_test_original_s2)

#MSE
MSE(exp(s_renorm_pred2.1),exchange_test_original_s2)

#Define MAPE
MAPE(exp(s_renorm_pred2.1),exchange_test_original_s2)

# examine the correlation between predicted and actual values
cor(s_renorm_pred2.1,exchange_test_original_s2)


###########2.2 change inputs of Y1#############
#Fit the SVR model (is the SVM type: eps-regression)

#Training a SVR model with normalized data
exchange_model2.2 <- svm( Output  ~ .,data = exchange_test2 , cost = 2^(2:9), type="eps-regression",kernel="radial",scale=F) 
#Predicting a new result
pred2.2<-predict(exchange_model2.2,exchange_test2) 
#pred2.2

# extract the original (not normalized) training and testing desired Output
exchange_train_original_s2 <- x[1:400,"USD"]  # the first 400 rows
exchange_test_original_s2 <- x[401:496,"USD"] # the remainining rows

# and find its maximum & minimum value
s_min2 <- min(exchange_train_original_s2)
s_max2 <- max(exchange_train_original_s2)

# display its contents
head(exchange_test_original_s2)

######Renormalization#####
s_renorm_pred2.2 <- unnormalize(pred2.2, s_min2, s_max2)
s_renorm_pred2.2   

#SUMMARY for 2.2
summary(exchange_model2.2)

##########REAL VALUES-ERRORS 2.2(Y2,model 2)##############
##computing the error we will chek the error for SVM model(how close the predicted values are in contrast with actual values Y$x2)

#Define RMSE function
RMSE(exp(s_renorm_pred2.2),exchange_test_original_s2)

#MSE
MSE(exp(s_renorm_pred2.2),exchange_test_original_s2)

#Define MAPE
MAPE(exp(s_renorm_pred2.2),exchange_test_original_s2)

# examine the correlation between predicted and actual values
cor(s_renorm_pred2.2,exchange_test_original_s2$USD)


