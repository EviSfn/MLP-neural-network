### MADE BY  ####
#3rd Objective: SVR
#Made by:Paraskevi Sifnaiou
############

####IMPORT DATA######
#Set environment
setwd("C:/Users/sifne/Desktop/data_mining_CW1_w1707896")
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
x<-s[,3]

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
all_norm1 <- as.data.frame(lapply(x, normalize))
summary(all_norm1)

#write.xlsx(all_norm1,"ExchangeNorm.xlsx")

all_lag3 <- read_excel("ExchangeLag3.xlsx")
summary(all_lag3)
all_lag4 <- read_excel("ExchangeLag4.xlsx")
summary(all_lag4)

#######Step 3: Training a model on the data#########
#Training a nn model
set.seed(1234)
exchange_train1 <- all_lag3[1:400, ]
exchange_test1 <- all_lag3[401:497, ]
exchange_model1.1 <- neuralnet(Output  ~ .  ,hidden=c(3,3) , data = exchange_train1 ) 
plot(exchange_model1.1)

########Step 4: Evaluation model performance############
model_results1.1 <- predict(exchange_model1.1, exchange_test1[1:3])
model_results1.1
#predicted_s1.1

# extract the original (not normalized) training and testing desired Output
exchange_train_original_s1 <- x[1:400,"USD"]  # the first 400 rows
exchange_test_original_s1 <- x[401:497,"USD"] # the remainining rows

# and find its maximum & minimum value
s_min1 <- min(exchange_train_original_s1)
s_max1 <- max(exchange_train_original_s1)

# display its contents
head(exchange_train_original_s1)

#Create the reverse of normalised function - renormalized
unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}
#Now we renormalize the normalised NN's output
s_renorm_pred1.1 <- unnormalize(model_results1.1, s_min1, s_max1)
s_renorm_pred1.1   # this is NN's output renormalized to original ranges

##########REAL VALUES-TESTS 1.1(Y1,model 1)##############
#Define RMSE function
RMSE(exp(s_renorm_pred1.1),exchange_test_original_s1$USD)

#MSE
MSE(exp(s_renorm_pred1.1),exchange_test_original_s1$USD)

#Define MAPE
MAPE(exp(s_renorm_pred1.1),exchange_test_original_s1$USD)

# examine the correlation between predicted and actual values
cor(s_renorm_pred1.1,exchange_test_original_s1$USD)

#Plot for exchange_model
par(mfrow=c(1,1))
plot(exchange_test_original_s1$USD, s_renorm_pred1.1 ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN', pch=18,col='red', bty='n')

final_result1.1 <- cbind(exchange_test_original_s1, s_renorm_pred1.1)
final_result1.1


#Step 5:1.2 Changing model Performance hidden=1,nodes=3 and tanh act.function##########



##input change: hidden layers ##
set.seed(1234)
exchange_model1.2 <- neuralnet( Output ~ ., data = exchange_train1, hidden = c(1,3), act.fct='tanh') 
model_results1.2 <- predict(exchange_model1.1, exchange_test1[1:3])
plot(exchange_model1.2)

#Renormalized to original ranges
s_renorm_pred1.2 <- unnormalize(model_results1.2, s_min1, s_max1)
head(s_renorm_pred1.2)  
##########REAL VALUES_TESTS 1.2(Y1,model 2)########

#Define RMSE function
RMSE(exp(s_renorm_pred1.2),exchange_test_original_s1$USD)

#MSE
MSE(exp(s_renorm_pred1.2),exchange_test_original_s1$USD)

#Define MAPE
MAPE(exp(s_renorm_pred1.2),exchange_test_original_s1$USD)

# examine the correlation between predicted and actual values
cor(s_renorm_pred1.2,exchange_test_original_s1$USD)

#Plot
par(mfrow=c(1,1))
plot(exchange_test_original_s1$USD, s_renorm_pred1.2 ,col='blue',main='Real vs predicted NN 2',pch=18,cex=0.7)
points(exchange_test_original_s1$USD,s_renorm_pred1.1,col='red',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN', pch=18,col='red', bty='n')
final_result1.2 <- cbind(exchange_test_original_s1, s_renorm_pred1.2)
final_result1.2


############################################################################
#################      MODEL Y2         #################
#################   t=4 - input vectors #################
############################################################################
####Step 1: Data colection#########
#I already have the first three
set.seed(1234)
exchange_train2 <- all_lag4[1:400, ]
exchange_test2 <- all_lag4[401:496, ]

##############Step 3: Training a model on the data#########
#Training a nn model

exchange_model2.1 <- neuralnet( Output~. , data = exchange_train2,hidden=c(3,3) ) 
plot(exchange_model2.1)

############Step 4: Evaluation model performance############
model_results2.1 <- predict(exchange_model2.1, exchange_test2[1:4])
#model_results2.1

# extract the original (not normalized) training and testing desired Output
exchange_train_original_s2 <- x[1:400,"USD"]  # the first 400 rows
exchange_test_original_s2 <- x[401:496,"USD"] # the remainining rows

# and find its maximum & minimum value
s_min2 <- min(exchange_train_original_s2)
s_max2 <- max(exchange_train_original_s2)

# display its contents
head(exchange_train_original_s2)

#Now we call the function to renormalize the normalised NN's output
s_renorm_pred2.1 <- unnormalize(model_results2.1, s_min2, s_max2)
s_renorm_pred2.1   # this is NN's output renormalized to original ranges

######################REAL VALUES-TESTS 2.1(Y2,model 1)#######################
#Define RMSE function
RMSE(exp(s_renorm_pred2.1),exchange_test_original_s2$USD)

#MSE
MSE(exp(s_renorm_pred2.1),exchange_test_original_s2$USD)

#Define MAPE
MAPE(exp(s_renorm_pred2.1),exchange_test_original_s2$USD)

# examine the correlation between predicted and actual values
cor(s_renorm_pred2.1,exchange_test_original_s2$USD)

#Plot for exchange_model
par(mfrow=c(1,1))
plot(exchange_test_original_s2$USD, s_renorm_pred2.1 ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN', pch=18,col='red', bty='n')
final_result2.1 <- cbind(exchange_test_original_s2, s_renorm_pred2.1)
final_result2.1

#Step 5: Changing model Performance hidden layers=3 and logistic act.function##########
#######input change ########
set.seed(1234)
exchange_model2.2 <- neuralnet( Output~., data = exchange_train2, hidden = 3,algorithm = "rprop-", act.fct='logistic') 
model_results2.2 <- predict(exchange_model2.2, exchange_test2[1:4])

plot(exchange_model2.2)

# this is NN's output renormalized to original ranges
s_renorm_pred2.2 <- unnormalize(model_results2.2, s_min2, s_max2)
s_renorm_pred2.2  

########REAL VALUES TESTS 2.2(Y2, model 2)###########
#Define RMSE function
RMSE(exp(s_renorm_pred2.2),exchange_test_original_s2$USD)

#MSE
MSE(exp(s_renorm_pred2.2),exchange_test_original_s2$USD)

#Define MAPE
MAPE(exp(s_renorm_pred2.2),exchange_test_original_s2$USD)

# examine the correlation between predicted and actual values
cor(s_renorm_pred2.2,exchange_test_original_s2$USD)

#Plot
par(mfrow=c(1,1))
plot(exchange_test_original_s2$USD, s_renorm_pred2.2 ,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN', pch=18,col='red', bty='n')
final_result2.2 <- cbind(exchange_test_original_s2, s_renorm_pred2.2)
final_result2.2

plot(exchange_test_original_s1$USD , ylab = "Predicted vs Expected", type="l", col="red" )
par(new=TRUE)
plot(s_renorm_pred1.2, ylab = " ", yaxt="n", type="l", col="green" )
