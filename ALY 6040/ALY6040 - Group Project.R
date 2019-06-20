# ALY6015 - Group Project – Shen, Qiu,
# Group member: Danlin Shen, Limu Qiu, 

# Install & Load
install.packages("factoextra")
install.packages("NbClust")
install.packages("party")
install.packages("randomForest")
install.packages("dbscan")
install.packages("fpc")
install.packages("glmnet")
library("glmnet") 
library("party")
library("MASS")
library("ggplot2")

# Introduction of raw data:
# The raw data is collected by lianjia.com(the biggest housing agency in China). 
# All trade time was in January 2018. This data set describes several the relationships of factors and price 
# of the apartment.

# Factors:
# price: Housing price per squared meter(unit: ¥, RMB)
# square: The area of the apartment (unit: square meters).
# livingRoom: The number of living rooms.
# drawingRoom: The number of drawing rooms.
# kitchen: The number of kitchens.
# bathRoom: The number of bathrooms.
# floor: The number of the floor of the apartment.
# buildingType: including tower(1) , bungalow(2)，combination of plate and tower(3), and plate(4).
# constructionTime: the time of construction.
# renovationCondition: including other(1), rough(2),Simplicity(3), and hardcover(4)
# buildingStructure: including unknown(1), mixed(2), brick and wood(3), brick and concrete(4),steel(5) and steel-concrete composite (6).
# elevator: Is there an elevator?
# subway: Is there a subway nearby?
# district: District code of Beijing.

# Make sure the raw data and the program are in the same working direction.
# getwd()
# setwd("/Users/zack/Desktop/Code/R/ALY 6040")
bj.raw <- read.csv("bj.csv")
# bj.raw is the raw data.
bj.raw

plot_correlation(bj.raw)
plot_correlation(bj.norm)

####################################
# Data Preprocessing - Danlin Shen #
####################################
# 1. Convert all of the data: All of the matrix will be standardized to have unit L1 norm in each column and 
# zero mean. L1 norm is uesd for Lasso and descision tree.
bj.norm <- apply(bj.raw, 2, function(x) scale(x, center = T, scale = T)/norm(scale(x, center = T, scale = T), "1"))
bj.norm # bj.norm is the data have been standardized to have unit L1 norm in each column and zero mean.
plot_correlation(bj.norm)

# Exlanation & Testing
# Before we start analyzing the data, we have to preprocess the data -- if you don't, the data won't be in the 
# same range. For example, in this program, we can't directly compare the relationships in the numbers of the 
# bathroom, the price per squared meter, and the distance from the city center, because they have huge differences
# in position and scale that in the coordinate system. Therefore, through data preprocessing, we can put all of 
# the variables near the origin of the coordinate system without changing the data distribution. I will use 
# z-mean and L1 normalization. Step 1 is to move the data to the origin. Step 2 is to compress the range 
# of the data to [0, 1].
sub.o <- bj.raw$subway
sub.1<- sub.o - mean(sub.o) # Step 1
sub.2 <- (sub.o - mean(sub.o))/(sd(sub.o)) # Step 2
sub.2/norm(as.matrix(sub.2), "1")  # Step 2

# 2. Add the discription of the price
price <- bj.raw$price
summary(price) # Find out the minmum, the 1st quantile, the median, the mean, the 3rd quantile.
boxplot(price)

ggplot(data = bj.raw)+
  geom_boxplot(mapping = aes(y = price))
# We can notice that there are some points above the boxplot which are larger than the maximum of the 
# boxplot(about 90,000). Therefore, I set 90,000 as the luxury level.

# Description: Classify Beijing's house prices according to the price quartiles.
# Testing
for (i in 1:100) {
  if (price[i] < 42752) {cat(i, ":", price[i], "Poor","\n")}
  if (42752 <= price[i] & price[i]  < 54761) {cat(i, ":", price[i], "Cheap","\n")}
  if (54761 <= price[i] & price[i]  < 59666) {cat(i, ":", price[i], "Medium","\n")}
  if (59666 <= price[i] & price[i]  < 71180) {cat(i, ":", price[i], "Expensive","\n")}
  if (71180 <= price[i]) {cat(i, ":", price[i], "Luxary","\n")}
}

# The decision tree can only predict data using numerical data or categorical data. Essentially, 
# this is a data classification. Therefore categorical data must be used as the y value. Since the raw 
# data does not have a y value, I use a piece of code to generate the y value.
bj.tree <- bj.raw
for (i in 1:nrow(bj.tree)) {
  if (bj.tree$price[i] < 42752) {bj.tree$Description[i] <- "Poor"}
  if (42752 <= bj.tree$price[i] & price[i]  < 54761) {bj.tree$Description[i] <- "Cheap"}
  if (54761 <= bj.tree$price[i] & price[i]  < 59666) {bj.tree$Description[i] <- "Medium"}
  if (59666 <= bj.tree$price[i] & price[i]  < 71180) {bj.tree$Description[i] <- "Expensive"}
  if (71180 <= bj.tree$price[i]) {bj.tree$Description[i] <- "Luxury"}
}
bj.tree # bj.tree is the data that has extral label to dscribe the price


###############################
# Decision Tree - Danlin Shen #
###############################
# Generate Tree dataset
bj.norm <- as.data.frame(bj.norm)
Tree <- bj.norm
Tree$Description <- factor(bj.tree$Description) # Convert the Description from character to factor.

# split into training and test bj.raws
set.seed(100) # set.seed(): Generate random numbers in a specific order for simple verification.
Tree.index <- sample(2, nrow(Tree), replace=TRUE, prob=c(0.7, 0.3))
Tree.train <- Tree[Tree.index==1, ]
Tree.test <- Tree[Tree.index==2, ]
str(Tree.train)
str(Tree.test)

# build a decision tree
Tree.formula <- Description ~ subway + district + square + livingRoom + drawingRoom + kitchen + bathRoom
Tree.ctree <- ctree(Tree.formula, data=Tree.train)
plot(Tree.ctree)

# predict on test data
pred <- predict(Tree.ctree, newdata = Tree.test) # check prediction result
table(pred, Tree.test$Description)
Tree.test
# Conclusion: From the statistical results, using the subway, district, square, livingRoom, drawingRoom, kitchen, 
# and bathRoom could predict the range of the bj price. Therefore, if new data has the same characteristics
# as the decision tree model, the category can be predicted.


######################
# Lasso - Gugu Xiong #
######################
bj.raw <- read.table("/Users/guxiongxiong/Desktop/BeijingPrice.csv", head=T,sep=",") # The working diriction maybe changed
head(bj.raw) 

bj.norm <- apply(bj.raw, 2, function(x) scale(x, center = T, scale = T)/norm(scale(x, center = T, scale = T), "1"))
head(bj.norm) 

pairs(bj.norm)

bj.norm <- as.data.frame(bj.norm)
class(bj.norm)
bj.norm
model_OLS <- lm(price~subway+district+square+livingRoom+drawingRoom+kitchen+bathRoom, data=bj.norm)
summary(model_OLS)
par(mfrow=c(2,2))
plot(model_OLS)

#Extraction regression coefficient:
coef.ols <- coef(model_OLS)

#Finding regression coefficients not equal to 0:
coef.ols[coef.ols!=0]

##ggcorrplot
corr_bj <- round(cor(bj.norm),1)
head(corr_bj[,1:7])
install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(corr_bj)

##first regularization
model_OLS1 <- lm(price~subway+drawingRoom+kitchen+bathRoom, data=bj.norm)
summary(model_OLS1)
###do not work well


##When we try to figure out the coefficients that are not equal to 0, all the factors still exist.

##Lasso
install.packages("glmnet")
library("glmnet") 

lasso.1 <- glmnet(x = as.matrix(bj.norm[1:400,2:8]), y = as.matrix(bj.norm[1:400,1]),alpha=1)
plot.glmnet(lasso.1, xvar = "norm", label = T)

##We use the cv.glmnet function to get the cross-validation curve and the lambda value.

cvfit <- cv.glmnet(x=as.matrix(bj.norm[1:400,2:8]), y=as.matrix(bj.norm[1:400,1]), alpha=1, nlambda=1000)
plot.cv.glmnet(cvfit)

##To gain the value of lambda that minimizes the mean cross-validation error.

cvfit$lambda.min

###fit-min


fit1 <- glmnet(x=as.matrix(bj.norm[1:400,2:8]), y=as.matrix(bj.norm[1:400,1]),alpha = 1, lambda=cvfit$lambda.min)
fit1$beta

fit2 <- glmnet(x=as.matrix(bj.norm[1:400,2:8]), y=as.matrix(bj.norm[1:400,1]),alpha = 1, lambda=cvfit$lambda.1se)
fit2$beta

model_OLS2 <- lm(price~subway+kitchen+bathRoom, data=bj.norm)
summary(model_OLS2)