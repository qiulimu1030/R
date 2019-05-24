# 升级R Core
install.packages("devtools") #assuming it is not already installed
library(devtools)
install_github("andreacirilloac/updateR")
library(updateR)
updateR(admin_password = "Your Password")
version

# 添加Label
# Testing
for (i in 1:100) {
  if (price[i] < 27806) {cat(i, ":", price[i], "Poor","\n")}
  if (27806 <= price[i] & price[i]  < 38292) {cat(i, ":", price[i], "Cheap","\n")}
  if (38292 <= price[i] & price[i]  < 54240) {cat(i, ":", price[i], "Medium","\n")}
  if (54240 <= price[i] & price[i]  < 90000) {cat(i, ":", price[i], "Expensive","\n")}
  if (90000 <= price[i]) {cat(i, ":", price[i], "Luxary","\n")}
}

# The decision tree can only predict data using numerical data or categorical data. Essentially, 
# this is a data classification. Therefore categorical data must be used as the y value. Since the original 
# data does not have a y value, I use a piece of code to generate the y value.
Housing.tree <- Housing.original
for (i in 1:nrow(Housing.tree)) {
  if (Housing.tree$price[i] < 27806) {Housing.tree$Description[i] <- "Poor"}
  if (27806 <= Housing.tree$price[i] & price[i]  < 38292) {Housing.tree$Description[i] <- "Cheap"}
  if (38292 <= Housing.tree$price[i] & price[i]  < 54240) {Housing.tree$Description[i] <- "Medium"}
  if (54240 <= Housing.tree$price[i] & price[i]  < 90000) {Housing.tree$Description[i] <- "Expensive"}
  if (90000 <= Housing.tree$price[i]) {Housing.tree$Description[i] <- "Luxury"}
}
Housing.tree # Housing.tree is the data that has extral label to dscribe the price

# Stablization
# 1. Convert all of the data: All of the matrix will be standardized to have unit L1 norm in each column and 
# zero mean. L1 norm is uesd for Lasso and descision tree.
Housing.norm <- apply(Housing.original, 2, function(x) scale(x, center = T, scale = T)/norm(scale(x, center = T, scale = T), "1"))
Housing.norm # Housing.norm is the data have been standardized to have unit L1 norm in each column and zero mean.

# Exlanation & Testing
# Before we start analyzing the data, we have to preprocess the data -- if you don't, the data won't be in the 
# same range. For example, in this program, we can't directly compare the relationships in the numbers of the 
# bathroom, the price per squared meter, and the distance from the city center, because they have huge differences
# in position and scale that in the coordinate system. Therefore, through data preprocessing, we can put all of 
# the variables near the origin of the coordinate system without changing the data distribution. I will use 
# z-mean and L1 normalization. Step 1 is to move the data to the origin. Step 2 is to compress the range 
# of the data to [0, 1].
sub.o <- Housing.original$subway
sub.1<- sub.o - mean(sub.o) # Step 1
sub.2 <- (sub.o - mean(sub.o))/(sd(sub.o)) # Step 2
sub.2/norm(as.matrix(sub.2), "1")  # Step 2