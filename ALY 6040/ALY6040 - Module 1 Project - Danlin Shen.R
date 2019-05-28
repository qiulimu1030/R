################################################################################
# Title: ALY6040 - Module 1 Project - Danlin Shen.R                            #
# Created Date: May 23, 2019, 5:59:45 pm                                       #
# Author: Danlin Shen(shen.da@husky.neu.edu)                                   #
# Github: https://github.com/ZacksAmber/R                                      #
# Copyright (c) 2019 Danlin Shen                                               #
################################################################################

# Problem 2 - 1
getwd()

# Problem 2 - 2
setwd("/Users/zack/Desktop/Code/R/ALY 6040")
file.create("FingersAndCars.txt")

# Problem 2 - 3
vv <- c(rep(123, 5))
print(vv)
class(vv)
length(vv)
save(vv, file = "vv.RData")
rm(vv)
load("vv.RData")
print(vv)

# Problem 2 - 4
save(vv, file = "vv.txt", ascii = TRUE)

# Problem 2 - 5
list.files()

# Problem 2 - 6
ALY6040 <- list.files("/Users/zack/Desktop/Code/R/ALY 6070")
class(ALY6040)
str(vv)
mode(vv)

# Problem 2 - 7
rm(vv)
vv
load("vv.RData")
vv

# Problem 2 - 8
rm(vv)
vv
load("vv.txt")
vv

# Problem 3
A <- matrix(c(sample(-3:3, size = 4)), nrow = 3, ncol = 4)
A

# Problem 3 - 1
B <- A - 1
B

# Problem 3 - 2
C <- A * 2
C

# Problem 3 - 3
TA <- t(A)
TA

# Problem 3 - 4
ATA <- A %*% TA
ATA
dim(ATA)

# Problem 3 - 5
TAA <- TA %*% A
TAA
dim(TAA)


# Problem 4
B = diag(1:4) # Let values of diagonal be from 1 to 4
B[lower.tri(B)] = runif(3, min = 0.01, max = 0.2) # Let values off diagonal be from 0.01 to 0.2
B[upper.tri(B)] = t(B)[upper.tri(t(B))]
B

# Problem 4 - 1
BINV <- solve(B)
BINV

# Problem 4 - 2
C <- matrix(c(1, 2, 3, 4), 2)
C
solve(C)
C %*% solve(C)

# Problem 4 - 3
eigen(B)
eigen(BINV)

# Problem 4 - 4
eigen(t(B))
