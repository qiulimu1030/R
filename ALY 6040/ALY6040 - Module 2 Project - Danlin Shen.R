################################################################################
# Title: ALY6040 - Module 2 Project - Danlin Shen.R                            #
# Created Date: June 04, 2019, 5:49:59 pm                                      #
# Author: Danlin Shen(shen.da@husky.neu.edu)                                   #
# Github: https://github.com/ZacksAmber/R                                      #
# Copyright (c) 2019 Danlin Shen                                               #
################################################################################

install.packages("MASS")
library("MASS")
library("ggplot2")

# Problem 1: Create a vector V with 8 elements (7,2,1,0,3,-1,-3,4):
V <- c(7, 2, 1, 0, 3, -1, -3, 4)

# 1. Transform that vector into a rectangular matrix A of dimensions 4X2 (4- rows, 2-columns);
A <- matrix(V, nrow = 4, ncol = 2)
A

# 2. Create a matrix transpose to the above matrix A. Call that matrix AT;
AT <- t(A)
AT

# 3. Calculate matrix products: A*AT and AT*A. Present the results. What are the dimensions of those two product matrices;
A %*% AT
AT %*% A
dim(A %*% AT)
dim(AT %*% A)

# 4. Square matrixes sometimes have an inverse matrix. Try calculating inverse matrices (or matrixes, if you prefer) of above matrices (matrixes) A*AT and AT*A;
solve(A %*% AT)
solve(AT %*% A)
solve(AT %*% A) %*% (AT %*% A)

# 5. Extend the above vector V with the ninth number of value -2. Do it elegantly by concatenating two vectors!;
V1 <- c(-2)
V2 <- c(V, V1)
V2

# 6. Transform that extended vector into a 3X3 matrix B.
B <- matrix(V2, nrow = 3, ncol = 3)
B

# 7. Calculate the inverse matrix of matrix B. Call it Binv. Demonstrate that the product of B and Binv is the same as the product of Binv and B and is equal to what?
Binv <- solve(B)
Binv
fractions(B %*% Binv)
fractions(Binv %*% B)

# 8. Determine the eigenvectors of matrixes B;
eigen(B)
as.matrix(eigen(B))

# 9. Construct a new matrix C which is made by using each eigenvector of matrix B as a column. Calculate the product of matrix C and matrix B and the product of matrix B and C. Is there any significance to the elements of the product matrixes;
C <- matrix(c(0.86822600, 0.49436902, 0.04222416, 0.1825742, -0.9128709, 0.3651484, 0.2159107, -0.8426423, 0.4932914), nrow = 3, ncol = 3)
C

# 10. Transform matrix B into a matrix with named columns and named rows;
rownames(B) <- c("Row 1", "Row 2", "Row 3")
colnames(B) <- c("Col 1", "Col 2", "Col 3")
B

# 11. Transformed that fully “named” matrix into a data.frame;
D <- as.data.frame(B)
D

# 12. Ask the object you just created what is its class().
class(D)

# Problem 2: Find a way to generate a random variable with a uniform probability between -1 and 2. Create a histogram with 20 bars to convince yourself that generated values truly fall under a uniform distribution. Create a histogram presenting the relative cumulative distribution of generated data.
set.seed(100)
n <- runif(10000, min = -1, max = 2)

ggplot(data.frame(n), aes(x=as.vector(n), y=..density..)) + 
  geom_histogram(color="white", fill="black", alpha=0.5, bins=50) +
  xlim(-1, 2) +
  ggtitle("Problem 2") +
  labs(x="Value",
  y="Relative Frequency")
plot(ecdf(n))

# Problem 3:
# Create a matrix with 40 columns and 100 rows. Populate each column with random variable of the type created in problem 2. Do not create each vector manually. Try to find a way to present two distributions contained in any two of the columns of your matrix on a single plot. To do that you might want to export the distribution data from two columns into two stand-alone vectors of equal length, e.g. y1 and y2. Plot one distribution first using a call to plot(x,y1), where vector x contains the parameter vector with values between  -1 and 2 you selected above. To add the next curve (distribution y2) try invoking function lines(x,y2). To improve your diagram, present two curves in different colors and add labels on x and y axis, as well as the title to your graph.
m <- runif(4000, min = -1, max = 2)
M <- matrix(m, nrow = 100, ncol = 40)
x <- seq(-1, 2, length=100)
set.seed(200)
my <- sample(ncol(M), 2, replace = FALSE)
y1 <- M[, my[1]]
y2 <- M[, my[2]]

getDist <- function(vector, breaks){
  vector_cut = cut(vector, breaks, right=FALSE)
  vector_freq = table(vector_cut) 
  vector_freq = c(0, vector_freq)
  return(vector_freq)
}

breaks = seq(-1, 1, by=0.1)
dist1 <- getDist(y1, breaks)
dist2 <- getDist(y2, breaks)

df_dist <- data.frame(breaks, dist1, dist2)

ggplot(data=df_dist, aes(x=breaks)) +
  geom_line(y=dist1, color="black", alpha=0.5, size=0.8) +
  geom_line(y=dist2, color="darkred", alpha=0.5, size=0.8) +
  geom_point(y=dist1, color="black", alpha=0.5, size=2) +
  geom_point(y=dist2, color="darkred", alpha=0.5, size=2) +
  ylim(0, 11) +
  ggtitle("Problem 3") +
  labs(x="Value", y="Frequency")

# Problem 4:
# Start with your matrix from problem 3. Add yet another column to that matrix and populate that column with the sum of original 40 columns. Create a histogram of values in the new column showing that the distribution starts to resemble the Gaussian curve. Add a true, calculated, Gaussian curve to that diagram with the parameters you expect from the sum of 40 random variables of uniform distribution with values between -1 and 2.
M2 <- cbind(M, rowSums(M))
ggplot(data=NULL, aes(x=M2[, 41])) +
  geom_histogram(color="white", fill="black", alpha=0.5, bins=50) +
  ggtitle("Problem 4") +
  labs(x="Value", y="Frequency")

mean(x=M2[, 41])
sqrt(var(x=M2[, 41]))

ggplot(data=NULL, aes(x=M2[, 41])) +
  geom_histogram(color="white", fill="black",
                 alpha=0.5, bins=50,
                 aes(y=..density..)) +
  ggtitle("Problem 4") +
  labs(x="Value", y="Density")+
  stat_function(fun = dnorm, 
                args = list(mean = 0, sd = 0.5), 
                lwd = 0.8,
                col = 'darkred')
