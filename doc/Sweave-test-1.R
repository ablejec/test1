###################################################
### chunk number 1: 
###################################################
1:10


###################################################
### chunk number 2: 
###################################################
print(1:20)


###################################################
### chunk number 3: 
###################################################
1 + 1
1 + pi
sin(pi/2)


###################################################
### chunk number 4: 
###################################################
x <- rnorm(20)
print(x)
library(ctest)
print(t1 <- t.test(x))


###################################################
### chunk number 5: 
###################################################
data(iris)
print(summary(iris))


###################################################
### chunk number 6: 
###################################################
pairs(iris)


###################################################
### chunk number 7: 
###################################################
boxplot(Sepal.Length~Species, data=iris)


