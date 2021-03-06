#Demo
require(kernlab)
require(ggplot2)
require(dplyr)
source("kernelTST.R")

#one dimensional example
x <- rnorm(500,0,sd=sqrt(1/3))
y <- runif(500,-1,1)
kernelTST(x,y)

#multi dimensional example
data(iris)
setosa <- as.matrix(filter(iris,Species=='setosa')[,-5])
virginica <- as.matrix(filter(iris,Species=='virginica')[,-5])
kernelTST(X=setosa,Y=virginica)
