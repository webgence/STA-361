(var1 <- 71)
(var2 <- "Ivan")

## Delete all objects in memory
rm(list = ls())


## Scatter plot 
#plot(1, 5)

data("iris")
str(iris)
plot(iris$Petal.Length, iris$Petal.Width) ## isolate single variable in df <data frame> $ <variable>


## help file -> as long as you know the name of object
?iris
?mean
str(iris$Petal.Length)
is(iris$Petal.Length)

mean(iris$Petal.Length)


## Download and open an R package
data(faithful)
?faithful
data() # list of R datasets aviable
install.packages("ggplot2")
library("ggplot2")
data("presidents")


## Build a vector
vec1 <- 1:10
vec2 <- c(1,2,3,4,5,6,7,8,9,10)
vec3 <- seq(1,10,1)

mean(c(7,12,19,11,NA))

mean(c(7,12,19,11,NA), na.rm = TRUE)






