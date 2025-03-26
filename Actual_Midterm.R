v1 <- seq(from = 300, to = 625, by = 13)

sum(v1)



v2 = numeric(50)
for(i in 1:50){
  v2[i] <- i^2
}

sum(v2)

v3 <- intersect(v1, v2)
v3



v4 <- numeric(2251)  # To store values from 250 to 2500

# Loop to assign natural logarithm values
for(i in 250:2500){
  v4[i - 249] <- log(i)  # Adjust index to start from 1
}

# Print v4 (optional, could be long)
print(v4)


sum(v4)






p <- 0.2
a <- pgeom(6 - 1, prob = p)
a


b <- 1 - pgeom(10 - 1, prob = p)
b



c <- dgeom(4-1, p)
c


d <- pgeom(8 - 1, prob = p) - pgeom(2 - 1, prob = p)
d



p <- 0.2
median_Y <- qgeom(0.5, prob = p)
print(median_Y)


median_Y <- qgeom(0.5, prob = p) + 1
print(median_Y)





p_Y_leq_2 <- pgeom(2 - 1, prob = 0.2)
p_Y_greater_9 <- 1 - pgeom(9 - 1, prob = 0.2)
final_prob <- p_Y_leq_2 + p_Y_greater_9  # Correct probability calculation
print(final_prob)


f <- (pgeom(2 - 1, prob = p)) | (1 - pgeom(9 - 1, prob = p))
f




p <- 0.2
m <- qgeom(0.05, prob = p)
print(m)






A <- matrix(c(15,-9,10,7,6,3), nrow=3, byrow=T)
A

B <- matrix(c(8,13,3,26,9,24,3,12,17), nrow=3, byrow=T)
B

C <- matrix(c(5,4,3), nrow=3, byrow=T)
C

D <- matrix(c(1,1,1,1), nrow=2, byrow=T)
D


AD <- A %*% D
AD


sum(AD)




ba <- solve(B) %*% A
ba
sum(ba)

library("psych")

cc <- C %*% t(C)
cc

tr(cc)


ab <-  t(A) %*% B
ab





food <- read.csv("~/Downloads/food.txt", sep="")
View(food)

install.packages("dplyr")
library(dplyr)


promo <- filter(food, promotion == 1)
promo

dim(promo)


distri <- filter(food, market_share < 2.5 & nielsen_rating > 525)
distri


april <- filter(food, month == "Apr" & discount == 0)
april



months <- filter(food, month == "Jan" | month == "Feb" | month == "Mar")
months



avg_price <- sum(months$price)/9
avg_price




food_dis <- filter(food, (year == 2001) | (nielsen_rating > 400))
food_dis

dim(food_dis)




DR_mutate <- mutate(food,
                    New_Var = discount + ((2.7 * price)/(market_share))
)
head(DR_mutate)
tail(DR_mutate)


min(DR_mutate$New_Var)



x <- 0
fun <- 3 * x^2 - 5*x + 12

while(fun >= 1000000){
  x <- x + 1
  fun <- 3 * x^2 - 5 * x + 12 
  
}
  
print(x)
print(fun)






function_name <- function(x) {
  fun <- 3 * x^2 - 5*x + 12
  
  while(fun >= 1000000){
    x <- x + 1
    fun <- 3 * x^2 - 5 * x + 12 
    
  }
  
  result <- x
  return(result)  # Optional, as the last evaluated expression is returned by default
}


function_name(0)



g <- function(x) {
  return(3 * x^2 - 5 * x + 12)
}

x <- 0
fun <- g(x)

while (fun <= 1000000) {
  x <- x + 1
  fun <- g(x)
}

# Print the results
print(x)
print(fun)

  

