#Mathematical Operations
v1 <- c(10,15,9,17)

mean(v1)
sd(v1)

m1 <- matrix(1:9, ncol=3, byrow = T)
m1


dim(m1)

m2 <- matrix(rep(7, 9), ncol = 3)
m2
dim(m2)

m1 + m2
m1 - m2



#Matrix multiplication (3x3)(3x3)
m1 * m2

m1 %*% m2

t(m1)


m3 <- matrix(c(1,3,9,1,4,7,1,10,15), ncol=3, byrow = T)
m3
m3_inv <- solve(m3)
m3_inv


m3 %*% m3_inv

zapsmall(m3 %*% m3_inv)


install.packages("psych")
library(psych)
?tr#adds all diagonal values in matrix

tr(m2)




#for loop
v2 <- c(41, 51, 56, 98, 11)

v4 <- numeric(5)

for(i in 1:5){
  v4[i] <- 2*v2[i]
}
  
  
  
v4

m3 > 2



n1 <- 1000000
count <- 0
while(n1 > 500){
  n1 <- n1/2
  count <- count + 1
}
n1
count
  

height <- c(60, 55, 61, 72, 74, 100, 98, 87)
label <- ifelse(height > 78, 'tall', 'short') ## ifelse(<logical condition>, <value if T>, <Value if F>)  
  
  
cbind(height, label)

d1 <- data.frame(height, label)
str(d1)










