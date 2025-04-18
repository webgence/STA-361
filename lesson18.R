
qnorm(.975)
#critical value for z score


#Imagine we don't have qnorm function
random_data <- rnorm(1000000, mean=0, sd=1)
hist(random_data)
guess <- quantile(random_data, .975)
guess


#consider testing H0: mu = 5
#                 Ha: mu != 5
## let's pretend the true sigma = 3
#we will generate data under the null hypothesis many times, 
#execute the Z-test, and store the outcome.

n <- 10
rd2 <- rnorm(n, mean=5, sd=3)
rd2
Zstar <- (mean(rd2) - 5) / (3 / sqrt(n))
Zstar
pval <- 2 * pnorm(-abs(Zstar))
pval

reject <- ifelse(pval <= .05, 1, 0)
reject



## now it works once, set things up to store results over many iterations.

n <- 10
B <- 1000000
reject_vec <- numeric(B)
Z_vec <- numeric(B)
P_vec <- numeric(B)

for(i in 1:B){
  rd2 <- rnorm(n, mean=5, sd=3)
  Z_vec[i] <- (mean(rd2) - 5) / (3 / sqrt(n))
  P_vec[i] <- 2 * pnorm(-abs(Z_vec[i]))
  reject_vec[i] <- ifelse(P_vec[i] <= .05, 1, 0)
}

Z_vec
P_vec
reject_vec
sum(reject_vec)

mean(reject_vec)


#now consider different sample sizes
n_vec <- c(10,20,30)
B <- 1000000
reject_martix <- matrix(0, nrow = B, ncol = length(n_vec))
Z_vec <- numeric(B)
P_vec <- numeric(B)

for(i in 1:B){
  for(j in 1:length(n_vec)){
    
    rd2 <- rnorm(n_vec[j], mean=5, sd=3)
    Z_vec[i] <- (mean(rd2) - 5) / (3 / sqrt(n_vec[j]))
    P_vec[i] <- 2 * pnorm(-abs(Z_vec[i]))
    reject_matrix[i, j] <- ifelse(P_vec[i] <= .05, 1, 0)
    
  }
}
mean(reject_martix[,1])
mean(reject_martix[,2])
mean(reject_martix[,3])

