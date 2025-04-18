"""
1. Consider testing (α=0.05) the hypotheses:
    H_0: μ=50
    H_a:μ≠50
    
    
a. Under H_0, the test statistic below follows a t-distribution with n-1 degrees of freedom.

t^*=(Y ̅-μ_0)/(S/√n)

Generate a sample of size 100 from N(μ=50,σ=10). Use this sample to compute summary statistics 
Y ̅ and S, then obtain t^* and the corresponding two-sided p-value.
"""
sample <- 100
rd <- rnorm(n = sample, mean = 50, sd = 10)
rd
tStar <- (mean(rd) - 50) / (sd(rd) / sqrt(sample))
tStar
pval <- 2 * pt(-abs(tStar), df = sample - 1)
pval


"""
b.	Repeat the previous task B=1000 times using a for loop. Store the p-values in a vector, 
then create a new 0/1 vector that stores whether each p-value is small enough to reject H_0. 
Take care that you are storing a two-sided p-value in each case, regardless of whether a single 
statistic is negative or positive. Based on your simulation, estimate the type I error of the 
one-sample t-test.
"""
sample2 <- 100
B <- 1000
reject_vec <- numeric(B)
T_vec <- numeric(B)
P_vec <- numeric(B)

for(i in 1:B){
  rd2 <- rnorm(n = sample2, mean = 50, sd = 10)
  T_vec[i] <-  (mean(rd2) - 50) / (sd(rd2) / sqrt(sample2))
  P_vec[i] <- 2 * pt(-abs(T_vec[i]), df = sample2 - 1)
  reject_vec[i] <- ifelse(P_vec[i] <= .05, 1, 0)
}

T_vec
P_vec
reject_vec
sum(reject_vec)
mean(reject_vec)
type1error_rate <- mean(reject_vec)
type1error_rate



"""
c. We will now investigate the power of the test for one specific alternative hypothesis. 
Write a simulation that performs the following steps B=1000 times:

- Generate a sample of size 100 from N(μ=53,σ=10).  
- Calculate t^* and the corresponding two-sided p-value.
- Create a rejection variable that is set to 1 if we reject H_0: μ=50, and 0 otherwise.

Use your length-1000 “reject” vector to estimate the probability of rejecting H_0 when the 
population mean is equal to 53. This is a simulation-based estimate of power 
(for the specific alternative that μ=53).
"""
size <- 100
B <- 1000
p_values <- numeric(B)
rejects <- numeric(B)

for(i in 1:B){
  rd3 <- rnorm(n = size, mean = 53, sd = 10)
  
  t_stat <- (mean(rd3) - 50) / (sd(rd3) / sqrt(size))
  
  p_values[i] <- 2 * pt(-abs(t_stat), df = size - 1)
  rejects[i] <- ifelse(p_values[i] <= .05, 1, 0)
}

mean(rejects)















