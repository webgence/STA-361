"""
1. When we reject the null hypothesis in one-way ANOVA, we are convinced that the r groups do not 
have the same population mean. The natural follow-up question is: “Which means are statistically 
different?” This implies performing (■(r@2)) tests of the form H_0: μ_k=μ_l, which brings us 
face-to-face with the multiple testing problem. The Tukey correction for multiple testing relies 
on the distribution of the studentized range statistic, defined (for balanced samples) as:
"""

"""
a. Begin by declaring variables r=3, n=5, μ=100, and σ=10 for the number of groups, the number 
of observations per group, and the theoretical mean and standard deviation of a normal random 
variable, respectively. Simulate data in all three groups. While this is not the most efficient 
method, generate the N=nr=15 total observations using two for loops; one that loops over group 
number (from 1 to r), and one that loops over observation number (from 1 to n). Storing a 
length-15 vector of group labels will also be advantageous.
"""
r <- 3       
n <- 5       
mu <- 100    
sigma <- 10 

counter <- 0


data <- numeric(r*n)
groups <- numeric(r*n)

for (i in 1:r) {
  for (j in 1:n) {
    counter <- counter + 1
    data[counter] <- rnorm(1, mu, sigma)
    groups[counter] <- i
  }
}

data
groups
counter



"""
b. After generating the data, you will need to obtain the MSE by fitting a one-way ANOVA model. 
You may use either aov(.) or lm(.) to fit this model, but take care that your grouping variable 
is considered categorical (i.e. modeled as a factor). Once you have extracted MSE, calculate your 
observed value of Q. Call it Q^*.
"""

fitted <- aov(data ~ as.factor(groups))
fitted
summary(fitted)

mse <- summary(fitted)[[1]][2,3]
mse

group_means <- tapply(data, groups, mean)
group_means

Ybar_max <- max(group_means)
Ybar_max
Ybar_min <- min(group_means)
Ybar_min

Qstar <- (Ybar_max - Ybar_min) / sqrt(mse / n)
Qstar



"""
c.	Instead of generating data once and obtaining a single value Q^*, we would now like to generate 
data B=100 times, and store 100 observed values of Q^*. Do this by wrapping a third loop around 
the relevant portions of your existing program. You should initialize an empty vector of length 
100 to store the values of Q^*.
"""
B <- 100     
r <- 3   
n <- 5        
mu <- 100
sigma <- 10

Qstar2 <- numeric(B)

for (sim in 1:B) {
  
  counter <- 0
  data <- numeric(r * n)
  groups <- numeric(r * n)
  

  for (a in 1:r) {
    for (b in 1:n) {
      counter <- counter + 1
      data[counter] <- rnorm(1, mu, sigma)
      groups[counter] <- a
    }
  }
  

  fitted <- aov(data ~ as.factor(groups))
  mse <- summary(fitted)[[1]][2, 3]
  

  group_means <- tapply(data, groups, mean)
  
  Ybar_max <- max(group_means)
  Ybar_min <- min(group_means)
  
  Qstar2[sim] <- (Ybar_max - Ybar_min) / sqrt(mse / n)
}


Qstar2
hist(Qstar2)



"""
d. As you now possess many simulated values of Q^*, you may obtain a simulation-based estimate of 
	the 95th quantile of the distribution of Q. Use the quantile(.) function to obtain this estimate.
"""


quantile(Qstar2, 0.95)



"""
e. The theoretical distribution of Q has two parameters: the number of group means (r) and degrees 
of freedom (N-r). Use the function qtukey(.) to obtain the theoretical 95th quantile of the 
studentized range distribution when r=3 and N-r=12. How far off is your simulation-based estimate?
"""
qtukey(0.95, r, (n*r) - r)

# it's off by like 0.1


"""
f.	Re-run your simulation with B=10,000; this may take a moment to complete. 
Is your estimate of Q_0.95 closer to the true value than when B=100?
"""
B <- 10000     
r <- 3   
n <- 5        
mu <- 100
sigma <- 10

Qstar2 <- numeric(B)

for (sim in 1:B) {
  
  counter <- 0
  data <- numeric(r * n)
  groups <- numeric(r * n)
  
  
  for (a in 1:r) {
    for (b in 1:n) {
      counter <- counter + 1
      data[counter] <- rnorm(1, mu, sigma)
      groups[counter] <- a
    }
  }
  
  
  fitted <- aov(data ~ as.factor(groups))
  mse <- summary(fitted)[[1]][2, 3]
  
  
  group_means <- tapply(data, groups, mean)
  
  Ybar_max <- max(group_means)
  Ybar_min <- min(group_means)
  
  Qstar2[sim] <- (Ybar_max - Ybar_min) / sqrt(mse / n)
}


Qstar2
hist(Qstar2)



quantile(Qstar2, 0.95)

#Yes the value is closer to true value





