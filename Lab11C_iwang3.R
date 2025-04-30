"""
1. Applied statisticians should be in the habit of checking model assumptions after performing an 
analysis. For example, if an ANOVA was carried out, we can examine a Q-Q plot and a residual plot 
for violations of normality or constant variance. Sometimes, even when the assumptions appear to be 
violated, the statistician will still choose to report the ANOVA results, citing the fact that ANOVA 
is robust to mild violations of the assumptions. We will conduct a simulation to investigate the 
meaning of “robust.”
"""

"""
a.	Declare r=3 groups and n=10 observations per group. Use two for loops to generate N=rn observations 
	ndard normal. In addition, create a “group” vector to store which group each observation belongs to. 
	Declare a data frame that has two columns: the simulated normal data, and the “group” vector.
"""

r <- 3 
n <- 10 

y <- numeric(r * n)  
label <- integer(r * n) 

index <- 1  
for (i in 1:r) {
  for (j in 1:n) {
    y[index] <- rnorm(1)
    label[index] <- i
    index <- index + 1
  }
}

data_frame <- data.frame(y, label)
print(data_frame)


"""
b.	Fit an ANOVA model to the simulated data (take care that your grouping variable is interpreted as 
	categorical) and extract the p-value for the overall F-test.
"""
res_aov <- aov(y ~ label,
               data = data_frame
)

res_aov
summary(res_aov)

test <- summary(res_aov)[[1]]$`Pr(>F)`[1] #get p-value
test


"""
c.	Use a for loop to store B=10,000 p-values from ANOVA models fit to simulated data 
(you may wish to set B to a small value initially). Create a new vector called “reject” 
which is equal to 1 if the p-value is 0.05 or smaller, and 0 otherwise. The mean of this 
vector is your simulated type I error rate, and should be close to 0.05.
"""

B <- 10000
r <- 3 
n <- 10 
y <- numeric(r * n)  
label <- integer(r * n) 
pvals <- numeric(B)  

for (x in 1:B){
  index <- 1  
  for (i in 1:r) {
    for (j in 1:n) {
      y[index] <- rnorm(1)
      label[index] <- i
      index <- index + 1
    }
  }

  data_frame <- data.frame(y, label)
  res_aov <- aov(y ~ label,data = data_frame)
  summary(res_aov)
  test <- summary(res_aov)[[1]]$`Pr(>F)`[1] #get p-value
  pvals[x] <- test
}
pvals
reject <- ifelse(pvals <= 0.05, 1, 0)
reject

TOE <- mean(reject)
TOE


"""
d.	Violate the equal variance assumption by generating data (still with B=10,000) from N(μ=0,σ=i^2), 
where i is the group number. Obtain the simulation-based type I error.
"""
B <- 10000
r <- 3 
n <- 10 
y <- numeric(r * n)  
label <- integer(r * n) 
pvals <- numeric(B)  

for (x in 1:B){
  index <- 1  
  for (i in 1:r) {
    for (j in 1:n) {
      y[index] <- rnorm(1, 0, i^2)
      label[index] <- i
      index <- index + 1
    }
  }
  
  data_frame <- data.frame(y, label)
  res_aov <- aov(y ~ label,data = data_frame)
  summary(res_aov)
  test <- summary(res_aov)[[1]]$`Pr(>F)`[1] #get p-value
  pvals[x] <- test
}
pvals
reject <- ifelse(pvals <= 0.05, 1, 0)
reject

TOE <- mean(reject)
TOE
  
  
"""
e. 	Repeat part (d), but this time generate data from N(μ=0,σ=1/i^2) and estimate the type I error 
rate.
"""
B <- 10000
r <- 3 
n <- 10 
y <- numeric(r * n)  
label <- integer(r * n) 
pvals <- numeric(B)  

for (x in 1:B){
  index <- 1  
  for (i in 1:r) {
    for (j in 1:n) {
      y[index] <- rnorm(1, 0, 1/i^2)
      label[index] <- i
      index <- index + 1
    }
  }
  
  data_frame <- data.frame(y, label)
  res_aov <- aov(y ~ label,data = data_frame)
  summary(res_aov)
  test <- summary(res_aov)[[1]]$`Pr(>F)`[1] #get p-value
  pvals[x] <- test
}
pvals
reject <- ifelse(pvals <= 0.05, 1, 0)
reject

TOE <- mean(reject)
TOE


"""
f.	Repeat part (d), but violate the normality assumption. Generate each data point from 
Gamma(shape=2,scale=1), which is a skewed family of distributions, using the function rgamma(.), 
and estimate the type I error rate. 
"""
B <- 10000
r <- 3 
n <- 10 
y <- numeric(r * n)  
label <- integer(r * n) 
pvals <- numeric(B)  

for (x in 1:B){
  index <- 1  
  for (i in 1:r) {
    for (j in 1:n) {
      y[index] <- rgamma(n, shape = 2, scale = 1)
      label[index] <- i
      index <- index + 1
    }
  }
  
  data_frame <- data.frame(y, label)
  res_aov <- aov(y ~ label,data = data_frame)
  summary(res_aov)
  test <- summary(res_aov)[[1]]$`Pr(>F)`[1] #get p-value
  pvals[x] <- test
}
pvals
reject <- ifelse(pvals <= 0.05, 1, 0)
reject

TOE <- mean(reject)
TOE



  
  
  
  
  
  
  
  



