#lab11 D

"""
1. In this problem, we will consider the simple linear regression model for a continuous 
response variable Y and continuous predictor variable X. Under this model, we have:

Y_i ∼┴iid N(β_0+β_1 X_i,σ^2)

where β_0 is an intercept parameter and β_1 is a slope parameter. We will investigate the 
properties of this model when the normality assumption is true, and when it is violated.

"""

"""
a.	Declare variables for n=30, μ=100, and σ=5. Generate n observations of Y from N(μ,σ). 
Independently, generate n observations of X from standard normal. Due to the way we generated 
X and Y, there should be no relationship between these variables.
"""
n <- 30
mu <- 100
sd <- 5
y <- rnorm(n, mean = mu, sd = sd)
x <- rnorm(n)

sim_data <- cbind(y,x)
sim_data

cor(sim_data)

"""
b.	Use the lm(.) function to fit the simple linear regression model. 
Extract the p-value corresponding to

H_0:β_1=0
H_a: β_1≠0
	
and create a binary variable to note whether to reject the null hypothesis.
"""
data <- data.frame(y,x)

linear_regression <- lm(y ~ x, data)
linear_regression

summary(linear_regression)



#With F-statistic = 0.3197 and p-value = 0.5763 > 0.05, we failed to reject Ho. There 
#is no significant evdience that there's a significant linear relationship between a predictor variable 
#(X) and a response variable (Y)




"""
c.	Produce a version of your existing code that can run in a loop. Do the following 
B=10,000 times: generate data as specified in part (a), fit the simple linear regression model, 
and record whether the null hypothesis of zero slope is rejected. The simulation should produce 
a numeric vector of length B (call it “reject_normal”). Report your simulation-based estimate 
of type I error.
"""

n <- 30
mu <- 100
sd <- 5
B <- 10000
toe <- numeric(B)

for (i in 1:10000) {
  y <- rnorm(n, mean = mu, sd = sd)
  x <- rnorm(n)
  sim_data <- cbind(y,x)
  data <- data.frame(y,x)
  linear_regression <- lm(y ~ x, data)
  toe[i] <- summary(linear_regression)$coefficients[2,4]
}
toe
reject_vec <- ifelse(toe < .05, 1, 0)
reject_vec

hist(toe)
mean(reject_vec)



"""
d.	We will now produce a new simulation, where Y is sampled from a distribution that is not normal, 
but still has mean 100. Use rgamma(.) to generate Y from a gamma distribution having “shape=25” and 
“scale=4.” Carry out the rest of the simulation as done previously, generating B=10,000 rejection 
variables (call the vector “reject_gamma”), and report the estimated type I error rate. 
"""
#rgamma(n, shape, rate = 1, scale = 1/rate)



n <- 30
mu <- 100
sd <- 5
B <- 10000
toe <- numeric(B)

for (i in 1:10000) {
  y <- rgamma(n, shape=25, scale = 4)
  x <- rnorm(n)
  sim_data <- cbind(y,x)
  data <- data.frame(y,x)
  linear_regression <- lm(y ~ x, data)
  toe[i] <- summary(linear_regression)$coefficients[2,4]
}
toe
reject_gamma <- ifelse(toe < .05, 1, 0)
reject_gamma

hist(toe)
mean(reject_gamma)




"""
e. 	Write a program that returns a length-10,000 rejection vector called “reject_chisq” where 
Y is generated from χ_100^2, using rchisq(.). Give the estimated type I error rate.
"""
#rchisq(n, df, ncp = 0)


n <- 30
mu <- 100
sd <- 5
B <- 10000
toe <- numeric(B)

for (i in 1:10000) {
  y <- rchisq(n, df = 100 -1, ncp = 0)
  x <- rnorm(n)
  sim_data <- cbind(y,x)
  data <- data.frame(y,x)
  linear_regression <- lm(y ~ x, data)
  toe[i] <- summary(linear_regression)$coefficients[2,4]
}
toe
reject_chi <- ifelse(toe < .05, 1, 0)
reject_chi

hist(toe)
mean(reject_chi)
"""
output:

hist(toe)
> mean(reject_vec)
[1] 0.05

"""


"""
f.	Simulate once more, using rpois(.) to generate Y from Poisson(λ=100) and storing 
“reject_poisson.”
"""

#rpois(n, lambda)

n <- 30
mu <- 100
sd <- 5
B <- 10000
toe <- numeric(B)

for (i in 1:10000) {
  a <- rpois(n, 100)
  b <- rnorm(n)
  sim_data <- cbind(y,x)
  data <- data.frame(y,x)
  linear_regression <- lm(y ~ x, data)
  toe[i] <- summary(linear_regression)$coefficients[2,4]
}
toe
reject_poisson <- ifelse(toe < .05, 1, 0)
reject_poisson

hist(toe)
mean(reject_poisson)

"""
output: 

hist(toe)
> mean(reject_vec)
[1] 0.0533

"""


"""
g)	Use ggplot(.) to display your rejection rates in a single figure. This may require creating a data frame that contains your simulation results.
"""
install.packages("ggplot2")
library(ggplot2)

df_all <- data.frame(
  reject = c(reject_poisson, reject_chi, reject_gamma, reject_vec),
  distribution = factor(
    rep(c("Poisson", "Chi-squared", "Gamma", "Normal"),
        times = c(length(reject_poisson), length(reject_chi), 
                  length(reject_gamma), length(reject_vec)))
  )
)


ggplot(df_all, aes(x = reject)) +
  geom_bar(stat = "count", fill = "skyblue") +
  facet_wrap(~ distribution) +
  labs(title = "Rejection Rates for p-values < 0.05 by Distribution",
       x = "Rejection (0 = Fail to Reject, 1 = Reject)",
       y = "Count") +
  theme_bw()



