"""
1. Do not write a function for this problem. In the context of one-sample inference, 
the usual two-sided (1-α)100% confidence interval for the population mean has the form:

y ̅±t_(1-α/2; n-1)  s/√n

Install the “boot” package, open its library, and access the “urine” data set. 
Suppose that this sample of size 79 is representative of American adults. 
Based on this sample, use the formula above to obtain the endpoints of 
a 95% confidence interval for the population mean urine pH value.

"""
install.packages("boot")
library(boot)
data(urine)
urine

ph <- urine$ph
ph


n <- 79
y_bar <- mean(ph)
s <- sd(ph)

n
y_bar
s

alpha <- 0.05
t_critical <- qt(1 - alpha / 2, df = n - 1)
margin_error <- t_critical * s / sqrt(n)
lower_bound <- y_bar - margin_error
upper_bound <- y_bar + margin_error
c(lower_bound, upper_bound)


"""
2. Now that you have some initial code that completes a task, adapt your work from 
the previous problem for use in a function that computes the endpoints of the confidence interval. 
Write a function that accepts two arguments:

1) A vector containing the observed data y_1,y_2,…,y_n.
2) The desired confidence level (e.g. 0.95).

"""

confidence_interval<- function(num_vec, CI){
  n <- length(num_vec)
  y_bar <- mean(num_vec)
  s <- sd(num_vec)
  
  alpha <- 1 - CI

  t_critical <- qt(1 - alpha / 2, df = n - 1)
  margin_error <- t_critical * s / sqrt(n)
  lower_bound <- y_bar - margin_error
  upper_bound <- y_bar + margin_error
  c(lower_bound, upper_bound)
}

confidence_interval(c(1,2,3,4,5,6,7), 0.95)



"""
3. Demonstrate that your function produces the same endpoints that you obtained in problem 
1 when given the urine data.
"""

confidence_interval(ph, 0.95)

#Yes they give the same output 95CI (5.866245, 6.190717)


"""
4. Use the t.test(.) function to produce an identical confidence interval.
"""

t.test(ph)$conf.int
#yes the same output



