"""
1. Suppose that when a person buys their first vehicle from a given dealership, the probability that they 
will buy their next vehicle from that same dealership is 75%. In March 2019, 12 customers are searching 
for their second vehicle, having bought their first vehicle (years earlier) from JC Motors. Assume all of 
them will make a purchase before the end of the month. Let Y be a random variable that counts the number 
of vehicles purchased from JC Motors by this set of customers during March. Obtain the following binomial 
probabilities. (8 pts)
"""

#a. 	P(Y=6)
dbinom(6, size = 12, prob = 75/100)


#b. 	P(Y≥7)
1 - pbinom(6, size = 12, prob = 75/100)

#c. 	P(Y<5)
pbinom(4, size = 12, prob = 75/100)

#d. 	P(6≤Y≤9)

pbinom(9, size = 12, prob = 75/100) - pbinom(5, size = 12, prob = 75/100)



"""
2. Plot the theoretical probability mass function of Y, the random variable from the previous question. 
Label both axes. (4 pts)
"""
install.packages("ggplot2")
library(ggplot2)

n <- 12
p <- 75/100

Ys <- 0:n

pmf <- dbinom(Ys, size = n, prob = p)

ggplot(data.frame(Ys, pmf), aes(x = factor(Ys), y = pmf)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(x = "Number of Successes", y = "Probability", title = "Binomial Distribution PMF")



"""
3. Suppose that levels of a blood biomarker follow a normal distribution with mean 690 and standard deviation 
195. Obtain each of the following: (12 pts)
"""

#a)	The probability that an individual biomarker reading is between 600 and 1000.

pnorm(1000, 690, 195) - pnorm(600, 690, 195) # P(600 < y < 1000)

#b)	The probability that an individual biomarker reading is higher than 900.
1 - pnorm(900, 690, 195)

#c)	The probability that an individual biomarker reading is less than 500.
pnorm(500, 690, 195)

#d)	The probability that an individual biomarker reading is equal to 690.

#P(y = 0) = 0 because the probability of a single value in a continous distribution is very very very small.

#e)	The 15th quantile of the distribution of biomarker values.
qnorm(0.15, 690, 195)


#f)	The IQR.

#IQR = Q3 - Q1
IQR <- qnorm(0.75, 690, 195) - qnorm(0.25, 690, 195)
IQR








