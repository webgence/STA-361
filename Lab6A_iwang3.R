"""
Consider a (continuous) normal random variable with mean 60 and standard deviation 5. 
Suppose we obtain a single realization of this random variable. Use probabilistic functions in 
R to answer the following questions.
"""

#a)	What is the probability that the observation is smaller than 65?

pnorm(65, 60, 5) #P(Y < 65)

#b)	What is the probability that the observation is greater than 58?
1 - pnorm(58, 60, 5) #P(Y > 58)

#c)	What is the probability that the observation is between 50 and 61?
pnorm(61, 60, 5) - pnorm(50, 60, 5)

#d)	What is the population IQR of this random variable?

#IQR = Q3 - Q1 = 65 - 45 = 20
IQR <- qnorm(0.75, 60, 5) - qnorm(0.25, 60, 5)
IQR


"""
2. Consider the following (continuous) t random variables.
"""

#a) Find the 90th percentile of a t distribution with 17 degrees of freedom.
t_value <- qt(0.90, 17)
t_value


"""
b)	Find the probability that an observation from a non-central t distribution with 17 degrees of 
freedom and non-centrality parameter 0.5 exceeds the 90th percentile found in part (a).
"""

prob <- 1 - pt(t_value, 17, ncp = 0.5)
prob



"""
3. Suppose that the mean number of serious accidents per year in a large factory (where the number 
of employees remains constant) follows a (discrete) Poisson distribution with mean λ=5. 
Find the probability that in a given year there will be:
"""

#a)	Exactly seven accidents.

seven_accidents <- dpois(7, 5)
seven_accidents

#b)	Ten or more accidents.
more_accidents <- 1 - dpois(9, 5)
more_accidents

#c)	No accidents.

no_accidents <- dpois(0, 5)
no_accidents


#d)	At least three but no more than six accidents.

#(P(3 <= X <= 6))
atleast <- dpois(6, 5) - dpois(2, 5)
atleast


#e)	More than five accidents.
# P(Y > 5)
more_five <- 1 - dpois(5, 5)
more_five




"""
4. Suppose we randomly select 18 patients into a clinical trial examining the effectiveness of a
new medication. Suppose that a given patient will either recover or not recover (therefore the 
[discrete] binomial distribution should be used here), and that the true probability for an 
individual to recover after receiving this treatment is 30%.
"""


#a)	What is the probability that exactly 9 patients in the study recover?

pbinom(9, 18, 0.30) - pbinom(8, 18, 0.30)

dbinom(9, 18, 0.30)


#b)	What is the probability that at least 6 patients but no more than 8 patients recover?

#P(6 <= X < 8)
dbinom(6, 18, 0.30) + dbinom(7, 18, 0.30)



#c)	What is the probability that fewer than 6 patients recover?

# P(X < 6)
pbinom(5, 18, 0.30)

pbinom(5, 18, 0.30)




