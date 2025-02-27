#Lesson 9

### Continous Y ###
## find P(Y <= 7) when Y is N(10,4)
?pnorm
rnorm(100, 10, 4) #Generate 100 random numbers from mean 10 and standard deviation of 4


pnorm(7, mean=10, sd = 4) #cdf with value 7 and belows

1 - pnorm(11, mean=10, sd = 4) #P(Y > 11)


pnorm(9, 10, 4) - pnorm(6, 10, 4) # P(6 < y < 9)

# Find c such that P(Y < c) = 0.95
qnorm(0.95, mean = 10, sd = 4)


### Discrete Y ###
#Find P(Y <= 4) when Y is binomical with p = 0.5, n = 10

?pbinom
pbinom(4, 10, 0.5)

#Find P(Y = 4)

pbinom(4, 10, 0.5) - pbinom(3, 10, 0.5)

dbinom(4, 10, 0.5)


#Find P(Y = 10)

dnorm(10, 10, 4)
pnorm(10, 10, 4) - pnorm(9, 10, 4)



#Find P(Y > 6)
1 - pbinom(6, 10, 0.5)

dbinom(7, 10, 0.5) + dbinom(8, 10, 0.5) + dbinom(9, 10, 0.5) + dbinom(10, 10, 0.5)


#Find P(2 <= y < 7)

dbinom(2, 10, 0.5) + dbinom(3, 10, 0.5) + dbinom(4, 10, 0.5) + dbinom(5, 10, 0.5) + dbinom(6, 10, 0.5)



pbinom(6, 10, 0.5) - pbinom(1, 10, 0.5)








