#Question 1
"""
Suppose someone has collected y_1,y_2,…,y_n, a random sample from a normal distribution. 
Rather than turning over the raw data, you are given the sample mean y ̅=16.67 and is its
estimated standard error (se) ̂(y ̅ )=2.82. Write code that creates two new variables
: the upper and lower endpoints of the 95% confidence interval below.
"""

mean <- 16.67
standard_Error <- 2.82

lower <- (mean - 1.96 * standard_Error)
upper <- (mean + 1.96 * standard_Error)

v1 <- c(lower, upper)
v1

m1 <- matrix(v1)
m1

#Question 2
"""
It turns out that R can take derivatives, as long as we properly code a mathematical expression. 
Use the R help page to learn about the function D(.), and obtain the derivative (with respect to x) of 
the expression below. The examples listed near the end of the help page may be highly useful.
[sin⁡(x) ]^2+5e^2x
"""

f = expression((sin(x))^2 + 5 * exp(2 * x))
f


D(f, "x")



#Question 3
"""
A famous result in regression analysis (using matrix form) is the expression for b, the vector of 
least squares estimates of the regression parameters. The expression is:

b=(X^' X)^(-1) X'Y
"""



v1 <- c(99,91,92,109,106)

y <- matrix(v1, ncol=1, byrow = T)
y

x <- matrix(c(0,0,0,0,0, 
              14,22,11,22,13, 
              0,0,0,10,10), 
            ncol = 3, byrow = TRUE)

x

x_trans = t(x)
x_trans

xandXTRANS <- solve((x_trans %*% x))
yandXTRANS <- (x_trans %*% y)

B <- (xandXTRANS) %*% yandXTRANS
B






