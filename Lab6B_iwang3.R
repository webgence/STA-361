"""
1. 
	a. Store n=20 realizations of an F_3,15 random variable in a vector and print them.
	b. Store n=20 realizations of an F_3,15,4 random variable in a vector and print them.
	c. Obtain the sample means of both sets of observations. Comment on which sample produces the larger value of y ̅.

"""
#a
n <- 20
f1 <-rf(n, df1 = 3, df2 = 15)
f1

#b
f2 <- rf(n, df1 = 3, df2 = 15) * 4
f2



#c
mean(f1)
mean(f2)


"""
2. 
a. Set the randomization seed to 4444 and generate 10 observations from t_5. 
This can be done using the set.seed(.) function, then calling rt(.). 
Perform the Shapiro-Wilk test on your sample and leave a comment interpreting the result.

b. Produce a vector of length 1000 containing realizations of a t_5 random variable, 
and test for normality. Again leave a comment.

"""

#a
set.seed(4444)
observations <- rt(10, df = 5)
observations

shapiro.test(observations)
#With P-Value = 0.407, fail to reject Ho. I have insufficient statistical evidence to reject normality.


#b

nn <- 1000
realizations <- c(rt(nn, df = 5))
realizations
shapiro.test(realizations)
#With such a small P-Value(p-value = 9.534e-16), I failed to reject Ho. I have sufficient statistical evidence for normality.



"""
3. 
	a. Produce a Q-Q plot that compares the quantiles of the sample of size 1000 that you produced in Question 2b to theoretical quantiles from N(0,1).
	b. What does your Q-Q plot imply about the tails of the t_5 distribution, as compared to N(0,1)?

"""

#a
realizations_df <- data.frame(realizations)

ggplot(data=realizations_df, aes(sample=realizations)) +
  stat_qq(color="blue") +
  stat_qq_line()


#b

"""
The Q-Q plot shows that the quantiles of the sample of size 1000 is mostly normally 
distributed because most of the points are along the line. There are some outliers present on 
the left and right side.

"""




"""
4. Return to the code block in the Part 6 notes that plots the central t distribution with 
10 degrees of freedom, along with a noncentral t distribution. The plot contains a 
dashed vertical line indicating the 0.05-level critical value of the central t distribution. 
Add an overlay to the plot that shades the area beyond the critical value, like this:

"""

xvals <- seq(-4, 5.5, .01)
central <- dt(xvals, 10)
noncentral <- dt(xvals, 10, 1)
t_data <- data.frame(cbind(xvals, central, noncentral))
crit <- qt(.95, 10)
subt <- subset(t_data, xvals >= crit)

ggplot(t_data) +
  geom_line(aes(y=central, x=xvals, color="central")) +
  geom_line(aes(y=noncentral, x=xvals, color="noncentral")) +
  scale_color_manual(values = c(
    'central' = 'darkblue',
    'noncentral' = 'red')) +
  geom_vline(xintercept=crit, linetype=2) +
  geom_hline(yintercept=0) +
  labs(color = 'Distribution', x=2, y="probability") +
  geom_area(data = subt, aes(y = noncentral, x = xvals), fill = "yellow")





