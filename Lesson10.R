#Lesson 10
install.packages("ggplot2")
library(ggplot2)

rnorm(12, 10, 4)
dnorm(12, 10, 4)

xvals <- seq(0,20, by=.01)
xvals

yvals <- dnorm(xvals, 10, 4)
yvals

plot(xvals, yvals, type = "l", col = "skyblue", lwd = 2)



plot_dataframe <- data.frame(xvals, yvals)

ggplot(plot_dataframe, aes(x=xvals, y=yvals)) +
  geom_line()



n <- 100
p <- 0.5
xvals2 <- seq(0, n, 1)
xvals2
probs <- dbinom(xvals2, n, p)  
probs  

barplot(probs, names.arg = xvals2)
  
  

barchart_df <- data.frame(xvals2, probs)  
ggplot(barchart_df, aes(x=xvals2, y=probs)) +
  geom_bar(stat = "identity")
  
  
?shapiro.test
## test for normality
#Ho: my numbers come from a normal distribution
#Ha: my numbers did not come from a normal distirbution

xxx <- rnorm(20, 10, 4)
shapiro.test(xxx)
##with p-value = 0.9983, fail to reject ho. I have insufficient statistical evidence to reject normality.








  
  