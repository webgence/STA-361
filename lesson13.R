install.packages("dplyr")
library(dplyr)
data(starwars)
str(starwars)

plot(starwars$height, starwars$mass, xlim=c(-5, 260), ylim=c(-35, 150))
which.max(starwars$mass)

s2 <- starwars[-16,]
plot(s2$height, s2$mass)
# Treat Height as predcitor variable x
# Treat Mass as the response Y

?lm
lm1 <- lm(mass ~ height, data=s2)
coef(lm1)
abline(a = coef(lm1)[1], b= coef(lm1)[2], col="skyblue", lwd=2)

summary(lm1)

?predict
predict(lm1, newdate=data.frame(height=125), se=T)

newX <- data.frame(height=125)

predict(lm1, newdata = newX)


summary(lm1)
##H0: beta1 = 0
##Ha: beta1 != 0
##With p-value < .0001, we reject H0. There is a significant linear relationship between height and mass.

##CIs for regression parameters
confint(lm)



##Check for normality for the residuals

e <- lm1$residuals
e

sum(e)

hist(e)
qqnorm(e)
qqline(e)

shapiro.test(e)
##We appear to violate normality




###Check for constant variance
Yhat <- lm1$fitted.values
plot(Yhat,e)
abline(h=0, col='red')
## We appear to violate constant variance


###Hypothesis tests for nonzero null values
plot(s2$height, s2$mass, xlim=c(0, 260), ylim=c(0, 260))
abline(a=coef(lm1)[1], b=coef(lm1)[2], col="skyblue", lwd=2)



##H0: beta1 = 1
##Ha: beta1 != 1
## compute a p-value, use a t-distribution with df = n - 1
## t* = (b1 - beta_0) / se(b1)

b1 <- coef(lm)[2]
beta_0 <- 1

lm1_sum <- summary(lm1)
names(lm1_sum)
lm1_sum$coefficients[2,2]

se_b1 <- lm1_sum$coefficients[2,2]
(t_star <- (b1 - beta1_0) / se_b1)

s3 <- nrow(s2)

n <- nrow(s2)
options(scipen=8)
pval <- 2 * pt(t_star, n-1)













