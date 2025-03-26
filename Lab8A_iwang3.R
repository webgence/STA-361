install.packages("robustbase")
data(epilepsy, package="robustbase")

epilepsy

"""
a)	The variable “Base” records the number of seizures experiences by a patient during the eight weeks prior to the study. 
It is suspected that this variable can be used to predict the total number of seizures experienced during the study period. 
Produce a properly oriented scatter plot of the data. Does a linear function seem to describe the data well?
"""

plot(epilepsy$Base, epilepsy$Ysum, main="Scatterplot of Base and Number of Seizures",
     xlab="Base", ylab="Number of Seizures", pch=19)

#No there will be some outliers, I think a quadratic function might be better.


"""
b)	Add a new variable to the data set. Call the variable “sqrtY” and set it equal to the square root of “Ysum.” 
Create an updated scatter plot. Describe any improvement.
"""


epilepsy$sqrtY <- sqrt(epilepsy$Ysum)
epilepsy

plot(epilepsy$Base, epilepsy$sqrtY, main="Scatterplot of Base and Number of Seizures",
     xlab="Base", ylab="Number of Seizures", pch=19)

#Slightly better, there are definately some outliers but a linear function is better use now.


"""
c)	Fit the simple linear regression model using the lm(.) function, regressing “sqrtY” on “Base.” 
	Leave a comment summarizing the results of the two-sided test (α=0.05) of H_0: β_1=0.
"""
model <- lm(sqrtY ~ Base, data = epilepsy)
model

summary(model)

#H0: β_1 = 0
#Ha: β_1 != 0

#Since P-Value = 2.2e-16 < 0.05, we reject Ho. There is significant linear relationship evidence between sqrtY and Base.

"""
d)	Obtain a 95% confidence interval for β_0, the theoretical intercept parameter.
"""

confint(model, level = 0.95)

# 95% CI(1.64466726, 2.8956917)


"""
e)	Report the R^2 statistic and leave a comment interpreting its value.
"""
summary(model)

#Multiple R-squared:  0.7015, this means that approximately 70.15% of proportion in SqrtY can be explained by the base model.



"""
f)	Overlay the fitted regression line on a scatter plot of the data.
"""
coef(model)
abline(a = coef(model)[1], b= coef(model)[2], col="skyblue", lwd=2)


"""
g)	Use the predict(.) function to estimate (the square root of) the number of seizures occurring during the trial for a patient who experienced 25 
seizures during the 8-week period before the study.
"""

patient <- data.frame(Base = 25)
predicted_sqrtY <- predict(model, newdata = patient, se=T)
predicted_sqrtY


"""
h)	Create two new objects: one for the model residuals, and one for the fitted values. Use these to create a residual plot. 
Leave a comment assessing the constant variance assumption.
"""
install.packages("ggplot2")
library(ggplot2)

e <- model$residuals
e

fitted <- model$fitted.values
fitted


ggplot(epilepsy, aes(x=fitted, y=e)) +
  geom_point() +
  geom_hline(yintercept=0)

#There seems to be a Heteroscedasticity(violation of constant variance) because the points are not randomly scattered. Instead it seem to get wider as the fitted value increases.
#This suggest that variance of errors is not constant.



"""
i)	Apply the Shapiro-Wilk test to the residuals. Leave a comment about whether the normality assumption is violated.
"""

hist(e)
qqnorm(e)
qqline(e)

shapiro.test(e)


#The histogram looks sorta bell shaped which could suggest normality. But lets check qqplot.

#The qq-plot shows some points on/near the 45 degree line, but a good chunk of the points are not on the line. This could suggest that its not normal distributed.

#Finally lets perform Shapiro-Wilk test to the residuals...
#Therefore, with P-value = 0.04442 < 0.05, we reject Ho. There is significant evidence that the residuals are not normally distributed