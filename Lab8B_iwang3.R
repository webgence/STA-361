install.packages("ggplot2")
install.packages("dplyr")
install.packages("ISLR")

library(ggplot2)
library(dplyr)
library(ISLR)

data(Carseats)
summary(Carseats)

"""
a)	Fit the following multiple regression model:
Y=β_0+β_1 (Price)+β_2 (CompPrice)+β_3 (Urban)+ε where the predictors measure the sale price of the car seat, 
the sale price of a major competitor’s car seat, and an indicator of whether or not the store is located in an urban area.

"""

data(Carseats)

d2 <- select(Carseats, Sales, Price, CompPrice, Urban)

lm_model <- lm(Sales ~ Price + CompPrice + Urban, data=d2)

summary(lm_model)


"""
b)	Leave a comment that interprets the estimated regression coefficient on X_1 (Price) as obtained in part (a). 
When giving your interpretation, keep in mind that this is no longer simple linear regression.
"""

#Interpretation of b1: when sales increases by 1 AND we hold fixed 
#comp_price and urban, we estimate the mean price to change by 0.087438.

"""
c)	Obtain 98.75% confidence intervals for each of the four regression parameters.
"""
confint(lm_model, level = 0.9875)

"""
(Intercept)  3.9572779  8.67821792
Price       -0.1022945 -0.07258068
CompPrice    0.0679632  0.11389283
UrbanYes    -0.7118286  0.54071201
"""

"""
d)	Identify in a comment which predictors from the model in part (a) have significant linear associations with the number of car seats sold.
"""
options(scipen = 10)
summary(lm_model)

#Price and CompPrice have significant linear association with the number of car seats sold because p < 2e-16



"""
e)	Test (α=0.05) whether the “Urban” variable can be dropped from the model in part (a). 
Leave a comment stating your decision (of course based on a p-value).
"""

full <- lm_model
reduced <- lm(Sales ~ Urban, data=d2)
## H0: Beta1 = Beta2 = 0
## Ha: not both 0


anova(full, reduced) # carries out the test above(a partial F Test)


#Since F-Stat = 110.35, P-Value = 2.2e-16 < 0.05, we reject H0. There is significant evidence that the Urban variable does contribute to the model. 
# And should not be removed.


"""
f)	Remove any unnecessary variables, refit the model, and obtain residuals and fitted values. 
Use these quantities to assess normality and constant variance assumptions.
"""
lm_noUrban <- lm(Sales ~ Price + CompPrice, data = Carseats)
lm_withUrban <- lm(Sales ~ Price + CompPrice + Urban, data = Carseats)

residuals_urban <- residuals(lm_noUrban)
residuals_final <- residuals(lm_withUrban)

fitted_values_urban <- fitted(lm_noUrban)
fitted_values_final <- fitted(lm_withUrban)


hist(residuals_urban)
hist(residuals_final)

#The histograms seems kinda bell shaped.

qqnorm(residuals_urban)
qqline(residuals_urban)

qqnorm(residuals_final)
qqline(residuals_final)


shapiro.test(residuals_urban)

shapiro.test(residuals_final)

#Since p-value = 0.002507 < 0.05, we reject Ho. There is significant evidence that the distribution is not normal.
#Since p-value = 0.002443 < 0.05, we reject Ho. There is significant evidence that the distribution is not normal.


plot(fitted_values_urban, residuals_urban, main = "Residuals vs Fitted", 
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lwd = 2)

plot(fitted_values_final, residuals_final, main = "Residuals vs Fitted", 
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lwd = 2)

# There doesn't seem to be a pattern as the residuals are randomly scattered around the horizonal line(constant variance holds), there's homoscedasticity.




"""
g)	Use the add1(.) or step(.) function to build a model using the forward selection technique. 
Assume that your set of candidate predictors contains: {CompPrice, Price, Urban, Advertising, Age}. 
Verify that terms only get added to the model if the p-value from the F test is smaller than 0.05. 
Finally, write a comment stating the predictors that are chosen for the final model.
"""


stage0 <- lm(Sales ~ 1, data = Carseats)

full_model <- lm(Sales ~ CompPrice + Price + Urban + Advertising + Age, data = Carseats)

model_step <- step(stage0, direction = "forward", scope = formula(full_model), test = "F")

summary(model_step)

#Price, CompPrice, Advertising, and Age P-Values are smaller 0.05.
#Looks like Price, CompPrice, Advertising, and Age made it to the final model.

