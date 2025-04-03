"""
4. The data set gpa.txt contains data on 120 college students who had just completed their first year. 
We wish to investigate whether performance measures collected while these students were still in high school 
can be used to predict their first-year GPA (grade point average). (20 pts)
"""
library(ggplot2)
"""
a)	One of the measures recorded while the students were in high school was an IQ test. 
Produce a scatter plot involving the variables “gpa” and “IQ_score.” 
Be sure that you can properly identify which of these should be treated as the predictor variable and which should be treated as the response variable.
"""
data <- read.csv("~/Downloads/gpa.txt", sep="")
View(data)

ggplot(data, aes(x = IQ_score, y = gpa)) +
  geom_point(color = "blue") +  # Scatter plot points
  labs(title = "Scatter Plot of GPA vs IQ Score",
       x = "IQ Score",
       y = "GPA") 


#b)	Fit the simple linear regression model. Provide the table of regression estimates.
model <- lm(gpa ~ IQ_score, data = data)
summary(model)

#c)	A previous study suggests that a 1-unit increase in IQ score is associated with a 0.05-unit increase in college GPA. 
#Test whether the regression slope is different than 0.05.

"""
looking back at model summary.

F-statistic:   207 on 1 and 118 DF,  p-value: < 2.2e-16

With p-value: < 2.2e-16 < 0.05, we reject Ho. There is significant evidence that the regression slope is different than 0.05.
"""


#d)	Interpret the R^2 statistic, and show how it is calculated (i.e. use a formula).
anova(model)
"""
Multiple R-squared:  0.637,	Adjusted R-squared:  0.6339
SSE = 0.3899, Therefore SSR = 1 - 0.3899 = 0.6101
SSTO = SSE + SSR = 1

R^2=SSR/SSTO  = 0.6101/1

31.470 / 31.470 + 17.936
"""

#e)	Use the fitted model to estimate the first-year GPA for a student whose high school IQ score was 120. 
#Also provide a 95% confidence interval for the mean first-year GPA when IQ score is 120. 
smart <- data.frame(IQ_score = 120)

predicted <- predict(model, newdata = smart)
predicted


predict(model, smart, interval="confidence", level=0.95)


#f)	Use the model residuals to assess the assumptions of normality and constant variance.
e <- model$residuals
e

shapiro.test(e)
#W = 0.99132, p-value = 0.6566
#With p-value = 0.6566 > 0.05, we fail to reject Ho. There is significant evidence that the residuals have a normal distribution.
hist(e)
#looks bell shaped, which could suggest normality.
qqnorm(e)
qqline(e)
#most data points are along the 45 degree reference line, which suggest normality

Y_hat <-  model$fitted.values
ggplot(model, aes(x=Y_hat, y=e)) +
  geom_point() +
  geom_hline(yintercept=0)

#There is linearity because the points seem to be randomly scattered. There may be non-constant variance because Y-hat values 2-4 seem to contain the most points.

"""
5. We will again use the GPA data. Two additional predictors (ACT score and high school class rank) are available. (22 pts)
"""

#a)	Use the cor(.) function to obtain sample correlations between all pairs of variables in the data set. 
#Which variable is most strongly correlated with first-year GPA?

cor(data)
#IQ_score is most strongly correlated with first-year GPA.


#b)	Fit a multiple regression model using all three available predictors. 
#This will be known as the full model. Present a table containing parameter estimates.

model2 <- lm(gpa ~ act_score + IQ_score + class_rank, data=data)
summary(model2)

#c)	Which of the predictor variables are found to have significant association with first-year GPA? 
#Provide p-values when you identify the significant predictors.

#IQ_score is the significant predictor because its p-value <2e-16 < 0.05, therefore this predictor is significant.


#d)	Use the fitted model to estimate the first-year GPA for a student with ACT score 20, IQ score 100, and class rank 65.
student <- data.frame(act_score = 20, IQ_score = 100, class_rank = 65)
predicted_gpa <- predict(model2, newdata = student)
predicted_gpa


"""
e)	The current model has g=4 regression parameters (β_0,β_1,β_2,β_3), and the confint(.) function produces four 95% confidence intervals. 
The probability that all four intervals contain their target parameters is 〖0.95〗^4=0.8145. 
A Bonferroni correction can be applied to a set of g confidence intervals so that their joint probability is at least (1-α) 
by computing each interval at level (1-α/g)100%. Provide a set of Bonferroni-adjusted confidence intervals that maintains joint coverage probability 95%.
"""

#(1 – 0.05 / 4)100% = 98.75%

confint(model2, level = 0.9875)



#f)	Analyze the residuals from the full model, and assess the same assumptions as in the previous problem.
e2 <- model2$residuals
e2

shapiro.test(e2)
#W= 0.99257, p-value = 0.7741
#With p-value = 0.7741 > 0.05, we fail to reject Ho. There is significant evidence that the residuals have a normal distribution.
hist(e2)
#looks bell shaped, which could suggest normality.
qqnorm(e2)
qqline(e2)
#most data points are along the 45 degree reference line, which suggest normality

Y_hat2 <-  model2$fitted.values
ggplot(model2, aes(x=Y_hat2, y=e2)) +
  geom_point() +
  geom_hline(yintercept=0)


#g)	Report the R_a^2 statistic.
summary(model2)$adj.r.squared


#h)	Test whether the full model is any better than the reduced model fit in problem 4.
anova(model, model2)


#Since p-value = 0.3261 > 0.05, this means that there is no significant evidence that adding ACT scores and class rank improves the model

