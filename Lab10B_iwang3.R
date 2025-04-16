install.packages("dplyr")
library(dplyr)

"""
We will write a custom function to perform a one-way analysis of variance. 
a)	First we will fit a model using the aov(.) function. Load the starwars data 
(dplyr library) and obtain a data frame that contains characters who have one of the 
following eye_color values: black, blue, brown, or yellow. After removing ‘NA’ values of 
height, you should have 58 records of eye_color and height. Use an ANOVA model to decide 
whether average height differs by eye color.
"""

data(starwars)
starwars

eye_color <- filter(starwars, eye_color == "black" | eye_color == "blue" | eye_color == "brown" |
                      eye_color == "yellow")


cleanDS <- subset(eye_color, is.na(height) == FALSE)
cleanDS

dim(cleanDS)


starwar_anova <- aov(height ~ eye_color, data = cleanDS)
starwar_anova

summary(starwar_anova)

#with p-value = 0.421, we fail to reject Ho. Mean height does not seem significantly related to eye color.




"""
b)	Now that we have seen the ANOVA results, we will attempt to reproduce them manually, continuing 
to use the data frame with 58 rows. Begin by obtaining Y ̅_(⋅⋅), the grand mean of the height variable
. Also obtain Y ̅_(1⋅), Y ̅_(2⋅), Y ̅_(3⋅), and Y ̅_(4⋅), the sample mean heights for each eye color. 
"""
y_hat <- mean(cleanDS$height)
y_hat

blackeye <- subset(cleanDS, cleanDS$eye_color == "black")
y_hat1 <- mean(blackeye$height)
y_hat1

blueeye <- subset(cleanDS, cleanDS$eye_color == "blue")
y_hat2 <- mean(blueeye$height)
y_hat2

browneye <- subset(cleanDS, cleanDS$eye_color == "brown")
y_hat3 <- mean(browneye$height)
y_hat3


yelloweye <- subset(cleanDS, cleanDS$eye_color == "yellow")
y_hat4 <- mean(yelloweye$height)
y_hat4


group_mean <- tapply(cleanDS$height, cleanDS$eye_color, mean)
group_mean


"""
c) The SSTO can be computed by adding up all squared deviations of individual data points from 
the grand mean. We have four groups in this data set, so r=4. Obtain the quantity:
"""

ssto <- sum((cleanDS$height - y_hat)^2)
ssto

"""

d)	The SSE can be computed by adding up the squared deviations of individual data points from the 
appropriate group-specific sample mean. Obtain the quantity:

"""
sorted <- arrange(cleanDS, eye_color)
sorted

counts <- table(cleanDS$eye_color)
mean_vec <- rep(group_mean, counts)
mean_vec
sse2 <- sum((sorted$height - mean_vec)^2)
sse2





s1 <- (blackeye$height -  y_hat1)^2
s2 <- (blueeye$height -  y_hat2)^2
s3 <- (browneye$height -  y_hat3)^2
s4 <- (yelloweye$height -  y_hat4)^2

sse <- sum(s1, s2, s3, s4)
sse



#e)	Based on the ANOVA equation SSTO=SSTR+SSE, use subtraction to obtain the SSTR.

sstr <- ssto - sse2
sstr



"""
f)	The test statistic for ANOVA is F^*=MSTR/MSE, where MSTR=SSTR/(r-1) and MSE=SSE/(n-r). 
Calculate the observed value of the test statistic.
"""
r <- length(unique(cleanDS$eye_color))
n <- length(cleanDS$eye_color)

mstr <- sstr / (r - 1)
mse <- sse / (n - r)
f_star <- mstr/mse
f_star
#f = 0.9553407


"""
g) If the null hypothesis holds, and all treatment groups have the same mean, F^* follows an F 
	distribution with numerator and denominator degrees of freedom equal to r-1 and n-r, 
	respectively. Obtain the p-value of the observed test statistic.
"""
p_val <- 1 - pf(f_star, r-1, n-r)
p_val


"""
h)	Adapt your program into a function that accepts two arguments: a vector of numeric responses and
a vector of group labels. The function should output the test statistic and p-value (with headings).
Verify that executing your function matches the aov(.) results from part (a). 
Consider additional verification using a second data set.
"""




custom_anova <- function(num_vec, label_vec){
  label_vec <- as.factor(label_vec)
  df <- data.frame(response = num_vec, group = label_vec)
  grand_mean <- mean(df$response)
  group_mean <- tapply(df$response, df$group, mean)
  
  ssto <- sum((df$response - grand_mean)^2)
  sorted <- df[order(df$group), ]
  counts <- table(df$group)
  mean_vec <- rep(group_mean, counts)
  sse <- sum((sorted$response - mean_vec)^2)
  sstr <- ssto - sse
  
  r <- length(unique(df$group))
  n <- length(df$response)
  
  mstr <- sstr / (r - 1)
  mse <- sse / (n - r)
  
  f_star <- mstr/mse
  
  p_val <- 1 - pf(f_star, r-1, n-r)
  
  cat("F statistic:", f_star, "\n")
  cat("p-value:", p_val, "\n")
  
}

custom_anova(cleanDS$height, cleanDS$eye_color)
summary(starwar_anova)
#works!

library(MASS)
data(painters)

painters
paint_model <- aov(Composition ~ School, data = painters)
summary(paint_model)

custom_anova(painters$Composition, painters$School)
#works!


