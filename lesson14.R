# Install necessary packages (only needed once)
install.packages("ggplot2")
install.packages("dplyr")

# Load libraries
library(ggplot2)
library(dplyr)

# Load the diamonds dataset
data(diamonds)

# Check structure of the dataset
str(diamonds)

# Select relevant columns
d2 <- select(diamonds, price, depth, table, carat, x, y, z)

# Compute correlation matrix
cor(d2)

lm2 <- lm(price ~ depth + table + carat, data=d2)

summary(lm2)

#Interpretation of b3: when carats increases by 1 AND we hold fixed 
#"depth" and "table", we estimate the mean price to increase by $7,858


# Interpretation of b0: when all three predictors (depth, table, carats)
# are 0, we estimate that average price is $13,003.


new_diamond <- data.frame(depth=65, table=50, carat=2)
new_diamond


Yhat<- predict(lm2, newdata=new_diamond, type="response")
Yhat


### Possibly Eliminating a set of predictor variables

full <- lm2
reduced <- lm(price ~ carat, data=d2)
## H0: Beta1 = Beta2 = 0
## Ha: not both 0


anova(full, reduced) # carries out the test above(a partial F Test)


## Forward selection of a subset of predictors.
stage0 <- lm(price ~ 1, data = d2)
summary(stage0)


lm(price ~ depth, data=d2)
lm(price ~ table, data=d2)
lm(price ~ carat, data=d2)

options(scipen = 10)
add1(stage0, scope = ~ . + depth + table + carat, data=d2, test="F")



stage1 <- lm(price ~ carat, data = d2)
add1(stage1, scope = ~ . + depth + table, data=d2, test="F")

stage2 <- lm(price ~ carat + table, data = d2)
add1(stage2, scope = ~ . + depth, data=d2, test="F")

stage3 <- lm(price ~ carat + table + depth, data = d2)


model_step <- step(stage0, direction = "forward", test = "F", scope = ~ . + carat + table + depth)
