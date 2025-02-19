install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

data(diamonds)

#Creating new column, checks if price > 4000
d2 <- mutate(diamonds, 
             expensive = ifelse(price > 4000, "Yes", "No"))
d2


#Removing columns
d3 <- select(d2, -c("x","y","z"))
d3

test <- select(d2, -x)
test

d4 <- filter(d3, (cut == "Ideal"))
d4
dim(d4)


d5 <- arrange(d4, depth)
d5
head(d5)
tail(d5)


d6 <- arrange(d4, desc(depth))
d6
head(d6)
tail(d6)



d7 <- arrange(d4, expensive, depth)
d7
head(d7)
tail(d7)


#Summarize the depth column data
summarize(d5,
          ybar_depth = mean(depth),
          sd_depth = sd(depth),
          min_depth = min(depth),
          max_depth = max(depth)
          )

tapply(d5$price, d5$expensive, mean)
##try and reproduce with dplr function



##Produce a "grouped" data frame
d5_grouped <- group_by(d5, expensive)
options(pillar.sigfig = 8)
summarize(d5_grouped,
          ybar_price = mean(price)
          )













