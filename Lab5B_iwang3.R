install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
search()




beers <- read.csv("~/Documents/STA 361/beers.txt", sep="")
View(beers)
head(beers)

"""
1. We will repeat one of the prior exercises using the beers.txt data set.
a)	Create a scatter plot with beers on the x-axis and BAC on the y-axis.
b)	Add blue dashed horizontal reference lines at 0.05 and 0.07 (the ability impaired range).
c)	Apply custom labels to both axes and give the plot a title.

"""


ggplot(beers, aes(x=Quantity, y=BAC)) +
  geom_point()  +
  labs(x= "Quantity", y = "BAC", title = "Beers Consumption") + 
  geom_hline(yintercept = 0.05, color = "blue", lty=2) +
  geom_hline(yintercept = 0.07, color = "blue", lty=2)





"""
2. This plotting exercise requires the “starwars” data.
a)	Obtain a data frame that contains all of the Star Wars characters designated as “Human” by the species variable. Demonstrate that your data frame has 35 rows.
b)	Use your data frame from part (a) to produce a scatter plot that places mass on the x-axis and height on the y-axis.
c)	Update your previous scatter plot so that the sexes have different plotting symbols. Leave a comment in your script describing any clustering you observe.
d)	With 35 total humans, why do only three visible plotting symbols correspond to females? Investigate the raw data and leave a comment explaining what you find.
e)	Produce a new plot of height vs mass, this time with symbols that are colored differently according to the eye_color variable.
"""

install.packages("dplyr")
library(dplyr)
library(ggplot2)
data(starwars)

#a)	Obtain a data frame that contains all of the Star Wars characters designated as “Human” by the species variable. Demonstrate that your data frame has 35 rows.
human <- subset(starwars, species == "Human")
dim(human)
head(human, 35)


#b)	Use your data frame from part (a) to produce a scatter plot that places mass on the x-axis and height on the y-axis.
ggplot(human, aes(x = mass, y = height)) +
  geom_point() +
  labs(x = "Mass", y = "Height", title = "Mass vs Height for Human Characters")


#c)	Update your previous scatter plot so that the sexes have different plotting symbols. Leave a comment in your script describing any clustering you observe.
ggplot(human, aes(x = mass, y = height, shape = sex, color = sex)) +
  geom_point() +
  labs(x = "Mass", y = "Height", title = "Mass vs Height for Human Characters")
#The scatterplot seems to have a cluster of male heights 


#d)	With 35 total humans, why do only three visible plotting symbols correspond to females? Investigate the raw data and leave a comment explaining what you find.
human
tail(human)

"""
Females listed in the dataframe seem to have a body mass from approximately 40 to 75.
Historically, there haven't been a large number of prominent female characters in the Star Wars universe

"""

#e)	Produce a new plot of height vs mass, this time with symbols that are colored differently according to the eye_color variable.
ggplot(human, aes(x = mass, y = height, shape = eye_color, color = eye_color)) +
  geom_point() +
  labs(x = "Mass", y = "Height", title = "Mass vs Height for Human Characters")





"""
3. This again uses the starwars data.
a)	Obtain a data frame containing all characters having values of hair_color containing the text “black,” “brown,” or “none.” This can be done in a single line of code that uses the logical operator %in%. Verify that your data frame has 69 rows.
b)	Using this data frame with 69 rows, produce side-by-side boxplots built from the birth_year variable, one boxplot for each unique value of hair_color.

"""

#a)	Obtain a data frame containing all characters having values of hair_color containing the text “black,” “brown,” or “none.” This can be done in a single line of code that uses the logical operator %in%. Verify that your data frame has 69 rows.

character_hair <- subset(starwars, hair_color %in% c("black", "brown", "none"))
dim(character_hair)



#b)	Using this data frame with 69 rows, produce side-by-side boxplots built from the birth_year variable, one boxplot for each unique value of hair_color.
ggplot(data=character_hair, aes(x=hair_color, y=birth_year)) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot(fill="orange")



