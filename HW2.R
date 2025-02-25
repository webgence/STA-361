"""
1. A survey was taken in 2011 to assess the Canadian electorate’s opinions on abortion. 
A subset of the data can be accessed by loading the car library, then using the command data(CES11). (15 pts)
"""
install.packages("dplyr")
install.packages("car")
install.packages("ggplot2")
install.packages("gganimate")

library(car)
search()

data(CES11)
CES11

#a)	Extract the weight variable into its own vector object. Use this vector to build a histogram.
weight <- c(CES11$weight)
weight

hist(weight)

"""
b)	Sort the data set by both the province variable in reverse alphabetical order and 
then by population (ascending). This should be done as a single sort. Print the first 3 observations.
"""
library(dplyr)

sortByProvince <- arrange(CES11, desc(province), population)
head(sortByProvince, 3)


#c)	Add a new column called ratio, calculated as population divided by weight.
DR_mutate <- mutate(CES11,
                    ratio = population/weight
)
head(DR_mutate)


"""
d)	The column education is a categorical variable about the subject’s highest level of education. 
Create a simplified column called finished that records whether a subject has received a traditional 
4-year college degree using the following definition:
"""

DR_mutate$finished <- ifelse(DR_mutate$education %in% c("bachelors", "higher"), 1, 0)
DR_mutate

df_new <- DR_mutate %>% select(-education)
sortByFinished <- arrange(df_new, finished)
head(sortByFinished,3)
tail(sortByFinished,3)



"""
e)	A researcher is interested in attitudes on abortion only in the province of Ontario. 
Create the appropriate subset data frame, and print its dimensions. 
"""
ontario_df <- subset(sortByFinished, province == "ON")
dim(ontario_df)
head(ontario_df)



"""
f)	The abortion variable contains responses to the question “Should abortion be banned?” 
Create a grouped data frame, and obtain the proportion (a percentage) of Ontario survey 
respondents who were against an abortion ban in 2011.
"""
no <- subset(ontario_df, finished == 0)
yes <- subset(ontario_df, finished == 1)

count(no)
count(yes)

df_grouped <- ontario_df %>%
  mutate(proportion = count(no) / sum(count(no) + count(yes)) * 100)

head(df_grouped)




#2. Return to the original CES11 data set. Obtain the number of rows contained in each of the following subset data frames. (8 pts)

#a)	Male respondents from the New Brunswick (NB) province who have a bachelors degree.

NB <- subset(CES11, province == "NB" & education == "bachelors")
head(NB)
dim(NB)

#b)	Respondents who are either from a rural area, or who have a value of weight that is smaller than 2000.

respondents <- subset(CES11, urban == "rural" | weight < 2000)
head(respondents)
dim(respondents)


#c)	Respondents who are urban females, or who are males with the value “very” for the importance variable.

uber_females <- subset(CES11, (gender == "Female" & urban == "urban" ) | (gender == "Male" & importance == "very" ) )
head(uber_females)
dim(uber_females)


#d)	Respondents whose id is between 2800 and 3200 (inclusive).

id <- subset(CES11, 2800 <= id & id <= 3200)
head(id)
dim(id)



#3. Load the Soils data set, also from the car library. (13 pts)
library(ggplot2)
search()

data(Soils)
head(Soils)

#a)	Use ggplot(.) to produce a properly labeled scatter plot with calcium (Ca) on the x-axis and sodium (Na) on the y-axis.

ggplot(Soils, aes(x=Ca, y=Na)) +
  geom_point()  +
  labs(x= "calcium (Ca)", y = "sodium (Na)", title = "Calcium and Sodium")



#b)	Break up your scatter plot of the overall data into 12 sub-plots using the facet_wrap(.) overlay, using the variable Gp to define the 12 facets.

ggplot(Soils, aes(x=Ca, y=Na)) +  
  geom_point() +    
  labs(x= "calcium (Ca)", y = "sodium (Na)", title = "Calcium and Sodium") +
  facet_wrap(~Gp)



#d)	Use ggplot(.) to produce a scatter plot of phosphorus (P) vs sodium (Na). Assign the plotted symbols to have different shapes according to the Contour variable.  

ggplot(Soils, aes(x=P, y=Na, shape = Contour, color = Contour)) +  
  geom_point() +    
  labs(x= "phosphorus (P)", y = "sodium (Na)", title = "phosphorus (P) vs sodium (Na)")



"""
e)	Use the geom_smooth(.) overlay to draw three curves through the data points in your part (d) plot, one for each level of Contour. 
Use different line types so that the curves can be distinguished.
"""
ggplot(Soils, aes(x=P, y=Na, shape = Contour, color = Contour)) +  
  geom_point() +    
  labs(x= "phosphorus (P)", y = "sodium (Na)", title = "phosphorus (P) vs sodium (Na)") +
  geom_smooth(method = "lm", aes(linetype = Contour))



"""
4. Use the Soils data and options from the ggplot2 cheat sheet to show me a variety of plot I have never seen before. (4 pts)
"""


"""
Chemicals can form compounds when two or more elements combine chemically. These compounds are the result of chemcial reactions 
and the elements are held together through a chemcial bond(a force of attraction between atoms or ions through the share of valence electrons)

I will now make new columns of new compounds by forming compounds from the aviable elements

"""

Soils$Calcium_Phosphate <- (Soils$Ca * 2/3) * (Soils$P * 3/2)
Soils$Magnesium_Potassium_Salt <- Soils$Mg * 2 * Soils$K
Soils$Sodium_Chloride <- Soils$Ca * Soils$P  
head(Soils)

#Upon doing this I realize that this might not be mathematically correct welp, it's already done ¯\_(ツ)_/¯ maybe it'll be useful later.
library(gridExtra)
library(gganimate)



Soils$total_nutrients <- Soils$Ca + Soils$Mg + Soils$K
Soils

plot1 <- ggplot(Soils, aes(x = Ca, y = total_nutrients)) +
  geom_hex(bins = 30) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Ca vs Mg", x = "Ca", y = "Mg")  +
  facet_wrap(~Gp)



plot1



plot2 <- ggplot(Soils, aes(x = Mg, y = total_nutrients), size = pH, color = Contour) +
  geom_point(alpha = 0.6) +
  labs(title = "Bubble Plot of total nutrients by Mg", x = "Mg", y = "Total Nutrients") +
  facet_wrap(~Gp)

plot2


plot3 <- ggplot(Soils, aes(x = factor(P), y = factor(total_nutrients), fill = pH)) +
  geom_tile() +
  labs(title = "Heatmap of pH of P vs pH of Total Nutrients", x = "p", y = "Total Nutrients") +
  facet_wrap(~Gp)

plot3




grid.arrange(plot1, plot2, plot3, ncol = 2)




