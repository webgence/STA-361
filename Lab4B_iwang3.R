install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

data(starwars)


"""
1. Obtain a data frame that drops the “films,” “vehicles,” and “starships” variables. 
Ensure that you have dropped the correct columns by presenting the dimensions of your new 
data frame and applying the str(.) function. 
"""

starwars
d1 <- select(starwars, -c("films","vehicles","starships"))
d1
str(d1)


"""
2. Obtain two subset data frames: one with all the Droids in the data set (species variable), 
and the second having all characters with hair color recorded as NA. Recall that NA is a logical 
object and can be located using the is.na(.) function. Again, check whether the two data frames 
contain the same records.
"""

species <- filter(starwars, species == "Droid")
species

NAVars <- filter(starwars, is.na(hair_color == TRUE))
NAVars




"""
3. Return to the data frame from Question 1 and sort the data by species (alphabetically) and 
by name (reverse alphabetically). Do this as a single two-dimensional sort, not as two 
separate sorts. Print the first 10 rows of the ordered data.
"""

sorted_species <- arrange(d1, species, rev(name))
sorted_species
head(sorted_species, 10)
tail(sorted_species, 10)




"""
4. Add a new variable to the sorted data frame. The new column will be a logical vector containing 
TRUE if the character is human, and FALSE otherwise. Summarize the new variable using the count(.) 
function and verify that the count matches the number of humans in the “species” column.
"""

d2 <- mutate(d1, 
                human = ifelse(species == "Human", TRUE, FALSE))
d2

count(d2)
sum(d2%species == "Human")



"""
5. Use the functions group_by(.) and summarize(.) to obtain the sample median height for 
humans and non-humans, separately.
"""



d2_grouped <- group_by(d2, !is.na(human))
d2_grouped


summarize(d2_grouped, median_height = median(height, na.rm = TRUE))
