"""
1. The str(.) function can be a concise and easy way to learn about the variables in a data frame. 
In this case the presentation is not so nice, as three of the variables have complicated summaries. 
Carefully inspect the output given by str(.) and determine which three variables are not easily 
summarized by this function. Identify these variables in a comment.
"""
data(starwars)
head(starwars)

str(starwars)

#Films<list>,  vehicles <list>, starships <list> are not easily summarized because its in list form

"""
2. Obtain a data frame that drops the three variables identified in Question 1. Ensure that you have 
dropped the correct columns by presenting the dimensions of your new data frame, then reapplying the 
str(.) function. 
"""

df_new <- starwars %>% select(-films, -vehicles, -starships)
dim(df_new)
str(df_new)


"""
3. The principle investigator requests a data set that contains only robotic Star Wars characters. 
One option is to isolate records for which the species variable matches the text “Droid.” 
Another option is to isolate records for which hair_color is set to missing. 
Obtain two subset data frames, one for each of these approaches. 
Do the two data frames contain the same records? If not, comment on any discrepancies. 
[Hint: missing values of hair_color are set to the logical value NA, 
which may be difficult to find using text matching. Consider the function is.na(.) instead.]
"""

robotic_df <- subset(df_new, species == "Droid")
dim(robotic_df)
head(robotic_df)

hair_df <- subset(df_new, is.na(hair_color) == TRUE)
dim(hair_df)
head(hair_df)

#Sorta because the is.na also get rid of none.


"""
4. Return to the data frame from Question 2 and sort the data in reverse alphabetical 
order by name. Convince the investigator that the proper ordering has been achieved by 
printing the first 5 rows of the ordered data.
"""


DR_rev_alphabet <- df_new[rev(order(df_new$name)),]
head(DR_rev_alphabet)


"""
5. Add a new variable to the sorted data frame. The new column should be a 
logical vector containing TRUE if the character is human, and FALSE otherwise. 
Summarize the new variable using the table(.) function, and verify that the count
matches the number of humans in the “species” column.
"""
df_new

df_new$human_flag <- ifelse(df_new$species == "Human", TRUE, FALSE)

table(df_new$human_flag)













