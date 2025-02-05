data(iris)
str(iris) #Structure shows that iris is a flower dataframe
head(iris)

unique(iris[,5])

  #Selecting a column from a dataframe: $<data frame>
  
unique(iris$Species)
table(iris$Species) #is a factor with 3 levels: setosa, versicolor, virginica 


orig <- c(0,1,2,2,1,1,0,2,1,0,2)
class(orig)
str(orig)

cat_var <- factor(orig,
                  levels = c(0, 1, 2),
                  labels = c('sm', 'md', 'lg'))

l1 <- list(iris, orig, cat_var)

l1[2]



mean()


#Build a dataframe from individual columns

c1 <- c('joe', 'schmo', 'kuhlmann', 'lopez', 'lara')
c2 <- c(72, 5, 72, 73, 66)
df1 <- data.frame(c1,c2)
str(df1)
  
  
#Importing online dataset
DR <- read.csv("~/Downloads/DR.txt", sep="")
View(DR)
  

  
  







