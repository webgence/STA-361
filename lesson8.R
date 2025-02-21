install.packages("ggplot2")
library(ggplot2)

data("iris")
head(iris)

ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color = Species, shape = Species)) +
  geom_point()  +
  labs(x= "Pet Length", y = "Pet Width", title = "First ggplot") +
  geom_abline(intercept = 3, slope = -1, color = "magenta", linetype = 3, linewidth = 1.5) + 
  geom_hline(yintercept = 1, color = "skyblue")




ggplot(iris, aes(x=Petal.Length)) +
  geom_histogram(color="red" + fill="rosybrown") +
  labs(x= "Pet Length", y = "Frequency")


iris2 <- subset(iris, Species == 'virginica')
dim(iris2)

ggplot(iris2) +
  geom_qq(aes(sample=Petal.Length), color="blue") +
  geom_qq_line(aes(sample=Petal.Length))




ggplot(iris2, aes(y=Petal.Length)) +
  stat_boxplot(geom = "errorbar") +
  geom_boxplot()



ind <- sample(1:150, 100, replace=F)
iris3 <- iris[ind,]
dim(iris3)
table(iris3$Species)

ggplot(iris3, aes(x=Species, color = Species, fill = Species)) +
  geom_bar()



ggplot(data=iris, aes(x=Species, y=Sepal.Width)) +
  stat_boxplot(geom="errorbar") +
  geom_boxplot(fill="orange")






