"""
3. Businesses love to sell gift cards because many of the cards sold end up being lost, discarded, or used incompletely. 
The data set gift_card.txt (UBLearns) contains results from a year-long study of gift card usage in four different 
industries (labeled g1-g4). Each observation is the percentage of total gift card dollars spent at a business within a 
year of the gift card’s initial sale. Take care that the data are read in correctly during the initial import phase. (16 pts)
"""
library(ggplot2)

data <- read.table("~/Downloads/giftcard.txt", header = FALSE, skip = 1, sep = "", stringsAsFactors = FALSE)
View(data)



colnames(data) <- c("g1", "g2", "g3", "g4")
data_stacked <- stack(data[, -1])

#a)	Summarize the data using side-by-side boxplots. Also give a table containing group-specific sample means and standard deviations. 
#This may require you to reorganize the data into a stacked form; one option is to explore the stack(.) function.

data <- data.frame(lapply(data, as.numeric))
View(data)

data_stacked <- stack(data)

colnames(data_stacked) <- c("Percentage_Spent", "Industry")

ggplot(data_stacked, aes(x = Industry, y = Percentage_Spent, fill = Industry)) +
  geom_boxplot() +
  labs(title = "Gift Card Usage by Industry", x = "Industry", y = "Percentage Spent")



means <- tapply(data_stacked$Percentage_Spent, data_stacked$Industry, mean)
sds <- tapply(data_stacked$Percentage_Spent, data_stacked$Industry, sd)
summary_table <- data.frame(Industry = names(means), Mean = means, SD = sds)
summary_table


#b)	Fit an ANOVA model to compare the group means. Give the hypotheses, test statistic, p-value, and conclusion in context.
aov(Percentage_Spent ~ Industry, data = data_stacked)
summary(aov(Percentage_Spent ~ Industry, data = data_stacked))


#H0: u1 = u2 = u3 = u4(the means of all the industries are the same)
#Ha: not all the same

#F-value = 8.639, P-Value = 6.76e-05
#With P-value = 6.76e-05 < 0.05, we reject H0. There is significant evidence that atleast one of the indutries means is different.


#c)	If the previous null hypothesis was rejected, test for pairwise differences between group means using a Bonferroni correction. 
#Summarize which pairs are found to be statistically different.

pairwise.t.test(data_stacked$Percentage_Spent, data_stacked$Industry, p.adjust.method = "bonferroni")

#Pairs (g1 and g2), (g1 and g4), (g2 and g3) are statistically different because their p-value < 0.05.


#d)	Apply Levene’s test. Give the hypotheses, p-value, and conclusion.
library(car)

leveneTest(Percentage_Spent ~ Industry, data = data_stacked)
#H0: var of the industries is equal (homogeneous).
#Ha: atleast one of the industry variance is different(heterogeneous).

#F-value - 0.5063, P-Value = 0.6793
#With P-Value = 0.6793 > 0.05, we fail to reject Ho. There is significant evidence that the variance across all of the industries is equal and there's homogeneous.













