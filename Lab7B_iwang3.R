"""
1.	A study was conducted to observe the effect of a new experimental fungicide designed to combat the growth of colonies of 
toenail fungus. From past experience, the lab workers know that fungicides are tricky, and certain drug-resistant strains of 
the fungus will actually flourish in the presence of the fungicide. Fungal spores were spread on the feet of laboratory monkeys, 
where one foot was treated with the fungicide, and the other foot was left untreated. After a set time, the size of the fungal 
colonies was recorded. The data are stored in a file called fungal.txt (UBLearns).
"""


fungal <- read.csv("~/Downloads/fungal.txt", sep="")
View(fungal)

"""
a)	Are the “treated” measurements independent of the “untreated” measurements, or do the data have a paired structure? 
Carry out the appropriate t-test, and state the conclusion in context.
"""

#Ho: mu_t = mu_ut
#Ha: not all the same
## Test for pairwise differences.
t.test(fungal$treated, fungal$untreated, p.adjust.method = "bonf", paired = TRUE)

# Therefore, p-value = 0.6759 > 0.05, we failed to reject Ho. There is no significant difference between the treated and untreated groups.

"""
b)	Obtain the difference between the “treated” and “untreated” measurements. Test the differences for normality using the Shapiro-Wilk procedure.
"""
difference <- fungal$treated - fungal$untreated
shapiro.test(difference)

#Since p-value = 0.0001966 < 0.05, we reject Ho. This means that the difference between treated and untreated fungal is not normality distirbuted.


"""
c) Test (α=0.05) whether the median of the paired differences is equal to zero, and state the conclusion in context. Is the fungicide effective?
"""

wilcox.test(fungal$treated, fungal$untreated, paired = TRUE)

#Therefore, p-value = 0.6721 > 0.05, we failed to reject Ho. The fungicide has a signifcant effect on fungal growth.



"""
2.	The file migraine.txt (UBLearns) contains data on subjects who occasionally suffer from migraine headaches. 
They were randomly assigned one of three medications (A, B, or C), and told to record the pain level of their next migraine 
30 minutes after taking the medication. Pain was recorded on a 1:10 scale, with 10 being the highest pain score.

"""

migraine <- read.delim("~/Downloads/migraine.txt")
View(migraine)


#a) 	Carry out the ANOVA F-test (α=0.05), and write a comment giving the conclusion.

"""
Ho: mu_A = mu_B = mu_c (same pain level)
Ha: different mean pain level
"""
anova1 <- aov(Pain ~ Drug, data = migraine)
anova1

summary(anova1)
anova(anova1)

#Therefore, p-value = 0.0042 < 0.05, we reject Ho. This means that atleast one of the drugs have a different mean effect on migraine headaches compared to others.


#b)	Test for pairwise differences between group means. Use a correction for multiple testing. Write a comment stating which means differ significantly.

TukeyHSD(anova1)
# There no significant mean difference for drugs C and B because the p-value = 0.9891923 > 0.05, this means that there is not significant difference.
# However, there is significant mean difference for drugs (B, A) and (C, A) because there p-value < 0.05, meaning that there significantly different.


#c)	Test that the model residuals are normally distributed; leave your conclusion in a comment.

shapiro.test(anova1$residuals)
hist(anova1$residuals)

qqnorm(anova1$residuals)
qqline(anova1$residuals)

#Since p-value = 0.1763 > 0.05, we failed to reject Ho. There is significant evidence that the residual distirbution is not normally distirbuted.

#d)	Present the three sample variances. 

aggregate(Pain ~ Drug, data = migraine, FUN = var)


#e)	Test whether the variances are equal across the three medication groups. Leave your conclusion in a comment.


install.packages("car")
library(car)

leveneTest(Pain ~ Drug, data = migraine)

"""
Ho: sigma1^2 = sigma2^2
Ha: not all equal

Since p-value = 0.003191 < 0.05, we reject Ho. The three medication groups do not have the same variance.

"""



#3. Perform the following test (α=0.05) using the migraine data, where η_i is the population median of group i. Give your conclusion in a comment.
"""
H_0:η_1=η_2=η_3
H_a:not all equal

"""
kruskal.test(Pain ~ Drug, data = migraine)

#Since p-value = 0.002643 < 0.05, we reject Ho. The median population of migraine data is not equal, there is atleast one drug group with significantly different median pain level


