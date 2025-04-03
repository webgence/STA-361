#Homework 4

"""
1.	The data set apnea.txt (UBLearns) contains data on a facial measurement thought to 
be related to a disruptive sleep condition. Thirty-one subjects with sleep apnea 
(group 1) were measured, along with thirty-nine subjects without sleep apnea (group 2). (18 pts)
"""

#a)	Produce side-by-side boxplots to compare the two samples.  Comment on any visual differences.

apnea <- read.csv("~/Downloads/apnea.txt", sep="")
View(apnea)

boxplot(length ~ group, data = apnea, 
        col = (length(unique(apnea$group))), 
        main = "Side-by-Side Box Plot", 
        xlab = "Group", ylab = "Length")
#Most of Group 1 data seems be around 100, while most of Group 2 data is around 110.

#b)	Investigators wish to test whether this facial measurement differs on average between healthy and apnea patients. Write the appropriate null and alternative hypotheses.

#H0:  μ_1= μ_2(Facial measurement is same between healthy and apnea patients)
#Ha: μ_1≠ μ_2(Facial measurement is different between healthy and apnea patients)


#c)	Carry out the test (α=0.05) assuming that the groups have equal variance. 
#Give the test statistic, p-value, and conclusion in context.

t.test(length ~ group, data = apnea, var.equal = TRUE) 
#t = -8.5514, df = 68, p-value = 2.187e-12
#With P-value = 2.187e-12 < 0.05, we reject H0. There is significant evidence that facial measurement is different between healthy and apnea patients


#d)	Check for equal variance using Bartlett’s test - see the help file for bartlett.test(.). Give the hypotheses, p-value, and conclusion.

#H0: var1 = var2(there's equal variance between the healthy and apnea group)
#Ha: Var1 != var2(there's unequal variance between the two groups)
bartlett.test(length ~ group, data = apnea)

# Bartlett's K-squared = 0.65871, df = 1, p-value = 0.417
#With p-value = 0.417 > 0.05, we fail to reject H0. There is significant evidience that there's equal variance between the healthy and apnea group.


#e)	Test for normality in both groups.
apnea_pateients <- subset(apnea, group == 1)$length  
healthy_patients <- subset(apnea, group == 2)$length
apnea_pateients
healthy_patients

#qqplot
qqnorm(apnea_pateients)
qqline(apnea_pateients, col = "purple")
#The points closely follow the 45 degree reference line, which could indicate normality. Lets check Shapiro-Wilk Test

qqnorm(healthy_patients)
qqline(healthy_patients, col = "blue")
#The points closely follow the 45 degree reference line aside from a couple of outlier points, which could indicate normality. Lets check Shapiro-Wilk Test

shapiro.test(apnea_pateients)  
shapiro.test(healthy_patients)

"""
data:  apnea_pateients
W = 0.98584, p-value = 0.9456

data:  healthy_patients
W = 0.97145, p-value = 0.4157

with p-values both >0.05, we fail to reject H0. There is evidence that the data is normal.

"""

#f)	Give a 95% confidence interval for the difference between the two population means.
t.test(length ~ group, data = apnea, var.equal = TRUE) 




"""
2.	A rookie statistician views the output from Bartlett’s test and believes the group variances are different. 
Re-analyze the sleep apnea data using a nonparametric alternative to the two-sample t-test. Give the hypotheses, 
p-value, and conclusion in context. (4 pts)
"""

wilcox.test(length ~ group, data = apnea, exact = FALSE)
#H0: The distributions of facial measurements for apnea and healthy groups are the same.
#Ha: The distribution is not the same.
#W = 81, p-value = 6.261e-10

#with p-value = 6.261e-10 < 0.05, we reject H0. There is significant difference in facial measurements for apnea and healthy groups.



