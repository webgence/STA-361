"""
The first two questions require loading the “msleep” data set (ggplot2 package), which records sleep data on several mammalian species.
	A person who regularly sleeps for nine hours might be considered lazy.
	Test the claim (α=0.05) that mammals like those represented in this data set sleep significantly longer than nine hours, on average. 
	Use the variable “sleep_total.” State the conclusion of the test in context.
	Verify the normality assumption on the “sleep_total” variable.

"""

install.packages("ggplot2")

library(ggplot2)

data(msleep)
msleep

#a

sleep_total <- c(msleep$sleep_total)
sleep_total

##H0: mu <= 9
## Ha: mu > 9

t_test <- t.test(sleep_total, mu= mean(sleep_total), alternative="greater")
t_test

#Therefore, the P-value = 0.5 > 0.05, we failed to reject Ho. 
#There is no significant evidence that several mammalian species sleep for greater then 9 hours


#b
qqnorm(sleep_total)
qqline(sleep_total, col="pink")

##Since most of the data point is near the line, this satisfies normality.



"""
2.	A comparison of sleep habits between carnivores and herbivores might be interesting.  
"""

#a)	Obtain a subset data frame that only contains carnivores and herbivores. Demonstrate that this data frame has 51 rows.

msleep
new_df <- subset(msleep, msleep$vore == 'carni' | msleep$vore == 'herbi')

new_df
dim(new_df)


#b)	Apply the two-sample t-test (equal variances, α=0.05) that mean total sleep for carnivores differs from mean total sleep for herbivores. 
#State the conclusion of the test in context.

##Ho: U_c = U_h
##Ha: U_c != U_h

carn_sleeptotal <- c(new_df$sleep_total[new_df$vore == "carni"])
carn_sleeptotal

herb_sleeptotal <- c(new_df$sleep_total[new_df$vore == "herbi"])
herb_sleeptotal


t.test(carn_sleeptotal, herb_sleeptotal)


#Therefore, the p-value = 0.5308 > 0.05, we failed to reject Ho. 
#There is no significant evidence the mean total sleep for carnivores differs from mean total sleep for herbivores.


#c)	Consult the help page for the bartlett.test(.) function, and use it to test whether the groups have equal variances. 
#State the conclusion in context.

?bartlett.test(.)

total_sleep <- c(carn_sleeptotal, herb_sleeptotal)
group <- factor(c(rep("Carnivores", length(carn_sleeptotal)), 
                  rep("Herbivores", length(herb_sleeptotal))))

bartlett.test(list(carn_sleeptotal, herb_sleeptotal))


#Therefore, the p-value = 0.8364 > 0.05, we failed to reject Ho. 
#There is significant evidence the variance of  total sleep for carnivores differs from variance total sleep for herbivores.


"""
3.	A middle school track coach is considering the adoption of a new training program for his sprinters. 
To investigate its efficacy, the coach records the 100-yard dash times for 10 randomly selected students before 
implementing the training program. The same runners were timed again after one week of the training program. 
The data set is called dash_times.txt (UBLearns).
"""
dashtimes <- read.csv("~/Downloads/dashtimes.txt", sep="")
View(dashtimes)
dashtimes
dashtimes$before
dashtimes$after

#a)	Test whether the training program is effective at improving 100-yard dash times. State the conclusion in context.

#Paired t-test: Ho: mu_before = 0
#               Ha: mu_after >= 0 


t.test(dashtimes$before, dashtimes$after, mu = 0, paired = TRUE, alternative = "greater")

#Since, P-Value = 0.5821 > 0.05, we failed to reject Ho. There is not significant evidence that the training program improved sprint times.


#b)	Obtain the paired differences (“before” minus “after”) and test the differences for normality. State the conclusion.

differences <- dashtimes$before - dashtimes$after

shapiro.test(differences)
#Since P-Value = 0.2367 > 0.05, we failed to reject Ho. There evidence that the distributions are approximately normal.

