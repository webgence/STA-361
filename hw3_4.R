install.packages("ggplot2")
library(ggplot2)

"""
4.  Download the script hw3_4.R from UBLearns. The script creates a vector of length 25 (called “ddd”), 
and stores these values in a data frame called “df_1.” (6 pts)
"""

"""
a.	Use ggplot(.) to produce a Q-Q plot that compares the observed quantiles of the sample (“ddd”) to theoretical quantiles 
	from a χ_10^2 distribution. Include a reference line, also based on χ_10^2. (1)
"""

ddd <- c(3.542, 8.776, 5.89, 7.034, 9.897, 13.317, 7.734, 
         8.879, 8.686, 12.171, 9.914, 5.129, 5.228, 5.459,
         4.499, 9.201, 4.903, 22.175, 12.569, 31.19, 19.321,
         4.824, 14.144, 9.582, 10.03)
df_1 <- data.frame(ddd)

df_1


ggplot(df_1, aes(sample = ddd)) +
  stat_qq(distribution = qchisq, dparams = list(df = 10), color = "red") +
  stat_qq_line(distribution = qchisq, dparams = list(df = 10), color = "Chartreuse") +
  labs(title = "Q-Q Plot: Observed vs. Theoretical Chi-Square (df=10)",
       x = "Theoretical Quantiles(Chi-Square (df=10))",
       y = "Observed Quantiles")



"""
b)	Examine the help file for the overlay stat_qq_line(.). Pay particular attention to the argument “line.p.” 
Explain whether the default behavior described in the help file aligns with the description of qqline(.) in the Part 5 notes. (1)
"""
?stat_qq_line(.)

ggplot(df_1, aes(sample = ddd)) +
  stat_qq(distribution = qchisq, dparams = list(df = 10), color = "red") +
  stat_qq_line(distribution = qchisq, dparams = list(df = 10), color = "Chartreuse") +
  labs(title = "Q-Q Plot: Observed vs. Theoretical Chi-Square (df=10)",
       x = "Theoretical Quantiles(Chi-Square (df=10))",
       y = "Observed Quantiles")

ggplot(df_1, aes(sample = ddd)) +
  stat_qq(distribution = qchisq, dparams = list(df = 10), color = "red") +
  qqline(distribution = qchisq, dparams = list(df = 10), color = "Chartreuse") +
  labs(title = "Q-Q Plot: Observed vs. Theoretical Chi-Square (df=10)",
       x = "Theoretical Quantiles(Chi-Square (df=10))",
       y = "Observed Quantiles")

"""
The default behavior of stat_qq_line aligns with qqline because stat_qq_line uses line.p = c(0.25, 0.75), 
this means that it fits the line through the 25th and 75th percentiles.

qqline also fits the line through the first and thrid quartiles.
"""


"""
c)	Add four reference lines to your plot from part (a). The two vertical lines (give them a color) should be located at 
the true 25th and 75th percentiles of χ_10^2. The two horizontal lines (give them a different color) should be located at 
the sample 25th and 75th percentiles of the data. Explain how the intersections of your reference lines produce the reference 
line from part (a). (4)
"""
q25 <- qchisq(0.25, df = 10)  
q75 <- qchisq(0.75, df = 10)  

h25 <- quantile(df_1$ddd, 0.25)  
h75 <- quantile(df_1$ddd, 0.75) 


ggplot(df_1, aes(sample = ddd)) +
  stat_qq(distribution = qchisq, dparams = list(df = 10), color = "red") +
  stat_qq_line(distribution = qchisq, dparams = list(df = 10), color = "Chartreuse") +
  geom_vline(xintercept = q25, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = q75, color = "purple", linetype = "dashed") +
  geom_hline(yintercept = h25, color = "violet", linetype = "dashed") +
  geom_hline(yintercept = h75, color = "brown", linetype = "dashed") +
  labs(title = "Q-Q Plot: Observed vs. Theoretical Chi-Square (df=10)",
       x = "Theoretical Quantiles(Chi-Square (df=10))",
       y = "Observed Quantiles")



#The intersection of the vertical and horizontal lines shows how the sample quantiles align with the theoretical quantiles.






