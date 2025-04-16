"""
1. Experiment with generating some categorical data.
	Set the randomization seed to 4444 and simulate 80 individual flips of a fair coin:
Y=□({█(1  if Heads@0  if Tails   )┤ )
	Print your vector of length 80.

"""

#a
set.seed(4444)

Y <- c(rbinom(n=80, size=1, prob = 0.5))
Y

#b	Summarize the binary data in count form, then give an estimate of π=P(Y=1). 
#Comment on whether it would be appropriate to perform a hypothesis test based on the normal approximation.

table(Y)

(pi_hat <- sum(Y)/80)
#π = 0.5625 which is close to prob = 0.5, so using hypothesis test will be appropriate


#c	Test the null hypothesis that the coin is fair against the two-sided alternative using α=0.05.

#H0: p = 0.5
#Ha: p != 0.5


binom.test(x = sum(Y), n = 80, p=0.5, alternative = "two.sided")
#with p-value = 0.3143, we failed to reject H0. 
#The population success probability is significant different from 0.5


#d	Give a 90% confidence interval for π.
binom.test(x = sum(Y), n = 80, p=0.5, alternative = "two.sided", conf.level = 0.90)

#90 percent confidence interval: 0.4644424 0.6569875



"""
2. A medical study is carried out in which one group of patients are given an experimental treatment,
while another group of patients are given a sugar pill. 
In the treatment group, 17 patients improved, while 26 patients did not improve. 
In the placebo group, 8 patients improved, while 33 patients did not improve.
"""

#a	Define a 2×2 matrix that expresses the counts in this study. 
#Be sure to properly orient the grouping variable and the response variable. 
#After defining your matrix, use the rownames(.) and colnames(.) functions to label your matrix for clarity.


medical_study <- matrix(c(17, 26, 8, 33), nrow = 2, byrow = TRUE)
rownames(medical_study) <- c("Treatment", "Placebo")
colnames(medical_study) <- c("Improved", "Not Improved")
medical_study

#b	Obtain the values of π ̂_1 and π ̂_2, declaring two objects to store them.

pihat1 <- 17 / (17 + 26)

pihat2 <- 8 / (8 + 33)
pihat1
pihat2



#c	The new treatment will be declared effective if it shows a response rate that is larger than the 
#response rate in the placebo group. Perform the following test using α=0.05:

#H_0: π_1-π_2≤0
#H_a: π_1-π_2>0

successes <- c(17, 8) 
totals <- c(17 + 26, 8 + 33) 
prop.test(successes, totals, alternative = "greater")

"""
sample estimates:
   prop 1    prop 2 
0.3953488 0.1951220 
matches pihat1 and pihat2

With p-value = 0.03857 < 0.05, we reject H0. The treatment is significantly higher response rate than placebo.


"""


#d)	Argue whether it is appropriate to make inferences based on a normal approximation.


# Inference procedure relies on having “large enough” sample sizes, allowing
#the Central Limit Theorem to take effect


#check if the sample size conditions we used earlier are met for both groups:
# 𝑛1𝜋̂1 ≥ 5 and 𝑛1(1 − 𝜋̂1) ≥ 5
# 𝑛2𝜋̂2 ≥ 5 and 𝑛2(1 − 𝜋̂2) ≥ 5

treatment_success <- (17 + 26) * pihat1
treatment_failure <- (17 + 26) * (1 - pihat1)

placebo_success <- (8+33) * pihat2
placebo_failure <- (8+33) * (1 - pihat2)


treatment_success
treatment_failure
placebo_success 
placebo_failure

#Since all are greater then 5, Normal approximation is appropriate.

