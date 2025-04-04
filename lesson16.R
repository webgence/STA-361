#lesson15
n1 <- 20
n2 <- 30
p1 <- .12
p2 <- .08

set.seed(98)
sample1 <- rbinom(n1, 1, p1)
sample2 <- rbinom(n2, 1, p2)
sample1
sample2

(phat1 <- sum(sample1) / n1)

(phat2 <- sum(sample2) / n2)

y <- c(sample1, sample2)
labels <- c(rep(1,n1), rep(2, n2))
t <- table(y, labels)

y
labels
t

##last time
#H0: p1 = p2
#Ha: p1 != p2

##today:
#H0: RR = 1.   where RR = p1/p2
#Ha: RR != 1   

(RR_hat <- phat1/phat2)

install.packages("epitools")
library(epitools)

epitab(t, method= "riskratio", rev="rows")

#with p-value = 0.64 > 0.05, we failed to reject Ho. The RR is not significant different from 1.


#odds of an event   = p(success) / (1 - p(success))
#odds of success in group1: p1 / (1 - p1)
#odds of success in group2: p2 / (1 - p2)
#odds ratio of (OR) = (p1 / (1 - p1)) /  (p2 / (1 - p2))
#Ho: OR = 1 
#Ha: OR != 1


(OR_hat <- (phat1/(1-phat1)) / (phat2/(1-phat2)) )
epitab(t, method= "oddsratio", rev="rows")
#with p-value = 0.6411, we fail to reject H0. We lack evidence to claim that the OR is different from 1.


###The chi squared test for Association between two categorical variables

#H0: pullup success rate is independent of group membership
#Ha: pullup success rate is dependent of group membership


?chisq.test
xsq <- chisq.test(t)
#with test statistics = 0.01, p-value = 0.91, we fail to reject H0.
#we do not find pullup success depends on sex (M vs F)

xsq$expected
#a collection of cell counts under true independence

xsq$stdres
#standarizes residual outside +/- 1.96 indicates a cell that 
#contributes heavily to a significant chi-square test statistic


#Fisher's exact test for association between two cat variables
?fisher.test
fisher.test(t)

#H0: pullup success rate is independent of group membership
#Ha: pullup success rate is dependent of group membership

##with p-value = 0.64 > 0.05, we fail to reject H0. 
##We find no significant assiociation between pullup success and sex.













