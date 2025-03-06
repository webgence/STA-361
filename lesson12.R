#lesson 12

data(iris)
head(iris)

table(iris$Species)
## say Y = Petal.Length; X = Species

?aov

anova1 <- aov(Petal.Length ~ Species, data = iris)
anova1

summary(anova1)
anova(anova1) ##same output as above

## H0: mu_s = mu_ve = mu_vi 
## Ha: not all equal
## with test statistic 1180.2 and pvalue < .0001,
## we reject H0. Atleast one flower species has mean 
## petal length that differs significantly from one other species.


## Test for pairwise differences.
pairwise.t.test(iris$Petal.Length, iris$Species, 
                p.adjust.method = "bonf")

##all pairwise test reject their corresponding Ho.
## no two groups have the same mean.


?TukeyHSD
TukeyHSD(anova1)
##Again, no two groups have the same pop mean

### check for normality of residuals ###
shapiro.test(anova1$residuals)
hist(anova1$residuals)

qqnorm(anova1$residuals)
qqline(anova1$residuals)

### check for constant variance of residuals across groups ###

##plot residuals against fitted values

res <- anova1$residuals
fit <- anova1$fitted.values

plot(x = fit, y= res)

install.packages("car")

library(car)
?leveneTest

leveneTest(Petal.Length ~ Species, data = iris)

##Ho: sigma1^2 = sigma2^2 = sigma3^2
##Ha: not all equal

##reject p-value < .0001, reject Ho. The three flower species do not have the same variance.




### if we don't feel that one-way ANOVA is appropriate, we 
### can instead peform K-W test.

?kruskal.test

## Ho: median1 = median2 = median3
## Ha: not all the equal
kruskal.test(Petal.Length ~ Species, data = iris)

pairwise.wilcox.test()

##suppose we can to compare species 1 (se) and 2 (ve) but the assumption of the t-test are violated.

## Ho: mu_1 = mu_2
## Ha: mu_1 != mu2

?wilcox.test

## Ho: median1 = median2
## Ha: median1 != median2

kruskal.test(Petal.Length ~ Species, data = iris)

pairwise.wilcox.test(iris$Petal.Length, iris$Species,
                     p.adjust.method = "bonf")
















