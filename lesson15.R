

?rbinom
#"prob" if fixed probability of success in a single tail.
#"size" the number of binary events that are part of your experiments
#"n" the number of realizations of your specified binomial(size, p)

rbinom(n=1, size=10, prob = 0.5)
#summary output from tossing 10 coins

rbinom(n=10, size=1, prob = 0.5)
#Show all 10 outcomes when size is between 0-1


###1 sample proption

#imagine collecting survey data (0/1) from 1000 people
prob_Y <- 0.07
set.seed(645)
Y <- rbinom(n=1000, size=1, prob = prob_Y)
Y

table(Y)#law of larage numbers

p_hat <- sum(Y) / length(Y)
p_hat
#phat is smaple proportion

#H0: p = 0.10
#Ha: p != 0.10


?prop.test
prop.test(x = 71, n = 1000, p=0.1)
#with p-value = 0.002663, we reject H0. The population success probability is significant different from 0.10

mult <- rbinom(n= 10000,size=1000, prob = prob_Y)
hist(mult)

?binom.test
binom.test(x = 71, n = 1000, p=0.1)
# with p-value = 0.0018, we still reject H0

#consider using an exact test if n * phat < 5 (same as: is x < 5)



##2 sample proption

p1 <- 0.22
#group of grymbros
p2 <- 0.18
#ladies
n1 <- 17
n2 <- 31

set.seed(71)
sample1 <- rbinom(n1, size = 1, prob = p1)

sample2 <- rbinom(n2, size = 1, prob = p2)

phat1 <- sum(sample1)/n1

phat2 <- sum(sample2)/n2

phat1
phat2

#H0: p1 = p2
#Ha: p1 != p2
# stack the responses and create a vector of labels

Ystack <- c(sample1, sample2)
labels <- c(rep("M", n1), rep("F", n2))
labels

t <- table(labels, Ystack)
t

prop.test(t)
#this incorrect choose the "success category"

t2 <- matrix(c(4,27, 3, 14), byrow=T, nrow= 2)
rownames(t2) <- c("F", "M")
colnames(t2) <- c('1', '0')
t2

prop.test(t2)
#with p-value = 0.9858, we fail to reject H0. 
#We find no significanrt difference in the success rate for males and females





?rnorm
