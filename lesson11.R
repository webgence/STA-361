
## H0: mu = 5
## Ha: mu != 5

##test statistic t* = (Ybar - mu0)/(s / sqrt(n))

n <- 15
y <- rnorm(n, mean = 4, sd = 1)
y
#do these 15 number mean is different from 5

mu0 <- 5
ybar <- mean(y)
ybar

s <- sd(y)
s


t_star <- (ybar - mu0) / (s/sqrt(n))
t_star

pval <- 2 * pt(t_star, n-1)
pval

#with p-value < 0.05, I reject Ho. The mean is statistically different from 5




?t.test
t.test(y, mu = 5, conf.level = .90)

qt(0.95, n-1)
#Critical Value



#two samples: one group of men and one group of female
##Ho: mu_m = mu_f
##Ha: mu_m != mu_f

nM <- 16
nF <- 14

#simulating data
Y_M <- rnorm(nM, 10, 2)

Y_F <- rnorm(nF, 10, 1)


Y_M
Y_F

t.test(Y_M, Y_F)
## with p-value 0.5881, failed to reject Ho. I lack evidence to claim that MuF and MuM are statistically different.



##What if all responses are in the same vector?
Y <- c(Y_F, Y_M)
Y  

labels <- c(rep('F', nF), rep('M', nM))
labels


df <- data.frame(Y, labels)
df

t.test(Y ~ labels, data=df, mu = 0)  
  
  
  
boxplot(df$Y ~ df$labels)





##generate data for paired t-test  
## Paired t-test: Ho: mu_D = 0
##                Ha: mu_D != 0 

n_pairs <- 20
Y_pre <- rnorm(20, 165, 7)
y_post <- rnorm(20, 158, 5)

t.test(Y_pre, y_post, mu = 0, paired=T)
  ## With p-value < 0.05, we reject Ho. The mean weight 
  ## in the pregroup is statistically different from the mean from the Post group.
  
  









