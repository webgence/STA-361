### Midterm Exam Topics

#Hw 1-3 and Notes 1-6

#Basic R object
#Classes of object
#Help files for unfamiliar functions
#installing and using R packages
#Importing data sets 
#Ordinary and matrix algebra
#Use of logical operators
#use of while/for loops
#operations on data frames
#basic plotting
#functions relating to probability distributions(<d>, <p>, <q>, <r>)
### distinctions between dsicrete and continous RV's
### shapiro-wilk test


#Toss 10 coins. Find P(Y = 5) for different probablities of success.

prob_vec <- seq(.1, .9, by = .1)
result <- numeric(length(prob_vec))

for(i in 1:length(prob_vec)){
  result[i] <- dbinom(5, 10, prob_vec[i])
}
  
result


init <- 1
counter <- 1

while(init < 10000000){
  init <- 2 * init
  counter <- counter + 1
  
}


init
counter



  
  
  
  
  
  
  