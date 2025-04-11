
?mean
#imagine this function doesn't exists and we need to make our own mean function

input <- c(1,3,4,7,5,6,9)
#suppose the mean function is not available

sum(input)#gets sum
length(input)#gets length

avg <- sum(input)/length(input)#gets mean
avg


#now we use the code to build a function
average <- function(num_vec){
  sum <- sum(num_vec)
  n <- length(num_vec)
  sum / n
}


average((c(1,2,3,4)))

mean(c(3,6,9,1,10), bro = 2)


vec1 <- c(1,2,3,4, NA)#vector with a missing value
mean(vec1)
mean(vec1, na.rm = T)
mean(na.rm = T, vec1)

rnorm(10, 0, 1)
rnorm(sd = 1, n = 10, mean = 0)







