## For this script I've printed 20 random values using a random number generater. Afterwards, I've plotted the values in a histogram

set.seed(445566)
random_data <- rnorm(20)
print(random_data)


## Create Histogram
hist(random_data) 
