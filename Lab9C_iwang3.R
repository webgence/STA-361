#Lab 9C


"""
1. Suppose we observe the following 2×2 table and wish to perform Fisher’s exact test.
 	Success	Failure	 Total
Group 1	10	4	14
Group 2	2	8	10
 Total	12	12	24
	Create a matrix containing the four cell counts above (row/column labels are not necessary). Carry out Fisher’s exact test and store the p-value for future comparison.

"""

table <- matrix(c(10, 4, 2, 8), nrow = 2, ncol = 2, byrow=T)
table

fishing <- fisher.test(table)
fishing

(p_val <- fishing$p.value)

"""
Write a short program to create a  matrix object. The first four lines should define the following four constants, which are not simply the 4 cell counts from above:
	•	 the count in the (1,1) cell.
	•	 the first row total.
	•	 the first column total.
	•	 the grand total.

"""
count <- 10
firstRowTotal <- 14
firstColumnTotal <- 12
grandTotal <- 24


missingVal1 <- firstRowTotal - count #(2,1)
missingVal1
missingVal2 <- firstColumnTotal - count #(1,2)
missingVal2
missingVal3 <- grandTotal - (count +  missingVal1 +  missingVal2)
missingVal3

contingency_table <- matrix(c(count, 
                              missingVal1,
                              missingVal2,
                              missingVal3), nrow = 2, byrow = TRUE)

contingency_table




#Use your program to determine the range of possible values for  when the “marginal” counts are 
#fixed at the observed values (i.e.  and ). Caution: a legitimate  table cannot have any negative counts.

count <- 13
firstRowTotal <- 14
firstColumnTotal <- 12
grandTotal <- 24


missingVal1 <- firstRowTotal - count #(2,1)
missingVal2 <- firstColumnTotal - count #(1,2)
missingVal3 <- grandTotal - (count +  missingVal1 +  missingVal2)

contingency_table1 <- matrix(c(count, 
                              missingVal1,
                              missingVal2,
                              missingVal3), nrow = 2, byrow = TRUE)
                            
contingency_table1

#not 0, 1
#2 - 12 works
#13 has negative


poss <- seq(2, 12)
poss



#d. Obtain probabilities for every possible  table that has the same marginal counts as the observed table, using the hypergeometric distribution:
probs <- dhyper(poss , firstRowTotal, grandTotal - firstRowTotal, firstColumnTotal) #dhyper(n_11,R_1,n-R_1,C_1)
probs

sum(probs)
#sum equal to 1


#e. 
#The test statistic for fisher's exact test is the table probability.
n11 <- 10
R1 <- 14
C1 <- 12
n <- 24
#dhyper(n_11,R_1,n-R_1,C_1)
test_stat <- dhyper(n11, R1, n - R1, C1)
test_stat

pdf <- data.frame(probs)
extreme <- subset(pdf, probs <= test_stat)
(pval_e <- sum(extreme))

#Yes, same p-value of 0.03607484

