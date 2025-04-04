"""
1. A cancer study compared two-year remission status (1 = remission, 0 = cancer returned) of 
patients in two different treatment groups: one group was treated surgically, 
while the other group was treated with radiation therapy. The data appear below.
"""

#a)	Create a labeled matrix containing the counts above.

sample1 <- matrix(c(21, 2, 15, 3), nrow = 2, byrow = TRUE)

rownames(sample1) <- c("Surgery", "Radiation")
colnames(sample1) <- c("Remission", "Cancer Returned")
sample1

contingency_table <- as.table(sample1)
contingency_table



#b)	Estimate RR, the relative risk for two-year remission, for surgery relative to radiation. 
#Do this manually, by creating two objects that store π ̂_1 and π ̂_2. Leave a comment interpreting (RR) ̂.

surgery_remission <- 21
surgery_total <- 23
radiation_remission <- 15
radiation_total <- 18

pi_hat1 <- surgery_remission / surgery_total 
pi_hat2 <- radiation_remission / radiation_total 

RR <- pi_hat1 / pi_hat2

pi_hat1
pi_hat2
RR
#with  RR = 1.095652 means that patients trated with surgery are about 9.6% more 
#likely to achieve remission compared to those treated with radiation

#c)	Use the riskratio(.) function to reproduce (RR) ̂, and test whether RR differs from 1.
#Be careful which p-value you report. Leave a comment stating the conclusion of the test.
install.packages("epitools")
library(epitools)
riskratio(contingency_table, rev="row")

"""
p.value
           NA
two-sided   midp.exact fisher.exact chi.square
  Radiation         NA           NA         NA
  Surgery    0.4861822    0.6384258  0.4389008
  
well since all the p-values > 0.05, we fail to reject H0. There is no significant difference between the
remission rates of the the groups that had radiation and surgery.

"""

#d)	Give a 90% confidence interval for the relative risk.

riskratio(contingency_table, rev = "row", conf.level = 0.90)

# (0.1274433 , 2.135944)


"""
2. Consider the data below, which represent a set of vehicles tracked during a given month. 
The response variable is whether the driver of the vehicle was given a speeding ticket (1) or 
not (0). We wish to know whether ticket issuance has anything to do with vehicle color.
"""

#a)	Define a 3×2 matrix containing the counts given above and apply appropriate row and column labels.

ticket_issue <- matrix(c(43, 7, 21, 12, 26, 3), nrow = 3, byrow = TRUE)
rownames(ticket_issue) <- c("Blue", "Red", "Silver") 
colnames(ticket_issue) <- c(0, 1) 
ticket_issue

#b)	Write a comment stating the hypotheses to be tested.

#yes because why is red cars more likely to get ticketed as compared to the other cars.

#c)	Apply the chi-square test for association. State the p-value and explain the conclusion.

(chisq.test(ticket_issue))

#with p-value = 0.01468 < 0.05, we reject H0. 
#There is significant evidence between vechicle color and ticvket issuance


#d)	Obtain the expected number of tickets issued to red cars. 
#How does this compare to the observed number of tickets issued to red cars?
chisq.test(ticket_issue)$expected
expected_value <- chisq.test(ticket_issue)$expected[2,2]
expected_value

observed_tickets <- ticket_issue[2,2]  
observed_tickets

#With expected number of tickets issued to red cars = 6.482143 and the observed value = 12.
#Since the observed value is higher than expected, it suggest that red cars are ticketed more 
#frequently than expected under independence.


#e)	Examine the table of standardized residuals and explain the nature of the association between 
#vehicle color and ticket issuance.

residuals <- chisq.test(ticket_issue)$stdres
residuals

"""
Based off the table of standardized residuals blue cars residuals are close to 1 
which means there's no strong deviation from expected values and blue cars received 
about as many tickets as expected under independence.

Red cars residuals are large, which suggest that red cars receive more tickets under independence and 
receive fewer no tickets then expected.
There is a strong association between red cars and ticket issuance.


Silver cars residuals are close to 1 
which means there's no strong deviation from expected values and silver cars received 
about as many tickets as expected under independence.
"""



#f)	Apply Fisher’s exact test, and comment on whether the results are consistent with the chi-square test.
fishing <- fisher.test(ticket_issue)
fishing


#Fisher p-value = 0.02052, chi-square p-value = 0.01468.
#The p-value is slightly off but hypthesis test outcome is still the same.

##with p-value = 0.02052 < 0.05, we reject H0. 
#There is significant evidence between vechicle color and ticket issuance





