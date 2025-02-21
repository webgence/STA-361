#1. Import the data set beers.txt (UBLearns).  

"""
a.	Create a scatter plot that places the number of beers consumed (of course by adults at an 
event where they have arranged for proper transportation home) on the x-axis, and blood alcohol 
content on the y-axis.
"""
beers <- read.csv("~/Documents/STA 361/beers.txt", sep="")
View(beers)
head(beers, 2)

xvar <- beers$Quantity  
yvar <- beers$BAC      

plot(xvar, yvar, xlab="Quantity", ylab="BAC", main="Beer Consumption vs BAC")


"""
b.	In New York State, a driver found to have blood alcohol content between 0.05 and 0.07 is 
considered “driving while ability impaired.” Add two blue dashed horizontal lines to the plot 
to indicate the ability impaired range.
"""

plot(xvar, yvar, xlab="Quantity", ylab="BAC", main="Beer Consumption vs BAC")

abline(h=0.05, col="blue", lty=2)  
abline(h=0.07, col="blue", lty=2)  


"""
c.	If BAC is found to be 0.08 or higher, the driver can be charged with driving while intoxicated 
(DWI); presumably, the police office rounds the observed BAC reading to the nearest hundredth. 
Add a solid horizontal red line to the plot indicating the DWI 
"""

plot(xvar, yvar, xlab="Quantity", ylab="BAC", main="Beer Consumption vs BAC")

abline(h=0.05, col="blue", lty=2)  
abline(h=0.07, col="blue", lty=2) 
abline(h=0.08, col="red", lty=1, lwd=2) 

"""
d.	Apply custom labels to the x- and y-axes and give the plot a sensible title.
"""

#Already did so...




#2. Due to limitations of the device that measures BAC, we can consider BAC to be a discrete numeric variable.

#a.	Create a barplot using the BAC variable.

barplot(yvar)


#b.	Give the bars a flashy color, title the axes, and give the plot an overall title.

barplot(yvar, 
        main="Blood Alcohol Content (BAC) Frequency",
        xlab="Number of Alcohol", 
        ylab="Percent Alcohol in Blood", 
        col="blue",
        border="black")




"""
c.	What happens when you try to add a red vertical reference line at 0.08, the DWI threshold? 
Leave a comment in your script describing what you see.
"""

abline(v=0.08, col="red", lwd=2)


#The graph now has a red solid line at 0.08 threshold.



"""
d.	Use the width argument of barplot(.) to make sure that every bar has a width of 0.01. 
Also set the space argument to 0, so that consecutive bars touch each other.
"""
barplot(yvar, 
        main="Blood Alcohol Content (BAC) Frequency",
        xlab="Number of Alcohol", 
        ylab="Percent Alcohol in Blood", 
        width=0.01,
        space=0,  
        col="blue",
        border="black")

"""
e.	Make another attempt to add the red reference line. 
Leave another comment describing what happened.
"""


barplot(yvar, 
        main="Blood Alcohol Content (BAC) Frequency",
        xlab="Number of Alcohol", 
        ylab="Percent Alcohol in Blood", 
        width=0.01,
        space=0,  
        col="blue",
        border="black")

abline(v=0.08, col="red", lwd=2)


#The red solid line shifted towards the right and is sorta centered now.


"""
3. 
a.	Create a histogram using the BAC variable. Does it seem like the data might be normally distributed? Leave a detailed comment.

"""

hist(yvar, col="hotpink", xlab="Frequency of BAC")
#No it does not seem normally distributed because it's not bell shaped and not symmetrical around the mean.



"""
b.	Create a normal Q-Q plot, including the 45-degree reference line. Does it seem like the 
data might be normally distributed? Leave a detailed comment.
"""

qqnorm(yvar, pch=16, col="burlywood3")
qqline(yvar)

"""
The data is approximately normally distrubted because the points are close to the line. 
However there are some extreme outliers.
"""








