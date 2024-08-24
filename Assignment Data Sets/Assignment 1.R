# ASSIGNMENT 1 MAT 378 #
# Aditya Parey #
# aparey@oswego.edu#

#current_timestamp <- Sys.time()
#print(current_timestamp)
##[1] "2023-09-19 15:14:29 EDT"##



house.data = read.csv("HouseData.txt", sep = ",")
names(house.data)

# Using the print function to display the "Pool" column
print(house.data$Pool)
print(house.data$Price)

#QUESTION 1#
pool_count <- table(house.data$Pool)
# Print the counts
print(pool_count)

#QUESTION 2#
#nrow is the total number of houses#
#sum function gives out number of houses with or without pool#
# Calculate the proportion of houses with a pool
prop_with_pool <- sum(house.data$Pool == 1) / nrow(house.data)

# Calculate the proportion of houses without a pool
prop_without_pool <- sum(house.data$Pool == 0) / nrow(house.data)

# Print the proportions
print(prop_with_pool)
print(prop_without_pool)

#QUESTION 3#
# bar plot # 
barplot(pool_count, ylim=c(0,100),
        main = "Houses with and without a Pool",
        xlab = "Pool (0 = No, 1 = Yes)",                           
        ylab = "Count")                                            

#QUESTION 4#
# histogram#
library(psych)
hist(house.data$Price,
     xlab = "Price",
     main = "Distribution of House Prices")

#QUESTION 5#
#  a boxplot of the "Price" variable#
boxplot(house.data$Price,
        ylab = "Price",                   
        main = " House Prices")  

#QUESTION 6#
#description and summary the Price variable#
describe(house.data$Price)
summary(house.data$Price)

#QUESTION 7#
#We will use the test of proportion for this question#
#We have a null hypothesis where p is equal or greater than 0.5 and alternative hypothesis where
# p is less than 0.5, where alpha = 0.05#
#next up we will calculate the test statistic by inserting the formula#
#then we can get the p value by using pnorm function to derive conclusions#
sample_prop = mean(house.data$Pool)
print(sample_prop)

#Calculate the test stat#
sample_size <- length(house.data$Pool)
population_prop <- 0.5
test_stat <- (sample_prop - population_prop) / sqrt((population_prop * (1 - population_prop)) / sample_size)
print(test_stat)

## Calculate the p-value for a one-side test of proportion#
p_value <- pnorm(test_stat)
print(p_value)


#QUESTION 8#
# the critical value from the standard normal distribution for a 98% confidence interval
critical_value <- qnorm(0.99)  # 0.01 on each tail for a 98% confidence level

# the margin of error
error <- critical_value * sqrt((sample_prop * (1 - sample_prop)) / sample_size)

# the lower and upper bounds of the confidence interval
lower_bound <- sample_prop - error
upper_bound <- sample_prop + error

#s the confidence interval
cat("98% Confidence Interval for Proportion of Houses with a Pool: (", round(lower_bound, 4), ", ", round(upper_bound, 4), ")\n")

#QUESTION 9#

# Create a Q-Q plot to check normality#
qqnorm(house.data$Price)
qqline(house.data$Price)

# the Shapiro-Wilk test to check for normality#
shapiro.test(house.data$Price)

#  p-value being greater than 0.05, we fail to reject the null hypothesis (H0),
#which means that we do not have sufficient evidence 
#to conclude that the "Price" variable significantly varies from a normal distribution. 
#This suggests that the "Price" variable follows a normal distribution #


#QUESTION 10 #

# Set the hypothesized population mean
population_mean <- 278000 

# Perform a one-sample t-test
Price_test <- t.test(house.data$Price, mu = population_mean, alternative = "greater")
Price_test

# we fail to reject the alternative hypothesis and conclude that the price is greater than $278,000
#the p-value of approximately 0.007889, which is less than the chosen significance level of alpha = 0.05, we would reject the null hypothesis #


#QUESTION 11#
#  one-sample t-test to calculate the confidence interval
Price_CI <- t.test(house.data$Price, conf.level = 0.90)
Price_CI

# the confidence interval for Price data would range between ( 278406.3 to 280093.8) with a 90% confidence #









