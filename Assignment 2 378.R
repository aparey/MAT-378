house.data = read.csv("HouseDataa.txt")
names(house.data)
print(house.data$Price)
print(house.data$Town)

vehicle.data = read.table("VehicleData.txt")
names(vehicle.data)
print(vehicle.data$Region)
print(vehicle.data$C1)
print(vehicle.data$C2)
print(vehicle.data$C3)
print(vehicle.data$C4)
print(vehicle.data$C5)
print(vehicle.data$C6)

# QUESTION 1 #
town_A <- house.data$Price[house.data$Town == 0]
town_B <- house.data$Price[house.data$Town == 1]
print(town_A)
print(town_B)

# normality assumptions check with Shapiro-Wilk test
shapiro.test(town_A)
shapiro.test(town_B)

#two-sample t-test

t_test <- t.test(town_A, town_B, alternative = "two.sided", conf.level = 0.95)
t_test

if(t_test$p.value <= 0.5){
  print("Reject the null hypothesis. There is a significant difference.")
} else {
  print("Don't reject the null hypothesis. There is no significant difference")
}
#The average price of houses in Town A and B have a significant difference. We reject the null hypothesis.



#QUESTION 2#
# 99% confidence interval from the t-test result
t_test <- t.test(town_A, town_B, alternative = "two.sided", conf.level = 0.99)
t_test

# Print the confidence interval
print(t_test$conf.int)
#we are 99% confident that the true difference 
#in population average house prices between Town A and Town B falls
#within the interval from -4288.977 to 1173.251.



#QUESTION 3#
vehicle_type <- c(84,97,105)
prop <- 1/3

total_vehicle <- sum(vehicle_type)
exp_vehicles <- prop*total_vehicle
print(total_vehicle)
print(exp_vehicles)

vehicle_type = c(84,97,105)
result <- chisq.test(vehicle_type)
result

#chi_sq <- ((vehicle_type - exp_vehicles)^2) / exp_vehicles
#chi_sq

#chi_sq_stat <- sum(chi_sq)
#chi_sq_stat

#dof <- length(exp_vehicles) - 1
#dof

#p_value <- 1 - pchisq(chi_sq_stat,df = dof)
#p_value

alpha <- 0.05
if (p_value <= alpha) {
  print("Conclusion: Reject the null hypothesis. There is a statistically significant difference in vehicle type proportions")
} else {
  print("Conclusion: Fail to reject the null hypothesis. There is no statistically significant difference in vehicle type proportions.")
}
#This will perform the chi-squared test for independence, calculate the test statistic, degrees of freedom, and p-value, and make a conclusion based on the significance level (α = 0.05).


#QUESTION 4#

vehicle_type <- c(12, 30, 39, 24, 25, 27)

# Perform the chi-squared test
result <- chisq.test(vehicle_type)

# Print the result
print(result)

alpha <- 0.1
if (result$p.value <= alpha) {
  print("Conclusion: Reject the null hypothesis. There is a statistically significant difference in the distribution of customer satisfaction ratings between the two locations")
} else {
  print("Conclusion: Fail to reject the null hypothesis. There is no statistically significant difference in the distribution of customer satisfaction ratings between the two locations")
}


# QUESTION 5 #
vehicle.data = read.table("VehicleData.txt")

comp <- c("C1", "C2", "C3", "C4", "C5", "C6")

# Number of companies
n <- length(comp)

# Significance level
alpha <- 0.05

# Bonferroni and Šidák correction formulae
bonferroni <- alpha / n
sidak <- 1 - (1 - alpha)^(1 / n)

# Loop for every company's sidak and bonferroni correction
for (company in comp) {
  cat("Company:", company, "\n")
  
  for (region in unique(vehicle.data$Region)) {
    subset_data <- subset(vehicle.data, Region == region)
    t_test <- t.test(subset_data[, company])
    
    p_value <- t_test$p.value
    sig_bonferroni <- p_value <= bonferroni
    
    p_value_sidak <- t_test$p.value
    sig_sidak <- p_value_sidak <= sidak
    
    cat("  Region:", region, "\n")
    cat("  P-value (Bonferroni):", p_value, "\n")
    cat("  Significant (Bonferroni):", sig_bonferroni, "\n")
    
    cat("  P-value (Šidák):", p_value_sidak, "\n")
    cat("  Significant (Šidák):", sig_sidak, "\n\n")
  }
}




