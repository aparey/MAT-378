# Set the working directory to the location of your data file
getwd()

# Read the data from a text file (adjust the file path and format as needed)
house.data <- read.table("HouseReg.txt")

# Check the column names in the dataset
names(house.data)

# Linear regression
reg.model <- lm(price ~ size, data = house.data)
summary(reg.model)

# Load the "psych" library for descriptive statistics (if not already loaded)
library("psych")

# Descriptive statistics for "price" and "size"
describe(house.data$price)
describe(house.data$size)

# Calculate the estimated slope coefficient (est.b1)
est.b1 <- cor(house.data$price, house.data$size) * sd(house.data$price) / sd(house.data$size)
est.b1

# Calculate the estimated intercept (est.b0)
est.b0 <- mean(house.data$price) - est.b1 * mean(house.data$size)
est.b0

# Create a scatter plot of price against size with the regression line
plot(house.data$size, house.data$price)
abline(reg.model, col = "red")

# Normality tests for residuals
resid(reg.model)
shapiro.test(reg.model$residuals)
qqnorm(resid(reg.model))
qqline(resid(reg.model))

#Residual vs Predicted graphs
residuals <- resid(reg.model)
predicted_values <- predict(reg.model)

# Create the residuals vs. predicted values plot
plot(predicted_values, residuals, xlab = "Predicted Values", ylab = "Residuals", main = "Residuals vs Predicted Values")
abline(h = 0, col = "red")

#Independence
# Get the residuals from the model
residuals <- resid(reg.model)

# Create a sequence of numbers representing observation order
observation_order <- 1:length(residuals)

# Create a scatterplot of residuals vs observation order
plot(observation_order, residuals, xlab = "Observation Order", ylab = "Residuals", main = "Residuals vs Observation Order")
abline(h = 0, col = "red")  


# Diagnostic plots
plot(reg.model$fitted.values, rstandard(reg.model))

# Set up a 2x2 grid for diagnostic plots
par(mfrow = c(2, 2))
plot(reg.model)

# Influence measures
infl <- influence.measures(reg.model)
summary(infl)

# Summary of the regression model
summary(reg.model)

# Confidence intervals for coefficients
confint(reg.model, level = 0.95)

# Fit the regression model again (this step is redundant, as you've already fitted the model)
reg.model <- lm(price ~ size)

# Create a data frame for prediction
prediction.value <- data.frame(size = 1730)

# Predict the price for a house with 1730 square feet
predict(reg.model, prediction.value)

# Predict with a 90% confidence interval
predict(reg.model, prediction.value, interval = "confidence", level = 0.90)

# Predict with a 98% prediction interval
predict(reg.model, prediction.value, interval = "prediction", level = 0.98)

# Predict with a 95% prediction interval
predict(reg.model, prediction.value, interval = "prediction", level = 0.95)

# Confidence intervals for coefficients
confint(reg.model)
