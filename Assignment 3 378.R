getwd()
house.data <- read.table("HW4HouseData.txt", header = TRUE)
names(house.data)

## least-squares regression model ##
Regmodel.house <- lm(Price ~ HouseSize + LotSize + Pool, data = house.data)
summary(Regmodel.house)

shapiro.test(Regmodel.house$residuals)
qqnorm(resid(Regmodel.house))
qqline(resid(Regmodel.house))

par(mfrow = c(2,2))
plot(Regmodel.house)

residuals <- resid(Regmodel.house)

# Create a sequence of numbers representing observation order
observation_order <- 1:length(residuals)

# Create a scatterplot of residuals vs observation order
plot(observation_order, residuals, xlab = "Observation Order", ylab = "Residuals", main = "Residuals vs Observation Order")
abline(h = 0, col = "red")  

summary(Regmodel.house)$coefficients["HouseSize", c("t value", "Pr(>|t|)")]
summary(Regmodel.house)$coefficients["LotSize", c("t value", "Pr(>|t|)")]
summary(Regmodel.house)$coefficients["PoolYes", c("t value", "Pr(>|t|)")]




new_data <- data.frame(HouseSize = 2075, LotSize = 19450, Pool = "Yes")
predicted_price <- predict(Regmodel.house, newdata = new_data)
predicted_price

predict(Regmodel.house, newdata = new_data, interval = "confidence", level = 0.90)

intervals <- confint(Regmodel.house, level = 0.90)
intervals

## Q2 ##
q2.data <- read.table("HW4Q2Data.txt", header = TRUE)
names(q2.data)

model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = q2.data)
summary(model)

Regmodel.house <- lm(y~ x1 + x2 - x3 - x4 + x5, data = q2.data)
summary(model)

shapiro.test(model$residuals)
qqnorm(resid(model))
qqline(resid(model))

par(mfrow = c(2,2))
plot(model)

residuals <- resid(model)

# Create a sequence of numbers representing observation order
observation_order <- 1:length(residuals)

# Create a scatterplot of residuals vs observation order
plot(observation_order, residuals, xlab = "Observation Order", ylab = "Residuals", main = "Residuals vs Observation Order")
abline(h = 0, col = "red")  

#  full regression model
model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = q2.data)

#  summary of the model
model_summary <- summary(model)

f_statistic <- model_summary$fstatistic[1]
p_value <- pf(f_statistic, df1 = model_summary$fstatistic[2], df2 = model_summary$fstatistic[3], lower.tail = FALSE)
f_statistic
p_value

##library(MASS)
# Start with an intercept-only model
MultiReg.empty <- lm(y ~ 1, data = q2.data) 
stepForward1 = add1(MultiReg.empty, scope = q2.data, test = "F", trace=TRUE)
stepForward1

#x1 has the highest significant F-value
MultiReg.empty2 <- lm(y ~ x1, data = q2.data) 
stepForward2 = add1(MultiReg.empty2, scope = q2.data, test = "F", trace=TRUE)
stepForward2

#x2 has the highest significant F-value
MultiReg.empty3 <- lm(y ~ x1+x2, data = q2.data) 
stepForward3 = add1(MultiReg.empty3, scope = q2.data, test = "F", trace=TRUE)
stepForward3

#x5 has the highest significant F-value
MultiReg.empty4 <- lm(y ~ x1+x2+x5, data = q2.data) 
stepForward4 = add1(MultiReg.empty4, scope = q2.data, test = "F", trace=TRUE)
stepForward4

shapiro.test(MultiReg.empty4$residuals)
qqnorm(resid(MultiReg.empty4))
qqline(resid(MultiReg.empty4))

plot(MultiReg.empty4$fitted.values,rstandard(MultiReg.empty4))

par(mfrow=c(2,2))
plot(MultiReg.empty4)
par(mfrow=c(1,1))

##library(car)
vif(MultiReg.empty4)
coefficients <- coef(MultiReg.empty4)
coefficients



#backward elimination
# Fit a full regression model with all predictors
MultiReg.full <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = q2.data)

# Perform backward selection to drop the least significant predictors (x3 and x4)
stepBack1 <- drop1(MultiReg.full, scope = ~ x1 + x2 + x3 + x4 + x5, test = "F", trace = TRUE)
stepBack1

# Remove the least significant predictors (x3 and x4) from the linear model
MultiReg.full2 <- lm(y ~ x1 + x2 + x5, data = q2.data)
stepBack2 <- drop1(MultiReg.full2, scope = ~ x1 + x2 + x5, test = "F", trace = TRUE)
stepBack2


vif(MultiReg.full2)
coefficients2 <- coef(MultiReg.full2)
coefficients2

# this is essentially the same model as the forward selection so we don't 
# need to verify the assumptions


#stepwise elimination
##chooseCRANmirror(graphics = FALSE)
##install.packages("Matrix", type = "binary")
##install.packages("MatrixModels", type = "binary")
##install.packages("rms")
##library(rms)
##

MultiReg.stepwise=ols(y~x1+x2+x3+x4+x5,data=q2.data)
stepStepwise=fastbw(MultiReg.stepwise,rule="p")
stepStepwise

MultiReg.stepwise2=lm(y~x2+x5,data=q2.data)
summary(MultiReg.stepwise2)

shapiro.test(resid(MultiReg.stepwise2))
par(mfrow=c(2,2))
plot(MultiReg.stepwise2)
par(mfrow=c(1,1))

##library(car)
vif(MultiReg.stepwise2)
coefficients3 <- coef(MultiReg.stepwise2)
coefficients3




