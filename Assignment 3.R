getwd()
house.data <- read.table("HouseReg.txt")
names(house.data)

#Linear regression
reg.model <- lm(price ~ size, data = house.data)
summary(reg.model)

library("psych")

describe(house.data$price)
describe(house.data$size)

est.b1 <- cor(house.data$price, house.data$size) * sd(house.data$price)/sd(house.data$size)
est.b1

est.b0 <- mean(house.data$price) - est.b1 * mean(house.data$size)
est.b0

plot(house.data$price, house.data$size)
abline(reg.model, col = "red")

shapiro.test(reg.model$residuals)
qqnorm(resid(reg.model))
qqline(resid(reg.model))

plot(reg.model$fitted.values, rstandard(reg.model))

par(mfrow = c(2,2))
plot(reg.model)

influence.measures(reg.model)
summary(influence.measures(reg.model))

summary(reg.model)

confint(reg.model, level = 0.95)

attach(house.data)
library("psych")
describe(price)

reg.model = lm(price~size)
prediction.value = data.frame(size=1730)
predict(reg.model, prediction.value)

predict(reg.model, prediction.value, interval = "confidence", level = 0.90)

predict(reg.model, prediction.value, interval = "prediction", level = 0.95)

predict(reg.model, prediction.value, interval = "prediction", level = 0.98)

confint(reg.model)


##new_data <- data.frame(Size = 1730)
##predict_interval <- predict(reg.model, new_data, interval="confidence", level=0.90)
##predict_interval

# g) 98% confidence interval for a randomly selected house with 1730 square feet
##predict_interval_98 <- predict(reg.model, new_data, interval="prediction", level=0.98)
##predict_interval_98

# h) 95% confidence interval for the slope coefficient
##confint(reg.model)