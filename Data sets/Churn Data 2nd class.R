## A description of the data set and the variables are found on page 54-55
churn.data<-read.csv("churn.txt")
names(churn.data)

summary(churn.data)
boxplot(churn.data$Day.Mins)
boxplot(churn.data[,c(8,9,11,12,13)])
hist(churn.data$Day.Mins)
plot(churn.data$Day.Mins,churn.data$Eve.Mins,
col=ifelse(churn.data$Churn.=="True.","Black","Green"))
legend("topleft", c("True","False"),fill=c("Black","Green"),title="Churn")

plot(churn.data$Day.Mins,churn.data$Eve.Mins,
     col=ifelse(churn.data$Churn.=="True.","Black","Green"),
     pch=ifelse(churn.data$Churn.=="True.",1,4))
legend("topleft", c("True","False"),fill=c("Black","Green"),title="Churn")

