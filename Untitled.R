tree.data <- read.table("TreeCrossValidation.txt")
dim(tree.data)
summary(tree.data)

table.type = table(tree.data$type)
hyp.p = c(.2,.4,.1,.3)
obs.freq = c(table.type[1],table.type[2],table.type[3],table.type[4])

chisq.dataGOF = chisq.test(obs.freq, p=hyp.p)
chisq.dataGOF

set.seed(11)
partition.ind = sample(length(tree.data$type),
                       size = round((.7*length(tree.data$type))))

training.data = tree.data[partition.ind,]
test.data = tree.data[-partition.ind,]

dim(training.data)
dim(test.data)

# is there evidence at the alpha=0.2 that there is a diff in the pop of each type in the test vs training set

training.table = table(training.data$type)
test.table = table(test.data$type)

data.table = as.table(rbind(training.table, test.table))

chisq.data = chisq.test(data.table)
chisq.data

## since p value is greater than alpha we fail to reject Ho. the evidence doesnt support the a sig diff in the proportions of the tree types across the training and test set. 


getwd()
############# regression models ##############
tree.data <- read.table("TreeCrossValidation.txt")
names(tree.data)

tree.dataD = subset(tree.data, tree.data$type== "D")

reg.model = lm(tree.dataD$height~tree.dataD$base)
summary(reg.model)
library("psych")

describe(tree.dataD$height)

est.b1 = cor(tree.dataD$height, tree.dataD$base) * sd(tree.dataD$height)/sd(tree.dataD$base)
est.b1

est.b0 = mean(tree.dataD$height) ~ est.b1*mean(tree.dataD$base)


plot(tree.dataD$base, tree.dataD$height)
abline(reg.model, col = "red")

shapiro.test(reg.model$residuals)
qqnorm(resid(reg.model))
qqplot(resid(reg.model))

plot(reg.model$fitted.values, rstandard(reg.model))

par(mfrow = c(2,2))
plot(reg.model)

influence.measures(reg.model)
summary(influence.measures(reg.model))

## confidence interval for the regression coefficient ##
confint(reg.model, level = 0.95)

attach(tree.dataD)
library("psych")
describe(base)

reg.model = lm(height~base)
prediction.value = data.frame(base=15)
predict(reg.model, prediction.value)

predict(reg.model, prediction.value, interval = "confidence", level = 0.95)

predict(reg.model, prediction.value, interval = "prediction", level = 0.95)

############### MLR #################
cereals.data <- read.csv("cereals.csv", header = TRUE)
names(cereals.data)
summary(cereals.data)

###Determining the rows that have missing values for the sugars variable and then removing them##
Sugars.missing = which(is.na(cereals.data$Sugars))
Sugars.missing

cereals = cereals.data[-Sugars.missing,]
cereals

MultiReg.cereals = lm(Rating ~ Sugars + Fiber, data = cereals)
##MultiReg.cereals = lm(cereals$Rating ~ cereals$Sugars + cereals$Fiber)
summary(MultiReg.cereals)
  
shapiro.test(MultiReg.cereals$residuals)
qqnorm(resid(MultiReg.cereals))
qqline(resid(MultiReg.cereals))

par(mfrow = c(2,2))
plot(MultiReg.cereals)

par(mfrow = c(1,1))

## construct indicator variable ##

cereals$shelf1 = ifelse(cereals$Shelf==1,1,0)
cereals$shelf2 = ifelse(cereals$Shelf==2,1,0)

stripchart(Rating~Shelf, data=cereals, method="stack", pch = "1",
           col = c("green", "blue", "red"), main = "Ratings by Shelf")

MultiReg.cereals2 = lm(Rating~shelf1+shelf2+Sugars+Fiber, data = cereals)
summary(MultiReg.cereals2)

shapiro.test(resid(MultiReg.cereals2))
qqnorm(resid(MultiReg.cereals2))
qqline(resid(MultiReg.cereals2))

par(mfrow = c(2,2))
plot(MultiReg.cereals2)

datamat = matrix(c(cereals$shelf1, cereals$shelf2, cereals$Sugars, cereals$Fiber), ncol = 4)
cor(datamat)

install.packages("car")
library("car")
##vif("MultiReg.cereals2")

confint(MultiReg.cereals2, level = .95)

attach(cereals)
MultiReg.cereals2 = lm(Rating~Sugars+Fiber+shelf1+shelf2)
prediction.point = data.frame(Sugars=5, Fiber=5, shelf1=0, shelf2=1)
predict(MultiReg.cereals, newdata = prediction.point, interval = "confidence", level = .95)
predict(MultiReg.cereals, newdata = prediction.point, interval = "prediction", level = .95)


##creating a dataset that is a subset of the original dataset containing only the variables to be examine for regression##
cereals2=cbind(Rating,Sodium,Fiber,Carbo,Sugars,Cups,shelf1,shelf2)
cereals2=data.frame(cereals2)

######################################
##Forward selection###################
######################################
library(MASS)
MultiReg.empty=lm(Rating~1,data=cereals)
stepForward1=add1(MultiReg.empty,scope=cereals2,test="F",trace=TRUE)
stepForward1
###sugars has the highest significant F-value###

##add sugars to the linear model##
MultiReg.empty2=lm(Rating~Sugars,data=cereals2)
stepForward2=add1(MultiReg.empty2,scope=cereals2,test="F",trace=TRUE)
stepForward2
##Fiber had the highest significant F-value###

##add fiber to the linear model##
MultiReg.empty3=lm(Rating~Sugars+Fiber,data=cereals2)
stepForward3=add1(MultiReg.empty3,scope=cereals2,test="F",trace=TRUE)
stepForward3
##Sodium had the highest significant F-value##
##add sodium to the linear model##
MultiReg.empty4=lm(Rating~Sugars+Fiber+Sodium,data=cereals2)
stepForward4=add1(MultiReg.empty4,scope=cereals2,test="F",trace=TRUE)
stepForward4
##Shelf 1 has the highest significant F-Value##

##add Shelf 1 to the linear model##
MultiReg.empty5=lm(Rating~Sugars+Fiber+Sodium+shelf1,data=cereals2)
stepForward5=add1(MultiReg.empty5,scope=cereals2,test="F",trace=TRUE)
stepForward5
##Shelf 2 has the highest significant F-Value##

##add Shelf 2 to the linear model##
MultiReg.empty6=lm(Rating~Sugars+Fiber+Sodium+shelf1+shelf2,data=cereals2)
stepForward6=add1(MultiReg.empty6,scope=cereals2,test="F",trace=TRUE)
stepForward6
##Carbo has the highest significant F-Value##

##add Carbo to the linear model##
MultiReg.empty7=lm(Rating~Sugars+Fiber+Sodium+shelf1+shelf2+Carbo,data=cereals2)
stepForward7=add1(MultiReg.empty7,scope=cereals2,test="F",trace=TRUE)
stepForward7

shapiro.test(MultiReg.empty7$residuals)
qqnorm(resid(MultiReg.empty7))
qqline(resid(MultiReg.empty7))

plot(MultiReg.empty7$fitted.values,rstandard(MultiReg.empty7))

par(mfrow=c(2,2))
plot(MultiReg.empty7)
par(mfrow=c(1,1))

library(car)
vif(MultiReg.empty7)


###########################
##Backward selection#######
###########################


MultiReg.full=lm(Rating~.,data=cereals2)
stepBack1=drop1(MultiReg.full,scope=cereals2,test="F",trace=TRUE)
stepBack1
##Cups is the least significant###

##remove cups from the linear model##
MultiReg.full2=lm(Rating~Sugars+Fiber+Sodium+shelf1+shelf2+Carbo,data=cereals2)
stepBack2=drop1(MultiReg.full2,scope=cereals2[,-6],test="F",trace=TRUE)
stepBack2
###we end up with the same model as above we will not verify assumptions again##






############################
###stepwise selection#######
############################
#install.packages("rms")
library(rms)
MultiReg.stepwise=ols(Rating~Sodium+Fiber+Carbo+Sugars+Cups+shelf1+shelf2,data=cereals2)
stepStepwise=fastbw(MultiReg.stepwise,rule="p")
stepStepwise


MultiReg.stepwise2=lm(Rating~Sodium+Fiber+Sugars+shelf1+shelf2,data=cereals2)
summary(MultiReg.stepwise2)

shapiro.test(resid(MultiReg.stepwise2))
par(mfrow=c(2,2))
plot(MultiReg.stepwise2)
par(mfrow=c(1,1))

library(car)
vif(MultiReg.stepwise2)


#############################################

churn.data <- read.csv("churn.txt")
names(churn.data)

library("rpart")
cartfit=rpart(Churn.~VMail.Plan+Day.Charge+Eve.Charge+Night.Charge+CustServ.Calls,data=churn.data,method="class")
summary(cartfit)

install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(cartfit,main="Classification Tree")


install.packages("C50")
library(C50)
x=churn.data[,c(5,6,10,13,16,19,20)]
y=as.factor(churn.data[,21])
C50.fit1=C5.0(x,y)
C50.fit1
summary(C50.fit1)

getwd()

## the data and variable are found on page 526 of the textbook##

data = c(2,5,9,15,16,18,25,33,33, 45)
library(cluster)

#single linkage clustering #
agn.single = agnes( agnes, diss = FALSE, stand = FALSE, method = "single")
dend.ang.single = as.dendrogram(agn.single)
plot(dead.agn.single, xlab = "Data Points", main = "Single-linkage clustering")

#complete linkage clustering #
agn.complete = agnes( agnes, diss = FALSE, stand = FALSE, method = "complete")
dend.ang.complete = as.dendrogram(agn.complete)
plot(dead.agn.complete, xlab = "Data Points", main = "Complete-linkage clustering")

## data set and the variables are on page 530 of the textbook ##

## k- means clustering ##

data.matrix = matrix(c(1,3,3,3,4,3,5,3,1,4,2,1,1,2,1), byrow = TRUE, ncol =2 )
plot(data.matrix, ylim = c(0,4), xlim = c(0,6))
number.centers = 2
k.mean.data = kmeans(data.matrix, centers = number.centers)
k.mean.data

plot(data.matrix, col = k.mean.data$cluster, ylim = c(0,4), xlim = c(0,6))
points(k.mean.data$centers, col = 1:2, pch =3 )

k.mean.data$totss
k.mean.data$withinss
k.mean.data$tot.withinss
k.mean.data$between

matrix.dimensions = dim(data.matrix)
number.obs = matrix.dimensions[1]

MSB.data = k.mean.data$between/(number.centers - 1)

MSE.data = k.mean.data$tot.withinss/(number.obs - number.centers)

MSB.data/MSE.data

##################
iris.data = iris
iris

# standardize using min-max #
iris.data$SLN = (iris.data$Sepal.Length - min(iris.data$Sepal.Length))/(max(iris.data$Sepal.Length)-min(iris.data$Sepal.Length))
iris.data

iris.data$SWN = (iris.data$Sepal.Width - min(iris.data$Sepal.Width))/(max(iris.data$Sepal.Width)-min(iris.data$Sepal.Width))
iris.data

iris.data$PWN = (iris.data$Petal.Width - min(iris.data$Petal.Width))/(max(iris.data$Petal.Width)-min(iris.data$Petal.Width))
iris.data

iris.data$PLN = (iris.data$Petal.Length - min(iris.data$Petal.Length))/(max(iris.data$Petal.Length)-min(iris.data$Petal.Length))
iris.data

##library(cluster)

##k-means clustering with k = 3 ##

kmean1 = kmeans(iris.data[,6:9],3)
distance1 = dist(iris.data[,6:9], method = "euclidean")
##kmean1
##distance1

silh1 = silhouette(kmean1$cluster, distance1)
plot(silh1, col = 1:3)
summary(silh1)
## colors and order of clusters might be different in every computer ##
## only the values will stay the same ##


kmean1 = kmeans(iris.data[,6:7], 3)
number.centers = 3
sizeN = dim(iris.data)[1]

MSB.data1 = kmean1$between/(number.centers - 1)
MSB.data1
MSE.data1 = kmean1$withinss/(sizeN - number.centers)
MSE.data1

pfs = MSB.data1/MSE.data1
pfs
1 - pf(pfs, number.centers - 1, sizeN - number.centers)


getwd()
soccer <- read.csv("Uefa Euro Cup All Matches.csv")
soccer
count.fields("Uefa Euro Cup All Matches.csv")









  