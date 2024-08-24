churn.data=read.csv("churn.txt")
names(churn.data)
dim(churn.data)
churn.data[1:5,]
##A description of the churn dataset can be foun on page 54 and 55.
churn.data[1:5,c(5,7,9)]

##table
count.churn=table(churn.data$Churn,churn.data$Int.l.Plan,dnn=c("churn","International Plan"))
count.churn
sum.table=addmargins(count.churn,FUN=sum)
sum.table
length(churn.data$Churn.)
length(churn.data$Int.l.Plan)

row.prop=prop.table(count.churn, margin=1)
row.prop

col.prop=prop.table(count.churn, margin=2)
col.prop

table.churn=table(churn.data$Churn.)
table.churn

barplot(table.churn)
barplot(table.churn,ylim=c(0,3000),main="Churn",ylab="Count")

barplot(count.churn,col=c("red","blue"),ylim=c(0,3000),
        xlab=("International Plan"),main="Churn by International Plan",
        legend=rownames(count.churn),args.legend = list(title="churn"))

mean(churn.data$Day.Mins)
dim(churn.data)
sum(churn.data$Day.Mins/length(churn.data$Day.Mins))
sd.dayMin
sd.dayMin^2
var(churn.data$Day.Mins)
max(churn.data$Day.Mins)
min(churn.data$Day.Mins)
summary(churn.data)

install.packages("psych")
library(psych)

describe(churn.data$Day.Mins)
describe(churn.data)
describe(churn.data,omit=TRUE)

cor(churn.data$Day.Mins,churn.data$Eve.Mins)
plot(churn.data$Day.Mins,churn.data$Eve.Mins)


churn.count=table(churn.data$Churn.)
churn.count

#two-sided test and CI for proportion of churn = TRUE #
prop.test.output = prop.test(churn.count[2],length(churn.data$Churn.),
                             alternative = "two.sided", p=.15, conf.level=.95, correct = FALSE)
prop.test.output
prop.test.output$p.value


# showing calculations using formulas #
num.churn = sum(churn.data$Churn.=="TRUE")
sample.size = dim(churn.data)[1]
est.p = num.churn/sample.size

ts.churn=(est.p-0.15)/sqrt((0.15*(1-0.15))/sample.size)
ts.churn^2


# evaluation of normal distribution for Day Minutes#
qqnorm(churn.data$Day.Mins)
qqline(churn.data$Day.Mins)
shapiro.test(churn.data$Day.Mins)

DayMin.test = t.test(x=churn.data$Day.Mins, mu=180, alternative = "less",
                     conf.level = 0.95)
DayMin.test

DayMin.test$statistic

DayMin.test$conf.int

DayMin.CI = t.test(x=churn.data$Day.Mins, mu=180, alternative = "two.sided",
                     conf.level = 0.95)
DayMin.CI


# Two sample test for proportions #
prop.test(c(184,152), c(531,597), correct = FALSE, alternative = "two.sided", conf.level = 0.99)

# two population means #
churn.true = subset(churn.data, churn.data$Churn.=="True.")
churn.false = subset(churn.data, churn.data$Churn.=="False.")

shapiro.test(churn.false$Night.Mins)
qqnorm(churn.false$Night.Mins)
qqline(churn.false$Night.Mins)

t.test(churn.true$Night.Mins, churn.false$Night.Mins, alternative="two.sided", conf.level=0.95)`1`


#matched pairs t-test

Diff.eve.day = churn.data$Eve.Mins-churn.data$Day.Mins
qqnorm(Diff.eve.day)
qqline(Diff.eve.day)
shapiro.test(Diff.eve.day)

t.test(Diff.eve.day, mu = 0, alternative = "greater")
t.test(churn.data$Eve.Mins, churn.data$Day.Mins, alternative = "greater", paired = "TRUE")
t.test(churn.data$Eve.Mins, churn.data$Day.Mins, alternative = "two.sided", paired = TRUE, conf.level = 0.90)
