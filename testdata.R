getwd()
x=c(5,6,7,8)
y=c(5,21,18,13)
z=cbind(x,y)
dataex=data.frame(z)
dataex$x
write.table(dataex,file="testdata.txt")
read.table("testdata")
dataex
dataex[2,2]

