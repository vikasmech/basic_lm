rm(list = ls(all=T))
# set the Folder
########################################################################
path <- "D:\\R_projects\\basic_lm"
setwd(path)


# read in the data
########################################################################
data <- read.csv(file = "data.csv", header = T)

# summarize the data
########################################################################
str(data)
summary(data)

cor(data$BigMacPrice,data$NetHourlyWage)

##Randomly split  the data into two
########################################################################
rows<-seq(1,nrow(data),1)
set.seed(1234)

trainrows<-sample(rows,0.7*nrow(data))
Train<-data[trainrows,]
Test<-data[-trainrows,]


## Training the Data
########################################################################
LinReg <- lm(NetHourlyWage ~ BigMacPrice,data=Train )
summary(LinReg)
coefficients(LinReg)

## Predict the test
########################################################################
as.numeric(coefficients(LinReg)[1]+coefficients(LinReg)[2]*3.33)

#With the help of ???predict??? function 
testdata = data.frame(BigMacPrice=3.33)
predict(LinReg, testdata)

#7. Find the confidence limits value for the predicted value at 0.95 significance level 
predict(LinReg, testdata, interval="confidence",level=0.95)

#8. Summarize inferential statistics of the linear regression model 
summary(LinReg)

#9. Get the predictions and confidence limits on the data set 
Pred<-data.frame(predict(LinReg, data, interval="confidence" ,level=0.95)) 
names(Pred)

#10. Plotting data, fitted line and confidence limits 
plot(data$BigMacPrice,data$NetHourlyWage) 

points(data$BigMacPrice,Pred$fit,type="l", col="red", lwd=2)
points(data$BigMacPrice,Pred$lwr,pch="-", col="red", lwd=4)
points(data$BigMacPrice,Pred$upr,pch="-", col="red", lwd=4)

#11.Residual plot of the model 
par(mfrow=c(2,2)) 
plot(LinReg)


library(DMwR)
#Error verification on train data 

regr.eval(Train$NetHourlyWage, LinReg$fitted.values)
#Error verification on test data 
Pred<-predict(LinReg,Test) 
regr.eval(Test$NetHourlyWage, Pred)

#13. # leverage 
lev = hat(model.matrix(LinReg)) 
plot(lev)
data[lev>0.2,]

# cook's distance 
cook = cooks.distance(LinReg) 
plot(cook,ylab="Cooks distances")
