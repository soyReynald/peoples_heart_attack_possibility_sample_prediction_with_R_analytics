rm(list = ls()) #removes all variables stored previously
library(Hmisc) #import

#Problem Statement: 
#To predict if a person has a heart disease or not based 
#on attributes blood pressure,heart beat,exang,fbs and others.


#import the data
heart <- read.csv("C:/Users/PC/Documents/Tareas de la universidad/Taller de bases de datos/heart.csv")
View(heart)


#number of rows of the data set
nrow(heart)


#Heart Disease Exploraion
library(ggplot2)
gg <- ggplot(heart, aes(x=chol, y=trestbps)) + geom_point(aes(col=target))
plot(gg)


#summary
summary(heart)


#Line graph
x <- heart[1:20,"chol"]
plot(x, type = "o", col = "red", xlab = "Gender", ylab = "chol", main = "Heart details of Male and Female - Cholesterol level")
#x - vector containing numeric value
#xlab - label of x axis
#ylab - label of y axis
#main - title of the plot
#col - indicates the color
#type o - that draws both parts and lines, 
#type i - that draws only lines
#type p - that draws only the points


#Pie Chart
x <- heart[1:13, "oldpeak"]
Lables <- heart[1:13, "oldpeak"]
pie(x, Lables, main = "Oldpeak in Heart Disease", col = rainbow(length(x)))
#Lables gives the discription of the slices

#Bar plot
library(psych)
B <- heart[1:20, "ï..age"]
N <- heart[1:20, "cp"]
barplot(B, names.arg = N, xlab = "Age", ylab="Cp", main = "Details of Heart Disease Patient's Age and Cp", col = "Blue")
#names.arg is a vector of name appearing under each bar


#Boxplot 1
heart1=data.frame(heart$ï..age, heart$trestbps,heart$chol,heart$thalach)
boxplot(heart1,col=c("green","red","blue","yellow"))

#Box plot 2
input_data <- heart[, c("ï..age","cp")]
print(input_data)
#prints the column of ï..age and cp
boxplot(ï..age~cp, data = heart, xlab = "No. of Age", ylab = "Gender", main = "Details of Heart Attack", col=c("green","red"))


#Scatterplot 1
library(ggplot2)
gg <- ggplot(heart, aes(x=chol, y=trestbps)) + geom_point(aes(col=target, size=oldpeak)) + 
  geom_smooth(method="loess", se=F) + xlim(c(100, 430)) + ylim(c(75, 200)) + 
  labs(subtitle="trestbps Vs chol", y="trestbps", x="chol", title="Scatterplot", 
       caption = "Source: midwest", bins = 30)
plot(gg)

#Scatter plot 2
ss <- heart[1:303, c("trestbps", "thalach")]
print(ss) #prints the column of trestbps and thalach
plot(x=heart$trestbps, heart$thalach, xlab = "trestbps", ylab = "thalach", main = "Trestbps vs Thalach", col = "maroon")


#Logistic regression to predict which patients have heart disease.
#baseline model
table(heart$target)
#165/303
#The baseline model value of 0.545, 
#means that approximately 54% of patients suffering from heart disease.


#Splitting Data set into Train and Test set
library(caTools)
#randomly split data
set.seed(123)
split=sample.split(heart$target, SplitRatio = 0.75)
split

qualityTrain=subset(heart,split == TRUE)
qualityTest=subset(heart,split == FALSE)
nrow(qualityTrain)
nrow(qualityTest)


#Building the Model
#logistic regression model
heartlog=glm(target ~ target+ï..age+trestbps+chol+fbs+restecg+thalach+exang+oldpeak+slope+ca+thal,data=qualityTrain,family = binomial)
summary(heartlog)
#glm is the generalized linear model. 
#target~ used to model target that we want.
#family = binomial() is used to predict a binary outcome, 0 or 1.


#Removing Variables based on Significance Level using the backward method
heartlog2=glm(target ~ ï..age+sex+cp+trestbps+chol+fbs+thalach+exang+oldpeak+slope+ca+thal,data=qualityTrain,family = binomial)
summary(heartlog2)


#after removing as many as 4 variables - remove restecg,remove fbs,remove slove,remove exang
#then get model 6
heartlog6=glm(target ~ sex+cp+trestbps+chol+thalach+oldpeak+ca+thal,data=qualityTrain,family = binomial)
summary(heartlog6)


#Making predictions on training sets using heartlog6
predictTrain=predict(heartlog,type="response")
predictTrain


#Plotting ROCR curve
library(ROCR)
ROCRpred=prediction(predictTrain, qualityTrain$target)
ROCRperf=performance(ROCRpred,"tpr","fpr")
plot(ROCRperf)
plot(ROCRperf,colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1),
     text.adj=c(-0.2,1.7))
#From ROCR curve threshold of 0.7 seems to be okay so that true 
#positives are maximized such that maximum number of patients 
#with heart disease are not identified as healthy.


#Area under the curve
auc = as.numeric(performance(ROCRpred, 'auc')@y.values)
auc
#Higher the AUC, better the model is at 
#distinguishing between patients with disease and no disease.
#AUC value is 0.92 means the model is able to distinguish 
#between patients with the disease and no disease with a 
#probability of 0.92. So it is a good value.


#Accuracy
#Accuracy using a threshold of 0.7
predictTest=predict(heartlog, newdata = qualityTest,type = "response")
table(qualityTest$target,predictTest >=0.7)

#accuracy
(29+28)/75
#Logistic regression model with all the variables and
#logistic regression model after removing less significant 
#attributes performed best with an accuracy of testing 76%.