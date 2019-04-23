rm(list=ls())
setwd("E:/R/Bike Proect")
getwd()
#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')


install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

Bike_predict=read.csv("day.csv",header = T,na.strings = c(" ", "", "NA"))

View(Bike_predict)
str(Bike_predict)

Bike_predict$instant <- NULL
Bike_predict$dteday <- NULL
#Bike_predict$casual <- NULL
#Bike_predict$registered <- NULL


 Bike_predict$season=as.factor(Bike_predict$season)
Bike_predict$yr=as.factor(Bike_predict$yr)
 Bike_predict$mnth=as.factor(Bike_predict$mnth)
 Bike_predict$holiday=as.factor(Bike_predict$holiday)
 Bike_predict$weekday=as.factor(Bike_predict$weekday)
Bike_predict$workingday=as.factor(Bike_predict$workingday)
Bike_predict$weathersit=as.factor(Bike_predict$weathersit)

##Missing values

sum(is.na(Bike_predict))

summary(Bike_predict)

#Outlier analysis:

str(Bike_predict)
numeric_index = sapply(Bike_predict,is.numeric) #selecting only numeric

numeric_data = Bike_predict[,numeric_index]

cnames = colnames(numeric_data)

cnames

 
 for (i in 1:length(cnames))
 {
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(Bike_predict))+ 
            stat_boxplot(geom = "errorbar", width = 0.5) +
            geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                         outlier.size=1, notch=FALSE) +
            theme(legend.position="bottom")+
            labs(y=cnames[i],x="cnt")+
            ggtitle(paste("Box plot of Count for",cnames[i])))
 }
 
# ## Plotting plots together
 gridExtra::grid.arrange(gn1,gn5,gn2,ncol=3)
 gridExtra::grid.arrange(gn3,gn4,ncol=2)
 gridExtra::grid.arrange(gn1,gn6,ncol=2)
 summary(Bike_predict)
 View(Bike_predict)--0.3441670
 
  for(i in cnames){
   val = Bike_predict[,i][Bike_predict[,i] %in% boxplot.stats(Bike_predict[,i])$out]
 print(length(val))
  Bike_predict[,i][Bike_predict[,i] %in% val] = NA
  }
  
 
 sum(is.na(Bike_predict))
 
  View(Bike_predict)
 
 Bike_predict = knnImputation(Bike_predict, k = 7)
 
 
 #Correlation Anayysis
 corrgram(Bike_predict[,numeric_index], order = F,
          upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
 str(Bike_predict)
 #Anova testt
 
 print(summary(aov(cnt~season,data = Bike_predict)))
 print(summary(aov(cnt~yr,data = Bike_predict)))
 
 print(summary(aov(cnt~mnth,data = Bike_predict)))
 
 print(summary(aov(cnt~holiday,data = Bike_predict)))
 
 print(summary(aov(cnt~weekday,data = Bike_predict)))
 
 print(summary(aov(cnt~workingday,data = Bike_predict)))
 
 print(summary(aov(cnt~weathersit,data = Bike_predict)))
 
 
 ##Bike_predict=subset(Bike_predict,select=-c(holiday,weekday,workingday,atemp))
 
 Bike_predict=subset(Bike_predict,select=-c(weekday,atemp))
 
View(Bike_predict)
hist(Bike_predict$casual) 
hist(Bike_predict$registered)

library(usdm)
vif(numeric_data)
vifcor(numeric_data,th=0.9)

#Standardisation
 
  # Bike_predict$cnt = (Bike_predict$cnt - mean(Bike_predict$cnt))/
                                  (sd(Bike_predict$cnt))

   ##((Bike_predict$cnt)<=0)) <- 1

fea_names=c("casual","registered")
for (i in fea_names) {
  Bike_predict[,i]=(Bike_predict[,i]-min(Bike_predict[,i]))/(max(Bike_predict[,i])-min(Bike_predict[,i]))
  
}


#

#Bike_predict$registered=((Bike_predict$registered)-min(Bike_predict$registered))/(max(Bike_predict$registered)-min(Bike_predict$registered))

##Sampling
set.seed(123)
X_index=sample(1:nrow(Bike_predict),0.8*nrow(Bike_predict))
X_train=Bike_predict[X_index,-12]
View(Y_train)
X_test=Bike_predict[-X_index,-12]
Y_train=Bike_predict[X_index,12]
Y_test=Bike_predict[-X_index,12]

##Random Forest
RF_model=randomForest(x=X_train,y=Y_train,ntree = 100)


#Predict for new test cases
RF_predict=predict(RF_model,X_test)
getTree(RF_model,1,labelVar = TRUE)
summary(RF_model)
print(RF_model)

MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}

MAPE(RF_predict,Y_test)




###Linear regression 


train = Bike_predict[X_index,]

test = Bike_predict[-X_index,]

lm_model = lm(cnt ~., data = train)

summary(lm_model)

lm_predict=predict(lm_model,newdata = X_test)
lm_predict
MAPE(lm_predict,Y_test)



###Decision Tree

DT_model=rpart(cnt ~.,data=train,method = "anova")
#Predict for new test cases
DT_predict=predict(DT_model,X_test)
summary(DT_model)
print(DT_model)
MAPE(DT_predict,Y_test)




