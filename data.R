
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
###                                     MODEL 1 --- ENSEMBLE(LOGISTIC, KNN, SVM, C5.0, Rpart)                     ###

data<-read.csv("data.csv",header = TRUE)
data$PatientID<-as.factor(as.character(data$PatientID))
data$ActGPI<-as.factor(as.character(data$ActGPI))
data$Date <- as.Date(data$Date, "%d-%m-%Y")
sum(is.na(data))
length(unique(data$PatientID))
length(unique(data$ActGPI))
length(unique(data$Medication))
delay=vector(length=nrow(data))
t=vector(length = nrow(data))
delay[1]=0

j=1;
attach(data)
for(i in 1:(nrow(data)-1))
{
  if(Medication[i+1]==Medication[i])
    {
      delay[i+1]=Date[i+1]-Date[i]-For_How_Many_Days[i]
      t[i]=j;
      j=j+1;
      #delay[i+1]=delay[i+1]-30
    }
  else
    {
      t[i]=j;
      j=1;
      delay[i+1]=0
    }
}

#delay<-as.data.frame(delay)
#data<-data.frame(data,delay)
data<-cbind(data,delay,t)
target=vector(length = nrow(data))
attach(data)
for(i in 1:nrow(data))
{
  if(data$delay[i]<=2)
  {
    target[i]=1
  }
  else
  {
      target[i]=0
  }
}
  

#target<-as.data.frame(target)
#data<-data.frame(data,target)
data<-cbind(data,target)
sum(delay)
data$target=as.factor(as.character(data$target))


data_x=data



library(tidyr)
#df = data.frame(year=2009:2013, customerID=20227:20231) # using akrun's data

data<-unite(data, patient_medication, c(PatientID,Medication), remove=FALSE)

data<-data[,-c(2,3)]

data<-subset(data,select=-c(For_How_Many_Days,Date,delay))
data$patient_medication=as.factor(as.character(data$patient_medication))

train=data[0,]
test=data[0,]
attach(data)
for(i in 1:(nrow(data)-1))
{
  if((patient_medication[i+1]==patient_medication[i])   ||   ((patient_medication[i-1]!=patient_medication[i])&&(patient_medication[i]!=patient_medication[i+1])))
  {
    train=rbind(train,data[i,])
  }
  else
  {
    test=rbind(test,data[i,])
  }
}
##################################################
                  # TEST
train=train[,-1]
test=test[,-1]
###################################################

start.time<-Sys.time()
LogReg <- glm(target ~ ., data=train, family=binomial)
end.time<-Sys.time()
time.taken<-end.time-start.time
time.taken
#LogReg<-glm(formula = target ~ ActGPI + QTY + Age + Sex + PurchasedBy + Pharmacy, family = binomial, data = train)
#summary(LogReg)

# library(car)
# vif(LogReg)
# library(MASS)
# stepAIC(LogReg)

#str(data)


prob<-predict(LogReg, type="response")
glm_pred <- ifelse(prob > 0.5, 1, 0)


table(train$target,glm_pred)

fitted.results <- predict(LogReg,test,type='response')

fitted_class <- ifelse(fitted.results > 0.5,1,0)

table(test$target,fitted_class)

conf.mat1 = table(train$target,glm_pred)
accuracy1 = sum(diag(conf.mat1))/sum(conf.mat1)
accuracy1
precision1 = conf.mat1[2,2]/sum(conf.mat1[,2])
precision1
recall1 = conf.mat1[2,2]/sum(conf.mat1[2,])
recall1

#residualPlot(LogReg_updated)

conf.mat2 = table(test$target,fitted_class)
accuracy2 = sum(diag(conf.mat2))/sum(conf.mat2)
accuracy2
precision2 = conf.mat1[2,2]/sum(conf.mat2[,2])
precision2
recall2 = conf.mat2[2,2]/sum(conf.mat2[2,])
recall2


library(ROCR)
library(ggplot2)
# Predicting on the train data
predicted <- predict(LogReg,type="response")
prob <- prediction(predicted, train$target)
# Getting the true positive rate and false negative rate
tprfpr <- performance(prob, "tpr", "fpr")
# Plotting the true positive rate and false negative rate based on the threshold value
plot(tprfpr)

cutoffs <- data.frame(cut=tprfpr@alpha.values[[1]], fpr=tprfpr@x.values[[1]],tpr=tprfpr@y.values[[1]])
# Sorting the data frame in the decreasing order based on tpr
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]

head(subset(cutoffs, fpr < 0.2))
# Plotting the true positive rate and false negative rate based based on the cutoff # increasing from 0.1-1
plot(tprfpr, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
tpr <- unlist(slot(tprfpr, "y.values"))
fpr <- unlist(slot(tprfpr, "x.values"))
# creating the data frame with tpr and fpr
roc <- data.frame(tpr, fpr)
# Plotting the graph
ggplot(roc) + geom_line(aes(x = fpr, y = tpr)) +
  geom_abline(intercept=0,slope=1,colour="gray") +
  ylab("Sensitivity") + xlab("1 - Specificity")




#shapiro.test(test$t)

#####################################################       KNN    ###############################################

# data_knn<-data
# 
# 
# cat<-subset(data_knn,select=c(ActGPI,RouteOfAdmin,State,MailRetail,Sex,PurchasedBy,Pharmacy))
# library(dummies)
# dummy <- dummy.data.frame(cat,sep = ".")
# str(dummy)
# data_knn<-subset(data_knn,select=-c(ActGPI,RouteOfAdmin,State,MailRetail,Sex,PurchasedBy,Pharmacy))
# 
# data_knn=data.frame(dummy,data_knn,data$patient_medication)
# colnames(data_knn)[1144]<-"patient_medication"
# 
# # set.seed(123) 
# # # to take a random sample of  60% of the records for train data 
# # knn = sample(1:nrow(data_knn),nrow(data_knn)*0.7) 
# # data_train = data_knn[knn,] 
# # data_test =data_knn[-knn,]
# train_knn=data_knn[0,]
# test_knn=data_knn[0,]
# attach(data_knn)
# for(i in 1:(nrow(data_knn)-1))
# {
#   if((patient_medication[i+1]==patient_medication[i]) || ((patient_medication[i-1]!=patient_medication[i])&&(patient_medication[i]!=patient_medication[i+1])))
#   {
#     train_knn=rbind(train_knn,data[i,])
#   }
#   else
#   {
#     test_knn=rbind(test_knn,data[i,])
#   }
# }
# 
# trainwithoutclass = subset(train,select=-c(target))
# testwithoutclass = subset(test,select=-c(target))
# 
# library(vegan)
# library(dummies)
# library(class)
# # N = 1/3/5/7
# noOfNeigh <- 1
# 
# 
# start.time<-Sys.time()
# knn_pred = knn(trainwithoutclass,testwithoutclass, train$target , k = noOfNeigh)
# end.time<-Sys.time()
# time.taken<-end.time-start.time
# time.taken
# 
# 
# a = table(knn_pred,test$target)
# a
# accu = sum(diag(a))/nrow(testwithoutclass)
# accu



########################################################################
# With standardization
########################################################


data_knn<-data


cat<-subset(data_knn,select=c(ActGPI,RouteOfAdmin,State,MailRetail,Sex,PurchasedBy,Pharmacy))
num<-subset(data_knn,select=-c(patient_medication,ActGPI,RouteOfAdmin,State,MailRetail,Sex,PurchasedBy,Pharmacy,target))

library(vegan)
num=decostand(num,"range") # to standardize the data using 'Range' method


library(dummies)
dummy <- dummy.data.frame(cat,sep = ".")
#str(dummy)

data_knn=data.frame(data_knn$patient_medication,dummy,num,data_knn$target)
colnames(data_knn)[1]<-"patient_medication"
colnames(data_knn)[194]<-"target"

# set.seed(123) 
# # to take a random sample of  60% of the records for train data 
# knn = sample(1:nrow(data_knn),nrow(data_knn)*0.7) 
# data_train = data_knn[knn,] 
# data_test =data_knn[-knn,]


train_knn=data_knn[0,]
test_knn=data_knn[0,]
attach(data_knn)
for(i in 1:(nrow(data_knn)-1))
{
  if((patient_medication[i+1]==patient_medication[i])   ||   ((patient_medication[i-1]!=patient_medication[i])&&(patient_medication[i]!=patient_medication[i+1])))
  {
    train_knn=rbind(train_knn,data_knn[i,])
  }
  else
  {
    test_knn=rbind(test_knn,data_knn[i,])
  }
}
train_knn=subset(train_knn,select=-c(patient_medication))
test_knn=subset(test_knn,select=-c(patient_medication))


trainwithoutclass = subset(train_knn,select=-c(target))
testwithoutclass = subset(test_knn,select=-c(target))

library(vegan)
library(dummies)
library(class)
# N = 1/3/5/7
noOfNeigh <- 1


start.time<-Sys.time()
pred = knn(trainwithoutclass,testwithoutclass, train_knn$target , k = 5)
end.time<-Sys.time()
time.taken<-end.time-start.time
time.taken


a = table(pred,test_knn$target)
a
accu = sum(diag(a))/nrow(testwithoutclass)
accu


################################################################  SVM   #####################################################
#############################################################################################################################




####Classification using "e1071"####
# install.packages("e1071")
library(e1071)

# Store the independent variables and target variable separately
# (for easy use)
x = trainwithoutclass
y = train_knn$target

# Building the model on train data
model  =  svm(x = x, y = y, type = "C-classification", kernel = "linear", cost = 10)
summary(model)
#The "cost" parameter balances the trade-off between having a large margin and classifying
#all points correctly. It is important to choose it well to have good
#generalization.

# Predict on train data using the model
svm_pred=  predict(model, x) # x is all the input variables
table(svm_pred)

# Build confusion matrix ("loan-takers":1; "non loan-takers":0)
# compare actual (i.e. "y") vs. predicted (pred_train)
tb_train = table(y,svm_pred)#actual is on left and predicted shown on top

# Calculate error metrics
accuracy_train = sum(diag(tb_train))/sum(tb_train)
accuracy_train
recall_train = (tb_train[2,2]/(tb_train[2,2]+tb_train[2,1]))
recall_train

# Predict on test data using the model
a  = testwithoutclass
b  = test_knn$target
pred_test = predict(model, a)
table(pred_test)

# Build confusion matrix ("loan-takers":1; "non loan-takers":0)
#compare actual (i.e. "b") vs. predicted (pred_test)
tb_test <- table(b,pred_test)
accuracy_test = sum(diag(tb_test))/sum(tb_test)
accuracy_test
precision_test = tb_test[2,2]/sum(tb_test[,2])
precision_test
recall_test = (tb_test[2,2]/(tb_test[2,2]+tb_test[2,1]))
recall_test



###############################################################################################################################
###############################################           C5.0                #################################################

library(C50)
Model_C50 <- C5.0(x,y,rules = TRUE)
Model_C50
summary(Model_C50)
table(y)

#Predicting on Train (already shown in summary)
P1_train=predict(Model_C50,x)
#P1_train
dt_train=table(y,Predicted=P1_train)
dt_accuracy_train = sum(diag(dt_train))/sum(dt_train)
dt_accuracy_train
dt_precision_train = dt_train[2,2]/sum(dt_train[,2])
dt_precision_train
dt_recall_train = (dt_train[2,2]/(dt_train[2,2]+dt_train[2,1]))
dt_recall_train

#Predicting on Test
P1_test = predict(Model_C50,a)
#P1_test
dt_test=table(b,Predicted=P1_test)

dt_accuracy_test = sum(diag(dt_test))/sum(dt_test)
dt_accuracy_test
dt_precision_test = dt_test[2,2]/sum(dt_test[,2])
dt_precision_test
dt_recall_test = (dt_test[2,2]/(dt_test[2,2]+dt_test[2,1]))
dt_recall_test


######################################################################## R PART ##############################
#rpart
library(rpart)
library(rpart.plot)

Model_rpart= rpart(target~.,data=train, method="class")
Model_rpart
plot(Model_rpart)
rpart.plot(Model_rpart,type=3,extra=103,fallen.leaves = FALSE)



#Predicting on Train
P1_train_rpart=predict(Model_rpart,train,type="class")
table(train$target,predicted=P1_train_rpart)

#Predicting on Test
P1_test_rpart=predict(Model_rpart,test,type="class")
table(test$target,predicted=P1_test_rpart)



#########END########


# (4) Combining training predictions of CART, C5.0 & Log Regression together
train_pred_all_models <- cbind(fitted_class,pred,pred_test,P1_test,P1_test_rpart)
train_pred_all_models <- data.frame(apply(train_pred_all_models, 2, function(x) {as.factor(x)}))
# or first use "apply" then type data_ensemble <- data.frame(data_ensemble)
str(train_pred_all_models)
summary(train_pred_all_models)
rm(fitted_class,pred,pred_test)


# (5) Viewing the predictions of each model
table(train_pred_all_models$fitted_class) #Logistic 
table(train_pred_all_models$pred) #KNN
table(train_pred_all_models$pred_test) #SVM
table(b) #Original Dataset DV
table(test$target)



# (6) Adding the original DV to the dataframe
train_pred_all_models <- cbind(train_pred_all_models, test$target)
colnames(train_pred_all_models)[6] = "target"


# (7) Ensemble Model with GLM as Meta Learner
str(train_pred_all_models)
head(train_pred_all_models)

glm_ensemble <- glm(target ~ ., train_pred_all_models, family = binomial())
summary(glm_ensemble)

# (8) Check the "glm_ensemble model" on the train data

pred_ensemble <- predict(object = glm_ensemble, train_pred_all_models, type = "response")
pred_ensemble <- ifelse(test = pred_ensemble > 0.5, 1, 0)
table(pred_ensemble)

check4 <- table(train_pred_all_models$target,pred_ensemble)
accuracy_train = sum(diag(check4))/sum(check4)
accuracy_train
recall_en_train = sum(check4[2,2])/sum(check4[2,])
recall_en_train





############################################## ENSEMBLE MAJORITY VOTING  ##############################################
#######################################################################################################################

#The majority vote
#testSet$pred_majority<-as.factor(ifelse(testSet$pred_rf=='Y' & testSet$pred_knn=='Y','Y',ifelse(testSet$pred_rf=='Y' & testSet$pred_lr=='Y','Y',ifelse(testSet$pred_knn=='Y' & testSet$pred_lr=='Y','Y','N'))))
pred=as.data.frame(pred)
pred_test=as.data.frame(pred_test)
maj=cbind(fitted_class,pred,pred_test,P1_test,P1_test_rpart)
maj=data.frame(apply(maj, 2, function(x) {as.factor(x)}))
major_vote=vector(length=nrow(test))
  for(i in 1:nrow(maj))
  {
    major_vote[i]=as.character(getmode(maj[i,]))
  }

table(major_vote)
major_vote=replace(major_vote,major_vote==1,0)
major_vote=replace(major_vote,major_vote==2,1)


major_tab <- table(b,major_vote)
major_tab
accuracy_train = sum(diag(major_tab))/sum(major_tab)
accuracy_train
precision_train = major_tab[2,2]/sum(major_tab[,2])
precision_train
recall_en_train = major_tab[2,2]/sum(major_tab[2,])
recall_en_train

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
                                    ###                MODEL 2 --- MODE           ###
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

model2<-data
#getmode(model2$target)
j=1;
model2_train=model2[0,]
model2_test=model2[0,]
mode=0
mode=vector(length=nrow(test))
#model2_test=cbind(model2_test,m)
attach(model2)
for(i in 1:(nrow(model2)-1))
{
  if(patient_medication[i+1]==patient_medication[i])  
  {
    model2_train=rbind(model2_train,model2[i,])
  }
  else if((patient_medication[i-1]!=patient_medication[i])&&(patient_medication[i]!=patient_medication[i+1]))
  {
    
  }
  else
  {
    
    
    if(j==1)
      {
        mode[j]=getmode(model2_train$target)
        model2_test=model2[i,]
        model2_train=model2[0,]
        j=j+1
      }
    else
      {
        mode[j]=getmode(model2_train$target)
        model2_test=rbind(model2_test,model2[i,])
        model2_train=model2[0,]
        j=j+1
      }
  }
}


mode=replace(mode,mode==1,0)
mode=replace(mode,mode==2,1)

conf.mat_mode = table(model2_test$target,mode)
accuracy_mode = sum(diag(conf.mat_mode))/sum(conf.mat_mode)
accuracy_mode
precision_mode = conf.mat_mode[2,2]/sum(conf.mat_mode[,2])
precision_mode
recall_mode = conf.mat1[2,2]/sum(conf.mat1[2,])
recall_mode

write.csv(test, file = "test.csv",row.names=FALSE, na="")
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
###                                              MODEL 3 --- TIME SERIES                                          ###



model3<-read.csv("data.csv",header = TRUE)
model3$PatientID<-as.factor(as.character(model3$PatientID))
model3$ActGPI<-as.factor(as.character(data$ActGPI))
model3$Date <- as.Date(model3$Date, "%d-%m-%Y")

model3<-cbind(model3,delay)
#model3<-model3[1:20,]
#model3<-model3[,-c(1,2)]

library(forecast)
TimeSeries = ts(model3$delay)

library(DMwR)
# Building auto arima model
autoArimaModel <- auto.arima(TimeSeries)

autoarima_forecast <- data.frame(forecast(autoArimaModel,h=1))$Point.Forecast
regr.eval(trues = actuals, preds = autoarima_forecast)






for(i in 1:(nrow(model2)-1))
{
  if(patient_medication[i+1]==patient_medication[i])  
  {
    model2_train=rbind(model2_train,model2[i,])
  }
  else if((patient_medication[i-1]!=patient_medication[i])&&(patient_medication[i]!=patient_medication[i+1]))
  {
    
  }
  else
  {
    
    
    if(j==1)
    {
      mode[j]=getmode(model2_train$target)
      model2_test=model2[i,]
      model2_train=model2[0,]
      j=j+1
    }
    else
    {
      mode[j]=getmode(model2_train$target)
      model2_test=rbind(model2_test,model2[i,])
      model2_train=model2[0,]
      j=j+1
    }
  }
}