start.time1<-Sys.time()
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
###                                     MODEL 1 --- ENSEMBLE(LOGISTIC, KNN, SVM, C5.0, Rpart)                     ###

data<-read.csv("data.csv",header = TRUE)
data$PatientID<-as.factor(as.character(data$PatientID))
data$Medication<-as.factor(as.character(data$Medication))
data$ActGPI<-as.factor(as.character(data$ActGPI))
data$RouteOfAdmin<-as.factor(as.character(data$RouteOfAdmin))
data$State<-as.factor(as.character(data$State))
data$MailRetail<-as.factor(as.character(data$MailRetail))
data$Sex<-as.factor(as.character(data$Sex))
data$PurchasedBy<-as.factor(as.character(data$PurchasedBy))
data$Pharmacy<-as.factor(as.character(data$Pharmacy))

data$Date <- as.Date(data$Date, "%d-%m-%Y")
sum(is.na(data))

#########################3



######################333

length(unique(data$PatientID))
length(unique(data$Medication))
length(unique(data$ActGPI))
length(unique(data$RouteOfAdmin))

length(unique(data$State))
length(unique(data$MailRetail))
length(unique(data$Sex))
length(unique(data$PurchasedBy))
length(unique(data$Pharmacy))

levels(data$PurchasedBy)[levels(data$PurchasedBy)=="SPOUSE"] <- "Spouse"

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
#test=test[-(test$RouteOfAdmin=="N"),]
#delay<-as.data.frame(delay)
#data<-data.frame(data,delay)
data<-cbind(data,delay,t)
# data2=data[0,]
# 
# for(i in 1:nrow(data))
# {
#   if((data$delay[i]<100)&&(data$delay[i]>-100))
#     data2=rbind(data2,data[i,])
# }
# data=data2

target=vector(length = nrow(data))
#attach(data)
for(i in 1:nrow(data))
{
  if(data$delay[i]<=1)
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
write.csv(data_x, file = "data_x.csv",row.names=FALSE, na="")



library(tidyr)

data<-unite(data, patient_medication, c(PatientID,Medication), remove=FALSE)

data<-data[,-c(2,3)]

data<-subset(data,select=-c(For_How_Many_Days,Date))
data$patient_medication=as.factor(as.character(data$patient_medication))

########################################################################################################33

data2=data[0,]
i=1
while(i<=nrow(data)-2)
{
  if((data$patient_medication[i]==data$patient_medication[i+1])&&(data$patient_medication[i+1]==data$patient_medication[i+2]))
  {
    while((data$patient_medication[i]==data$patient_medication[i+1]))
    {
      data2=rbind(data2,data[i,])
      i=i+1
      if(i==nrow(data))
      {
        data2=rbind(data2,data[i,])
      }
    }
    data2=rbind(data2,data[i,])
    i=i+1
  }
  else
    {
    i=i+1
    }
  
}
data=data2
data=data[,-12]



#########################################################################################################3


train=data[0,]
test=data[0,]
attach(data)
for(i in 1:(nrow(data)))
{
  if((patient_medication[i]==patient_medication[i+1]) && (i!=nrow(data) ))
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
LogReg <- glm(formula = target ~ ActGPI + QTY + Age + PurchasedBy + Pharmacy, family = binomial, data = train)
end.time<-Sys.time()
time.taken<-end.time-start.time
time.taken

#LogReg<-glm(formula = target ~ ActGPI + QTY + Age + Sex + PurchasedBy + Pharmacy, family = binomial, data = train)
#summary(LogReg)

# library(car)
# vif(LogReg)
library(MASS)
 stepAIC(LogReg)

#str(data)


prob<-predict(LogReg, type="response")
glm_pred <- ifelse(prob > 0.63, 1, 0)


table(train$target,glm_pred)

fitted.results <- predict(LogReg,test,type='response')

fitted_class <- ifelse(fitted.results > 0.63,1,0)

table(test$target,fitted_class)

conf.mat1 = table(train$target,glm_pred)
accuracy1 = sum(diag(conf.mat1))/sum(conf.mat1)
accuracy1
precision1 = conf.mat1[2,2]/sum(conf.mat1[,2])
precision1
recall1 = conf.mat1[2,2]/sum(conf.mat1[2,])
recall1
specificity=conf.mat1[1,1]/sum(conf.mat1[1,])
specificity
#residualPlot(LogReg_updated)

conf.mat2 = table(test$target,fitted_class)
accuracy2 = sum(diag(conf.mat2))/sum(conf.mat2)
accuracy2
precision2 = conf.mat2[2,2]/sum(conf.mat2[,2])
precision2
recall2 = conf.mat2[2,2]/sum(conf.mat2[2,])
recall2
specificity2=conf.mat2[1,1]/sum(conf.mat2[1,])
specificity2

write.csv(fitted_class, file = "test_log.csv",row.names=FALSE, na="")

library(ROCR)
library(ggplot2)
# Predicting on the train data
predicted <- predict(LogReg,type="response")
prob <- prediction(predicted, train$target)
# Getting the true positive rate and false negative rate
tprfpr <- performance(prob, "tpr", "fpr")
# Plotting the true positive rate and false negative rate based on the threshold value
plot(tprfpr)
str(tprfpr)

cutoffs <- data.frame(cut=tprfpr@alpha.values[[1]], fpr=tprfpr@x.values[[1]],tpr=tprfpr@y.values[[1]])
head(cutoffs)
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
ggplot(roc) + geom_line(aes(x = fpr, y = tpr)) + geom_abline(intercept=0,slope=1,colour="gray") +ylab("Sensitivity") + xlab("1 - Specificity")


auc.tmp <- performance(prob,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc





#shapiro.test(test$t)

################################################################  Random Forest   ###########################################
#############################################################################################################################


library(randomForest)
rf <- randomForest(target~ RouteOfAdmin+State+QTY+Age+MailRetail+AmountPaid+Sex+PurchasedBy+Pharmacy+t, data=train,ntree=200,do.trace=20) 

# View results and understand important attributes
print(rf)
rf$predicted 
rf$importance #gives 1st col(accuracy reduces if imp var are removed)
importance(rf)
round(importance(rf), 2)   

# Extract and store important variables obtained from the random forest model
# Imp_rf <- data.frame(rf$importance)
# Imp_rf1 <- data.frame(row.names(Imp_rf),Imp_rf[,1])
# colnames(Imp_rf1) = c('Attributes','Importance')
# Imp_rf2 <- Imp_rf1[order(Imp_rf1$Importance , decreasing = TRUE),]
# Imp_rf2 <- Imp_rf2[1:6,]
# 
# # plot (directly prints the important attributes) 
# varImpPlot(rf)
# #importance(hepatitis_rf)
# 
# # Predict on Train data 
# pred_model_train <-predict(rf,train[,-c(12)],type="response")
# 
# result_train <- table("actual _values"= train$target, pred_model_train)
# result_train
# #OR
# #table(trainR$target, predict(hepatitis_rf, trainR, type="response", norm.votes=TRUE)) 
# 
# # Predicton Test Data
# pred_model_test <-predict(rf,test[,-c(12)], type="response", norm.votes=TRUE)
# result_test <- table("actual _values"= test$target, pred_model_test)
# result_test

# # Accuracy,Precision and Recall on testR
# test_accuracy <- sum(diag(result_test))/sum(result_test)*100
# test_accuracy
# test_recall <- ((result_test[2,2])/(result_test[2,2]+result_test[2,1])*100)
# test_recall
# test_precision <-((result_test[2,2])/(result_test[2,2]+result_test[1,2])*100)
# test_precision
# test_specificity <-((result_test[1,1])/(result_test[1,1]+result_test[1,2])*100)
# test_specificity
############################# 

# Optimizing "mtry" to improve model/metrics

# ## find optimal value of mtry for randomForest
# bestmtry <- tuneRF(train[,c(-1,-12)],train$target, ntreeTry=100,
#                    stepFactor=1.5,improve=0.01, trace=F, plot=F, dobest=FALSE)
# 
# best.m <- bestmtry[bestmtry[, 2] == min(bestmtry[, 2]), 1]
# print(bestmtry)
# print(best.m)

# Use the optimal number of variables selected at each split and 
#run random forest again
set.seed(71)
nw_rf <-randomForest(target~ RouteOfAdmin+State+QTY+Age+MailRetail+AmountPaid+Sex+PurchasedBy+Pharmacy+t,data=train, mtry=2, importance=TRUE,ntree=400)
print(nw_rf)





# Predicton Train Data
pred_model_train1 <-predict(nw_rf,train[,c(-1,-12)],type="response", norm.votes=TRUE)
result_train1 <- table("actual _values"= train$target, pred_model_train1)
result_train1

# Accuracy,Precision and Recall on testR
test_accuracy1 <- sum(diag(result_train1))/sum(result_train1)*100
test_accuracy1
test_recall1 <- ((result_train1[2,2])/(result_train1[2,2]+result_train1[2,1])*100)
test_recall1
test_precision1 <-((result_train1[2,2])/(result_train1[2,2]+result_train1[1,2])*100)
test_precision1
test_spec1 <-((result_train1[1,1])/(result_train1[1,1]+result_train1[1,2])*100)
test_spec1

# Predicton Test Data
pred_model_test1 <-predict(nw_rf,test[,c(-1,-12)],type="response", norm.votes=TRUE)
result_test1 <- table("actual _values"= test$target, pred_model_test1)
result_test1

# Accuracy,Precision and Recall on testR
test_accuracy1 <- sum(diag(result_test1))/sum(result_test1)*100
test_accuracy1
test_recall1 <- ((result_test1[2,2])/(result_test1[2,2]+result_test1[2,1])*100)
test_recall1
test_precision1 <-((result_test1[2,2])/(result_test1[2,2]+result_test1[1,2])*100)
test_precision1
test_spec1 <-((result_test1[1,1])/(result_test1[1,1]+result_test1[1,2])*100)
test_spec1

write.csv(pred_model_test1, file = "rf_test.csv",row.names=FALSE, na="")

#####################################################       KNN    ###############################################

########################################################################
# With standardization
########################################################


data_knn<-data


cat<-subset(data_knn,select=c(ActGPI, RouteOfAdmin,State,MailRetail,Sex,PurchasedBy,Pharmacy))
num<-subset(data_knn,select=-c(ActGPI,patient_medication,RouteOfAdmin,State,MailRetail,Sex,PurchasedBy,Pharmacy,target))
#cat<-subset(data_knn,select=c(RouteOfAdmin,State,MailRetail,Sex,PurchasedBy,Pharmacy))
#num<-subset(data_knn,select=-c(ActGPI,patient_medication,RouteOfAdmin,State,MailRetail,Sex,PurchasedBy,Pharmacy,target))
num <- data.frame(apply(num,2,function(x){as.character(x)}))
num <- data.frame(apply(num,2,function(x){as.numeric(x)}))


library(vegan)
num=decostand(num,"range") # to standardize the data using 'Range' method


library(dummies)
dummy <- dummy.data.frame(cat,sep = ".")
#str(dummy)

data_knn=data.frame(data_knn$patient_medication,dummy,num,data_knn$target)
colnames(data_knn)[1]<-"patient_medication"
colnames(data_knn)[166]<-"target"

# set.seed(123) 
# # to take a random sample of  60% of the records for train data 
# knn = sample(1:nrow(data_knn),nrow(data_knn)*0.7) 
# data_train = data_knn[knn,] 
# data_test =data_knn[-knn,]


train_knn=data_knn[0,]
test_knn=data_knn[0,]
attach(data_knn)
for(i in 1:(nrow(data_knn)))
{
  if((patient_medication[i]==patient_medication[i+1]) && (i!=nrow(data_knn)))
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

library(class)
# N = 1/3/5/7



start.time<-Sys.time()
pred_knn_train= knn(trainwithoutclass,trainwithoutclass, train_knn$target , k = 3)
end.time<-Sys.time()
time.taken<-end.time-start.time
time.taken




a = table(train_knn$target,pred_knn_train)
a
accu = sum(diag(a))/nrow(trainwithoutclass)
accu

prec = a[2,2]/sum(a[,2])
prec
recal = a[2,2]/sum(a[2,])
recal
specif=a[1,1]/sum(a[1,])
specif


start.time<-Sys.time()
pred_knn_test = knn(trainwithoutclass,testwithoutclass, train_knn$target , k = 3)
end.time<-Sys.time()
time.taken<-end.time-start.time
time.taken




a = table(test_knn$target,pred_knn_test)
a
accu = sum(diag(a))/nrow(testwithoutclass)
accu

prec = a[2,2]/sum(a[,2])
prec
recal = a[2,2]/sum(a[2,])
recal
specif=a[1,1]/sum(a[1,])
specif

write.csv(pred_knn_test, file = "test_knn.csv",row.names=FALSE, na="")

################################################################  SVM   #####################################################
#############################################################################################################################

####Classification using "e1071"####
# install.packages("e1071")
library(e1071)

# Store the independent variables and target variable separately
# (for easy use)
x = trainwithoutclass
y = train_knn$target

model = svm(x,y, method = "C-classification", kernel = "radial", cost = 10, gamma = 0.1)
#summary(model)


# 
# library(kernlab)
# names(train_bankdata)
#Build model using ksvm with "rbfdot" kernel
#model <- ksvm(as.matrix(x),y,type='C-svc',kernel="rbfdot",kpar="automatic",C=10, cross=5)
#kern_rbf

# Building the model on train data
#model  =  svm(x = x, y = y, type = "C-classification", kernel = "linear", cost = 10)
#summary(model)
#The "cost" parameter balances the trade-off between having a large margin and classifying
#all points correctly. It is important to choose it well to have good
#generalization.

# Predict on train data using the model
svm_pred_train=  predict(model, x) # x is trainwithoutclass
table(svm_pred_train)

# Build confusion matrix ("loan-takers":1; "non loan-takers":0)
# compare actual (i.e. "y") vs. predicted (pred_train)
tb_train = table(y,svm_pred_train)#actual is on left and predicted shown on top
tb_train
# Calculate error metrics

accuracy_train = sum(diag(tb_train))/sum(tb_train)
accuracy_train

precision = tb_train[2,2]/sum(tb_train[,2])
precision

recall_train = (tb_train[2,2]/(tb_train[2,2]+tb_train[2,1]))
recall_train

spec_train=tb_train[1,1]/sum(tb_train[1,])
spec_train

# Predict on test data using the model
a  = testwithoutclass
b  = test_knn$target
svm_pred_test = predict(model, a)
table(svm_pred_test)

# Build confusion matrix ("loan-takers":1; "non loan-takers":0)
#compare actual (i.e. "b") vs. predicted (pred_test)
tb_test <- table(b,svm_pred_test)
tb_test
accuracy_test = sum(diag(tb_test))/sum(tb_test)
accuracy_test
precision_test = tb_test[2,2]/sum(tb_test[,2])
precision_test
recall_test = (tb_test[2,2]/(tb_test[2,2]+tb_test[2,1]))
recall_test
specificity_test = (tb_test[1,1]/sum(tb_test[1,]))
specificity_test


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
dt_train
dt_accuracy_train = sum(diag(dt_train))/sum(dt_train)
dt_accuracy_train
dt_precision_train = dt_train[2,2]/sum(dt_train[,2])
dt_precision_train
dt_recall_train = (dt_train[2,2]/(dt_train[2,2]+dt_train[2,1]))
dt_recall_train

dt_specificity_train = (dt_train[1,1]/sum(dt_train[1,]))
dt_specificity_train

#Predicting on Test
P1_test = predict(Model_C50,a)
#P1_test
dt_test=table(b,Predicted=P1_test)
dt_test
dt_accuracy_test = sum(diag(dt_test))/sum(dt_test)
dt_accuracy_test
dt_precision_test = dt_test[2,2]/sum(dt_test[,2])
dt_precision_test
dt_recall_test = (dt_test[2,2]/(dt_test[2,2]+dt_test[2,1]))
dt_recall_test
dt_specificity_test = (dt_test[1,1]/sum(dt_test[1,]))
dt_specificity_test


######################################### R PART ##############################
#############################################################################3#
#rpart
library(rpart)
library(rpart.plot)

Model_rpart= rpart(target~ QTY + Age + Sex + PurchasedBy + Pharmacy+ t,data=train, method="class")
Model_rpart
plot(Model_rpart)
rpart.plot(Model_rpart,type=3,extra=103,fallen.leaves = FALSE)



#Predicting on Train
P1_train_rpart=predict(Model_rpart,train,type="class")
table(train$target,predicted=P1_train_rpart)


#P1_train
rp_train=table(y,Predicted=P1_train_rpart)
rp_accuracy_train = sum(diag(rp_train))/sum(rp_train)
rp_accuracy_train
rp_precision_train = rp_train[2,2]/sum(rp_train[,2])
rp_precision_train
rp_recall_train = (rp_train[2,2]/(rp_train[2,2]+rp_train[2,1]))
rp_recall_train

rp_specificity_train = (rp_train[1,1]/sum(rp_train[1,]))
rp_specificity_train


#Predicting on Test
P1_test_rpart=predict(Model_rpart,test,type="class")
#table(test$target,predicted=P1_test_rpart)


rp_test=table(b,Predicted=P1_test_rpart)
rp_test
rp_accuracy_test = sum(diag(rp_test))/sum(rp_test)
rp_accuracy_test
rp_precision_test = rp_test[2,2]/sum(rp_test[,2])
rp_precision_test
rp_recall_test = (rp_test[2,2]/(rp_test[2,2]+rp_test[2,1]))
rp_recall_test
rp_specificity_test = (rp_test[1,1]/sum(rp_test[1,]))
rp_specificity_test






#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################            NEURAL NETWORKS          ###########################################

NN_train.x = data.matrix(x)
NN_train.y = as.numeric(y)


NN_train.y=replace(NN_train.y,NN_train.y==1,0)
NN_train.y=replace(NN_train.y,NN_train.y==2,1)

NN_test.x = data.matrix(a)
NN_test.y = b


require(mxnet)

mx.set.seed(0)
Sys.time() -> start
model <- mx.mlp(NN_train.x, NN_train.y, hidden_node=c(10), out_node=1, activation="tanh", out_activation="logistic",
                num.round=200, array.batch.size=200, learning.rate=0.01, momentum=0.9,
                eval.metric=mx.metric.accuracy)
Sys.time() -> end
paste(end - start)

preds_NN_train = predict(model, NN_train.x)

preds_NN_train=t(preds_NN_train)
pred.label.train = ifelse(preds_NN_train<0.62, 0, 1)

conf.mat = table(NN_train.y,pred.label.train)
conf.mat
accuracy = sum(diag(conf.mat))/sum(conf.mat)
accuracy
precision = conf.mat[2,2]/sum(conf.mat[2,])
precision
recall = conf.mat[2,2]/sum(conf.mat[,2])
recall
spec=conf.mat[1,1]/sum(conf.mat[1,])
spec




preds_NN_test = predict(model, NN_test.x)

preds_NN_test=t(preds_NN_test)
pred.label.test = ifelse(preds_NN_test<0.65, 0, 1)

conf.mat = table(b,pred.label.test)
conf.mat
accuracy = sum(diag(conf.mat))/sum(conf.mat)
accuracy
precision = conf.mat[2,2]/sum(conf.mat[2,])
precision
recall = conf.mat[2,2]/sum(conf.mat[,2])
recall
spec=conf.mat[1,1]/sum(conf.mat[1,])
spec
write.csv(pred.label.test, file = "nn_test.csv",row.names=FALSE, na="")




#########END########
############################################## ENSEMBLE Logistic  ##############################################
#######################################################################################################################




# (4) Combining training predictions of LOGISTIC, RANDOM FOREST, KNN, SVM, C5.0, RPART, NEURAL  Regression together
train_pred_all_models <- cbind(glm_pred,pred_model_train1,pred_knn_train,svm_pred_train,P1_train,P1_train_rpart,pred.label.train)
train_pred_all_models <- data.frame(apply(train_pred_all_models, 2, function(x) {as.factor(x)}))
# or first use "apply" then type data_ensemble <- data.frame(data_ensemble)
# str(train_pred_all_models)
# summary(train_pred_all_models)
# rm(fitted_class,pred,pred_test)

test_pred_all_models <- cbind(fitted_class,pred_model_test1,pred_knn_test,svm_pred_test,P1_test,P1_test_rpart,pred.label.test)
test_pred_all_models <- data.frame(apply(test_pred_all_models, 2, function(x) {as.factor(x)}))
# 
# 
# # (5) Viewing the predictions of each model
# table(train_pred_all_models$fitted_class) #Logistic 
# table(train_pred_all_models$pred) #KNN
# table(train_pred_all_models$pred_test) #SVM
# table(b) #Original Dataset DV
# table(test$target)



# (6) Adding the original DV to the dataframe
train_pred_all_models <- cbind(train_pred_all_models, train$target)
colnames(train_pred_all_models)[8] = "target"
colnames(train_pred_all_models)[7] = "neural"


#test_pred_all_models <- cbind(test_pred_all_models, test$target)
#colnames(test_pred_all_models)[8] = "target"
colnames(test_pred_all_models)[7] = "neural"

colnames(train_pred_all_models)[1:7] <- c("logistic", "rf", "knn","svm","c5.0","rpart","neural")
colnames(test_pred_all_models)[1:7] <- c("logistic", "rf", "knn","svm","c5.0","rpart","neural")
#major_vote=replace(major_vote,major_vote==1,0)
#major_vote=replace(major_vote,major_vote==2,1)

# # (7) Ensemble Model with GLM as Meta Learner
# str(train_pred_all_models)
# head(train_pred_all_models)

glm_ensemble <- glm(target ~ ., train_pred_all_models, family = binomial())
summary(glm_ensemble)

# (8) Check the "glm_ensemble model" on the train data

pred_ensemble_train <- predict(object = glm_ensemble, train_pred_all_models, type = "response")
pred_ensemble_train <- ifelse(pred_ensemble_train > 0.7, 1, 0)
table(pred_ensemble_train)

check4 <- table(train_pred_all_models$target,pred_ensemble_train)
check4
accuracy_train = sum(diag(check4))/sum(check4)
accuracy_train
precision_train = check4[2,2]/sum(check4[2,])
precision_train
recall_train = check4[2,2]/sum(check4[,2])
recall_train
spec=check4[1,1]/sum(check4[1,])
spec

pred_ensemble_test <- predict(glm_ensemble, test_pred_all_models, type = "response")
pred_ensemble_test <- ifelse(pred_ensemble_test > 0.7, 1, 0)
table(pred_ensemble_test)

check4 <- table(test$target,pred_ensemble_test)
check4
accuracy_train = sum(diag(check4))/sum(check4)
accuracy_train
precision_train = check4[2,2]/sum(check4[2,])
precision_train
recall_train = sum(check4[2,2])/sum(check4[,2])
recall_train

spec=check4[1,1]/sum(check4[1,])
spec





############################################## ENSEMBLE MAJORITY VOTING  ##############################################
#######################################################################################################################

#The majority vote train

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

maj_train=cbind(glm_pred,pred_model_train1,pred_knn_train,svm_pred_train,P1_train,P1_train_rpart,pred.label.train)
maj_train=data.frame(apply(maj_train, 2, function(x) {as.factor(x)}))
major_vote=vector(length=nrow(train))

for(i in 1:nrow(train))
{
  major_vote[i]=as.character(getmode(maj_train[i,]))
}

table(major_vote)
#major_vote=replace(major_vote,major_vote==1,0)
#major_vote=replace(major_vote,major_vote==2,1)


major_tab <- table(train$target,major_vote)
major_tab
accuracy_train = sum(diag(major_tab))/sum(major_tab)
accuracy_train
precision_train = major_tab[2,2]/sum(major_tab[,2])
precision_train
recall_en_train = major_tab[2,2]/sum(major_tab[2,])
recall_en_train
specificity_en_train = major_tab[1,1]/sum(major_tab[1,])
specificity_en_train

#The majority vote test

maj_test=cbind(fitted_class,pred_model_test1,pred_knn_test,svm_pred_test,P1_test,P1_test_rpart,pred.label.test)
maj_test=data.frame(apply(maj_test, 2, function(x) {as.factor(x)}))
major_vote=vector(length=nrow(test))
  
for(i in 1:nrow(maj_test))
  {
    major_vote[i]=as.character(getmode(maj_test[i,]))
  }

table(major_vote)
#major_vote=replace(major_vote,major_vote==1,0)
#major_vote=replace(major_vote,major_vote==2,1)


major_tab <- table(test$target,major_vote)
major_tab
accuracy_train = sum(diag(major_tab))/sum(major_tab)
accuracy_train
precision_train = major_tab[2,2]/sum(major_tab[,2])
precision_train
recall_en_train = major_tab[2,2]/sum(major_tab[2,])
recall_en_train
specificity_en_train = major_tab[1,1]/sum(major_tab[1,])
specificity_en_train

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
                                    ###                MODEL 2 --- MODE           ###


model2<-data
#getmode(model2$target)
j=1;
model2_train=model2[0,]
model2_test=model2[0,]
mode=0
mode=vector(length=nrow(test))
#model2_test=cbind(model2_test,m)
attach(model2)
for(i in 1:(nrow(model2)))
{
  if((patient_medication[i]==patient_medication[i+1])  && (i!=nrow(model2)))  
  {
    model2_train=rbind(model2_train,model2[i,])
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



######################################################################
######################      TEST          ############################
conf.mat_mode = table(model2_test$target,mode)
conf.mat_mode
accuracy_mode = sum(diag(conf.mat_mode))/sum(conf.mat_mode)
accuracy_mode
precision_mode = conf.mat_mode[2,2]/sum(conf.mat_mode[,2])
precision_mode
recall_mode = conf.mat_mode[2,2]/sum(conf.mat_mode[2,])
recall_mode

specificity_mode = conf.mat_mode[1,1]/sum(conf.mat_mode[1,])
specificity_mode

write.csv(mode, file = "mode.csv",row.names=FALSE, na="")


write.csv(test, file = "test.csv",row.names=FALSE, na="")

write.csv(train, file = "train.csv",row.names=FALSE, na="")
write.csv(delay, file = "delay.csv",row.names=FALSE, na="")

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#######################################        ENSEMBLE OF GOOD MODELS            ###########################


# (4) Combining training predictions of LOGISTIC, RANDOM FOREST, KNN, SVM, C5.0, RPART, NEURAL  Regression together
train_pred_all_models <- cbind(glm_pred,pred_model_train1,pred_knn_train,svm_pred_train,pred.label.train)
train_pred_all_models <- data.frame(apply(train_pred_all_models, 2, function(x) {as.factor(x)}))
# or first use "apply" then type data_ensemble <- data.frame(data_ensemble)
# str(train_pred_all_models)
# summary(train_pred_all_models)
# rm(fitted_class,pred,pred_test)

test_pred_all_models <- cbind(fitted_class,pred_model_test1,pred_knn_test,svm_pred_test,pred.label.test)
test_pred_all_models <- data.frame(apply(test_pred_all_models, 2, function(x) {as.factor(x)}))
# 
# 
# # (5) Viewing the predictions of each model
# table(train_pred_all_models$fitted_class) #Logistic 
# table(train_pred_all_models$pred) #KNN
# table(train_pred_all_models$pred_test) #SVM
# table(b) #Original Dataset DV
# table(test$target)



# (6) Adding the original DV to the dataframe
train_pred_all_models <- cbind(train_pred_all_models, train$target)
colnames(train_pred_all_models)[6] = "target"
colnames(train_pred_all_models)[5] = "neural"


#test_pred_all_models <- cbind(test_pred_all_models, test$target)
#colnames(test_pred_all_models)[8] = "target"
colnames(test_pred_all_models)[5] = "neural"

colnames(train_pred_all_models)[1:5] <- c("logistic", "rf", "knn","svm","neural")
colnames(test_pred_all_models)[1:5] <- c("logistic", "rf", "knn","svm","neural")
#major_vote=replace(major_vote,major_vote==1,0)
#major_vote=replace(major_vote,major_vote==2,1)

# # (7) Ensemble Model with GLM as Meta Learner
# str(train_pred_all_models)
# head(train_pred_all_models)

glm_ensemble <- glm(target ~ ., train_pred_all_models, family = binomial())
summary(glm_ensemble)

# (8) Check the "glm_ensemble model" on the train data

pred_ensemble_train <- predict(object = glm_ensemble, train_pred_all_models, type = "response")
pred_ensemble_train <- ifelse(test = pred_ensemble_train > 0.7, 1, 0)
table(pred_ensemble_train)

check4 <- table(train_pred_all_models$target,pred_ensemble_train)
check4
accuracy_train = sum(diag(check4))/sum(check4)
accuracy_train
precision_train = check4[2,2]/sum(check4[2,])
precision_train
recall_train = check4[2,2]/sum(check4[,2])
recall_train
spec=check4[1,1]/sum(check4[1,])
spec

pred_ensemble_test <- predict(glm_ensemble, test_pred_all_models, type = "response")
pred_ensemble_test <- ifelse(test = pred_ensemble_test > 0.7, 1, 0)
table(pred_ensemble_test)

check4 <- table(test$target,pred_ensemble_test)
check4
accuracy_train = sum(diag(check4))/sum(check4)
accuracy_train
precision_train = check4[2,2]/sum(check4[2,])
precision_train
recall_train = sum(check4[2,2])/sum(check4[,2])
recall_train

spec=check4[1,1]/sum(check4[1,])
spec

##########################################################################################3
##########################################################################################
##########################################################################################
##########################################################################################3
##########################################################################################
##########################################################################################

#                                 FINAL - MODEL      

log<-read.csv("test_log.csv",header = TRUE)
rf<-read.csv("rf_test.csv",header = TRUE)
knn<-read.csv("test_knn.csv",header = TRUE)
nn<-read.csv("nn_test.csv",header = TRUE)
mean<-read.csv("mean.csv",header = TRUE)

test<-read.csv("test.csv",header = TRUE)


final<-cbind(log,rf,knn,nn,mean)
colnames(final)[1]="Logistic"
colnames(final)[2]="Random forest"
colnames(final)[3]="KNN"
colnames(final)[4]="Neural"
colnames(final)[5]="mean"

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

pred_mode=vector(length = nrow(final))

for(i in 1:nrow(final))
{
  pred_mode[i]=as.character(getmode(final[i,]))
  
}

result=table(test$target,pred_mode)
result
accuracy = sum(diag(result))/sum(result)
accuracy
precision = result[2,2]/sum(result[,2])
precision
recall = result[2,2]/sum(result[2,])
recall
specificity = result[1,1]/sum(result[1,])
specificity



end.time<-Sys.time()
time.taken<-end.time-start.time1
time.taken


