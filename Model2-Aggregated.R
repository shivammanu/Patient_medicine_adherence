

###############################################################################################################
###############################################################################################################
###############                     AGGREGATED MODEL                        #################################3#


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
# length(unique(data$PatientID))
# length(unique(data$ActGPI))
# length(unique(data$Medication))

delay=vector(length=nrow(data))
t=vector(length = nrow(data))
delay[1]=0

j=1;
attach(data)

#calculating delay in buying medications 
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

data<-cbind(data,delay,t)
target=vector(length = nrow(data))
attach(data)

# Classifying adherents and non adherents according to delay
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


library(tidyr)


data<-unite(data, patient_medication, c(PatientID,Medication), remove=FALSE)

#data<-data[,-c(2,3)]

#data<-subset(data,select=-c(For_How_Many_Days,Date,delay))
data$patient_medication=as.factor(as.character(data$patient_medication))


data2=data[0,]
i=1

while(i<=nrow(data))
{
  if((data$patient_medication[i]==data$patient_medication[i+1])&&(data$patient_medication[i+1]==data$patient_medication[i+2]))
  {
    while(data$patient_medication[i]==data$patient_medication[i+1])
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
# Building a time series model
timeseries_model<-data[,-c(1,2,17,18)]

timeseries_model_with_target<-data[,-c(1,2,17)]
#getmode(timeseries_model$target)
j=1;
timeseries_model_train=timeseries_model[0,]
ts_train=timeseries_model_train[0,]
timeseries_model_test=timeseries_model[0,]

forecast=vector(length=628)
mean=vector(length=628)
median=vector(length=628)

#timeseries_model_test=cbind(timeseries_model_test,m)
attach(timeseries_model)
library(forecast)


#use=data[1,]



timeseries_model=data
attach(data)
# Building a time series and auto arima model for every patient-medication combination 
for(i in 1:(nrow(timeseries_model)))
{
  if((patient_medication[i]==patient_medication[i+1]) && (i!=nrow(timeseries_model)) )
  {
    timeseries_model_train=rbind(timeseries_model_train,timeseries_model[i,])
    #ts_train=rbind(ts_train,timeseries_model_train)
  }
  
  else
  {
    
    TimeSeries = ts(timeseries_model_train$delay)
    autoArimaModel <- auto.arima(TimeSeries)
    autoarima_forecast <- data.frame(forecast(autoArimaModel,h=1))$Point.Forecast
    forecast[j]=autoarima_forecast
    mean[j]=mean(TimeSeries)
    median[j]=median(TimeSeries)
    timeseries_model_train=timeseries_model[0,]
    
    
    if(j==1)
    {
      timeseries_model_test=timeseries_model[i,]
    }
    
    else
      timeseries_model_test=rbind(timeseries_model_test,timeseries_model[i,])
   
    j=j+1
  }
}

#test_with_target=read.csv("test.csv",header = TRUE)

final<-cbind(timeseries_model_test,forecast,mean,median)
target_pred=vector(length = nrow(final))
for(i in 1:nrow(final))
{
  if(final$forecast[i]<=1)
  {
    target_pred[i]=1
  }
  else
  {
    target_pred[i]=0
  }
}
########   TIME SERIES RESULTS    ####################
major_tab <- table(timeseries_model_test$target,target_pred)
major_tab
accuracy_train = sum(diag(major_tab))/sum(major_tab)
accuracy_train
precision_train = major_tab[2,2]/sum(major_tab[,2])
precision_train
recall_en_train = major_tab[2,2]/sum(major_tab[2,])
recall_en_train
specificity_train=major_tab[1,1]/sum(major_tab[1,])
specificity_train





###########################   MEAN    ##############################
target_pred=vector(length = nrow(final))
for(i in 1:nrow(final))
{
  if(final$mean[i]<=1)
  {
    target_pred[i]=1
  }
  else
  {
    target_pred[i]=0
  }
}
########   MEAN RESULTS    ####################
major_tab <- table(timeseries_model_test$target,target_pred)
major_tab
accuracy_train = sum(diag(major_tab))/sum(major_tab)
accuracy_train
precision_train = major_tab[2,2]/sum(major_tab[,2])
precision_train
recall_en_train = major_tab[2,2]/sum(major_tab[2,])
recall_en_train
specificity_train=major_tab[1,1]/sum(major_tab[1,])
specificity_train
write.csv(target_pred, file = "mean.csv",row.names=FALSE, na="")


###########################   MEDIAN    ##############################
target_pred=vector(length = nrow(final))
for(i in 1:nrow(final))
{
  if(final$median[i]<=1)
  {
    target_pred[i]=1
  }
  else
  {
    target_pred[i]=0
  }
}
########   MEDIAN RESULTS    ####################
major_tab <- table(timeseries_model_test$target,target_pred)
major_tab
accuracy_train = sum(diag(major_tab))/sum(major_tab)
accuracy_train
precision_train = major_tab[2,2]/sum(major_tab[,2])
precision_train
recall_en_train = major_tab[2,2]/sum(major_tab[2,])
recall_en_train
specificity_train=major_tab[1,1]/sum(major_tab[1,])
specificity_train

