
data<-read.csv("data.csv",header = TRUE)
data$PatientID<-as.factor(as.character(data$PatientID))
data$ActGPI<-as.factor(as.character(data$ActGPI))
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


library(tidyr)
#df = data.frame(year=2009:2013, customerID=20227:20231) # using akrun's data

data<-unite(data, patient_medication, c(PatientID,Medication), remove=FALSE)

#data<-data[,-c(2,3)]

#data<-subset(data,select=-c(For_How_Many_Days,Date,delay))
data$patient_medication=as.factor(as.character(data$patient_medication))

# Building a time series model
timeseries_model<-data[,-c(1,2,17,18)]

#getmode(timeseries_model$target)
j=1;
timeseries_model_train=timeseries_model[0,]
timeseries_model_test=timeseries_model[0,]

forecast=vector(length=760)
#timeseries_model_test=cbind(timeseries_model_test,m)
attach(timeseries_model)
library(forecast)



# Building a time series and auto arima model for every patient-medication combination 
for(i in 1:(nrow(timeseries_model)-1))
{
  if(patient_medication[i+1]==patient_medication[i])  
  {
    timeseries_model_train=rbind(timeseries_model_train,timeseries_model[i,])
  }
  else if((patient_medication[i-1]!=patient_medication[i])&&(patient_medication[i]!=patient_medication[i+1]))
  {
    
  }
  else
  {
    
    
    if(j==1)
    {
      TimeSeries = ts(timeseries_model_train$delay)
      autoArimaModel <- auto.arima(TimeSeries)
      autoarima_forecast <- data.frame(forecast(autoArimaModel,h=1))$Point.Forecast
      forecast[j]=autoarima_forecast
      timeseries_model_test=timeseries_model[i,]
      timeseries_model_train=timeseries_model[0,]
      j=j+1
    }
    else
    {
      TimeSeries = ts(timeseries_model_train$delay)
      autoArimaModel <- auto.arima(TimeSeries)
      autoarima_forecast <- data.frame(forecast(autoArimaModel,h=1))$Point.Forecast
      forecast[j]=autoarima_forecast
      timeseries_model_test=rbind(timeseries_model_test,timeseries_model[i,])
      timeseries_model_train=timeseries_model[0,]
      j=j+1
    }
  }
}

test_with_target=read.csv("test.csv",header = TRUE)

final<-cbind(timeseries_model_test,forecast)
target_pred=vector(length = nrow(final))
for(i in 1:nrow(final))
{
  if(final$forecast[i]<=2)
  {
    target_pred[i]=1
  }
  else
  {
    target_pred[i]=0
  }
}

major_tab <- table(test_with_target$target,target_pred)
major_tab
accuracy_train = sum(diag(major_tab))/sum(major_tab)
accuracy_train
precision_train = major_tab[2,2]/sum(major_tab[,2])
precision_train
recall_en_train = major_tab[2,2]/sum(major_tab[2,])
recall_en_train
