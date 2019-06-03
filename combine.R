log<-read.csv("test_log.csv",header = TRUE)
knn<-read.csv("test_knn.csv",header = TRUE)
mean<-read.csv("mean.csv",header = TRUE)
test<-read.csv("test.csv",header = TRUE)


final<-cbind(log,knn,mean)
colnames(final)[1]="Logistic"
colnames(final)[2]="knn"
colnames(final)[3]="mean"

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

