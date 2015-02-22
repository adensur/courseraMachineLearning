library(ggplot2)
library(caret)
library(dplyr)
#reading the data
data=read.csv("pml-training.csv")
data.test=read.csv("pml-testing.csv")

#cleaning data
View(tr)
a=data[data$classe=="A",7]
b=data[data$classe=="B",7]
c=data[data$classe=="C",7]
d=data[data$classe=="D",7]
e=data[data$classe=="E",7] #all num_windows have their own classe


#creating simple data partitions. 80% - train, 20% - test
ind=createDataPartition(data[,160],times = 1,p = 0.7,list = FALSE)
tr=data[ind,]
te=data[-ind,]

#performing simple Random Forest to all vars except timestamps:




#
preprocess=function(tr){
  I=NULL
  for(i in 8:159){
    tr[,i]=as.numeric(as.character(tr[,i]))
    if(sum(is.na(tr[,i]))>0){
     # print(i)
      I=c(I,i)
    } 
  }
  tr[is.na(tr)]=0
  tr$new_window=as.character(tr$new_window)
  tr$new_window[tr$new_window=="yes"]=1
  tr$new_window[tr$new_window=="no"]=0
  
  tr=tr[,-c(1:7,I)]
  tr
}
tr=preprocess(tr)
te=preprocess(te)





#model
fit=train(classe~.,method="rf",data=tr)
pre=predict(fit,newdata = te)
confusionMatrix(data = pre,reference = te$classe)
