
#Loading Important Libraries
library(ggplot2)
library(pROC)
library(mice)
library(caret)
library(plotly)
library(tidyverse)
library(factoextra)
library(yardstick)
library(kernlab)
library(e1071)
library(ROCR)
library(naivebayes)

#Data Pre-processing

#Importing Dataset
j<-read.csv("E:/Modules/heart_disease_modified.csv")

#Take a look on the data

head(j)
tail(j)
summary(j)


#Observation on some variables

j %>% group_by(pace_maker) %>% count()
j %>% group_by(drug) %>% count()
j %>% group_by(fam_hist) %>% count()

#Encoding Categorical Data into Numeric Data

#Drug variable
for(i in 1:nrow(j)){
  if(j[i,19]=="Aspirin"){
    j[i,19]=1
  }else if(j[i,19]=="Clopidogrel"){
    j[i,19]=3
  }else if(j[i,19]=="Both"){
    j[i,19]=2
  }else if(j[i,19]=="None"){
    j[i,19]=0
  }
}

ncol(j)

#fam_hist variable

for(i in 1:nrow(j)){
  if(j[i,21]=="yes"){
    j[i,21]=1
  }else if(j[i,21]=="no"){
    j[i,21]=2
  }
}

#Decoding the Class attribute into Categorical Data
for(i in 1:nrow(j)){
  if(j[i,22]==1){
    j[i,22]="yes"
  }else if(j[i,22]==0){
    j[i,22]="no"
  }
}

j$drug<-as.numeric(j$drug)
j$fam_hist<-as.numeric(j$fam_hist)

#Removing pace_maker variable since it is independent variable

j <-j%>% select(-pace_maker)


#Recheck
str(j)

j$class<-as.factor(j$class)
levels(j$class) <- c("yes", "no")

#Sampling/Partitioning of Data

partition_j<-createDataPartition(j[,"class"],p=0.6,list = FALSE)
train_j<-j[partition_j,]
test_j<-j[-partition_j,]

nrow(train_j)

#Run the classifier with default parameters.

names(getModelInfo()) #Modelling Ensembles

#NAIVE BAYES CLASSIFICATION METHOD

nj <- naiveBayes(class ~., data=train_j)
nj

control <- trainControl(method="cv", number="7", 
                        savePredictions = TRUE, classProbs = TRUE)
control


nj <- train(class ~., data = train_j, method = "naive_bayes",
            trControl = control)

#Prediction
pj <- predict(nj,test_j)
table(predicted=pj, truth=test_j$class)

#Model performance by confusion matrix
confusionMatrix(table(pj,test_j$class))

#Model Perfomance by ROC curve
njResult <- nj$pred %>%
  roc_auc(obs,yes) %>%            
  select(.estimate) %>% round(2) %>%     
  cbind(nj$pred, "model" = paste0("nj AUC = ",.))

plotnj <- njResult %>% roc_curve(obs,yes) %>% autoplot +
  theme(text = element_text(size=10)) + aes(label=.threshold)

plotnj %>% ggplotly()


#Model Perfomance by AUC
njResult$model[1]


#SUPPORT VECTOR MACHINE (SVM)
control <- trainControl(method="cv", number="7", 
                        savePredictions = TRUE, classProbs = TRUE)
control

svmFit <- train(class ~., data = train_j, method = "svmLinear",
                trControl = control)

svmFit

head(svmFit)

#Prediction Model
pred <-predict(svmFit,test_j)
pred

#Model Perfomance by Confusion Matrix
confusionMatrix(pred, test_j$class)

#Model Perfomance by ROC Curve
svmFitResult <- svmFit$pred %>%
  roc_auc(obs,yes) %>%            # to get AUC on graph
  select(.estimate) %>% round(2) %>%
  cbind(svmFit$pred, "model" = paste0("svmFit AUC = ",.))

plotSVM <- svmFitResult %>% roc_curve(obs,yes) %>% autoplot +
  theme(text = element_text(size=10)) + aes(label=.threshold)

plotSVM %>% ggplotly()

#Model Perfomance by AUC
svmFitResult$model[1]






























































