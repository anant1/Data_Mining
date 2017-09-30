#HW1-3
rm(list = ls())
library("ElemStatLearn")
setwd("/Users/anant/Google Drive/MASTERS/Courses/545/Codes")
library("ggplot2")

########################################
### Find the 2's and 3's digit
########################################
zipTraining <- as.data.frame(zip.train)
zipTraining <- zipTraining[zipTraining$V1 ==2 | zipTraining$V1 == 3,]
dim(zipTraining)
#[1] 1389  257

zipTest <- as.data.frame(zip.test)
zipTest <- zipTest[zipTest$V1 ==2 | zipTest$V1 == 3,]
dim(zipTest)
#[1] 364 257

########################################
### Linear Regression
########################################

lin.model <- lm(zipTraining$V1 ~ ., data= zipTraining)
summary(lin.model)

lr.predict <- predict(lin.model, zipTest, se.fit = TRUE)
lin<- as.data.frame(lr.predict$fit)
lin<- lin$`lr.predict$fit`

#### Now lets approximate the values to the nearest 2's and 3's
for (i in 1:364){
  if(lin[i]> 2.5) lin[i]=3 else lin[i] =2
}

### ERROR RATE
errorrate.lin <- sum(zipTest$V1 != lin)/nrow(zipTest) #### >>>> 0.04120879
print(paste0("Accuary (Precision): ", 1 - errorrate.lin)) ## >>>>>>>>  0.9587912

table(`Actual Class` = zipTest$V1, `Predicted Class` =lin)


########################################
### KNN Classification
########################################

knnm<- function(train, test, a, k){
            
          KNN <- knn(train, test, a, k)
          return(KNN)
}

k=1
errorrate.knn.test=0
errorrate.knn.train = 0

for (i in seq(1,15,2)){
 knn.test<-knnm(zipTraining, zipTest, zipTraining$V1, i);
 knn.train<-knnm(zipTraining, zipTraining, zipTraining$V1, i);
 knn.test <-as.data.frame(knn.test);
 knn.train <-as.data.frame(knn.train);
 knn.test<-knn.test$knn.test[1:364];
 knn.train<-knn.train$knn.train[1:1389]
 errorrate.knn.test[k] <- sum(zipTest$V1!= knn.test)/nrow(zipTest);
 errorrate.knn.train[k] <- sum(zipTraining$V1!= knn.train)/nrow(zipTraining);
 #print(paste0("Accuary (Precision): ", 1 - errorrate.knn));
 k=k+1;
}
print(errorrate.knn.test)
print(errorrate.knn.train)

minimumerror.rate.knn <- min(errorrate.knn.test)
print(paste0("Accuary (Precision): ", 1 - minimumerror.rate.knn))