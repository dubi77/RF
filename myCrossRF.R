setwd('D:/myFoldPath')

library(data.table)
library(randomForest)
library(pROC)

asd <- read.delim('abundanceTable.txt', row.names = 1, sep = ',', stringsAsFactors = FALSE, check.names = FALSE)
asd <- data.frame(asd) 

max_auc <- 0.1
CROSS_SUM <- 10

seedList <- c(1:100)
for(seed_index in seedList)
{
  set.seed(seed_index)
  ind=sample(1:CROSS_SUM,nrow(asd),replace=TRUE)  
  list<- 1:CROSS_SUM  
  prediction <- data.table()
  testsetCopy <- data.table()  
  for(cross_index in list) 
  {
    trainingset <- subset(asd, ind %in% list[-cross_index])
    testset <- subset(asd, ind %in% c(cross_index))  
    n<-length(trainingset)    	
    errRate_mtry<-c(1)
    for (i in 1:(n-1))
    {
      set.seed(seed_index)
      asd.rf<-randomForest(as.factor(LABEL)~.,data = trainingset,mtry=i,proximity=TRUE,importance=TRUE) 
      errRate_mtry[i]<-mean(asd.rf$err.rate)
    }
    ctn_mtry <- which.min(errRate_mtry)  

    errRate_ntree<-c(1)
    for (j in 1:100)
    {
      j_tree <- j * 100      
      set.seed(seed_index)
      asd.rf<-randomForest(as.factor(LABEL)~.,data = trainingset,mtry=ctn_mtry,ntree = j_tree,proximity=TRUE,importance=TRUE) 
      errRate_ntree[j]<-mean(asd.rf$err.rate)
    }
       
    ctn_ntree <- which.min(errRate_ntree)  
    set.seed(seed_index)
    asd.rf<-randomForest(as.factor(LABEL)~.,data = trainingset,mtry=ctn_mtry,ntree = ctn_ntree*100,proximity=TRUE,importance=TRUE) 
    
    x<-subset(testset,select = -LABEL) 
    temp <- as.data.frame(predict(asd.rf,x))
    prediction <- rbind(prediction, temp)
    testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,"LABEL"]))
  }
  
  result <- cbind(prediction, testsetCopy[, 1])
  names(result) <- c("Predicted", "Actual")
  rf_roc <- roc(as.numeric(result$Actual),as.numeric(result$Predicted))
  if (max_auc < rf_roc$auc)
  {
    max_auc <- rf_roc$auc
    plot(rf_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), 
         max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='ROC Curve')
  }
  
  tempStr <- paste("Features-",seed_index,".csv", sep = "")
  write.table(paste("SEED=",seed_index,"AUC=",rf_roc$auc), tempStr,
              col.names = F,row.names = F,sep = ",")
  write.table(importance(asd.rf), tempStr,col.names = T,row.names = T,sep = ",",append = T)
}



