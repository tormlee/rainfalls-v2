# load library
library(kernlab)
library(jsonlite)
library(caret)
require(gdata)
library(binaryLogic)
data.train <- read.csv("data/tuning/rainfall_train.csv", header=TRUE)
data.test <- read.csv("data/tuning/rainfall_test.csv", header=TRUE)
svm.rs.log <- "result/svm.wet.dry.classification.all.log"
# features
# "shum700", "R850", "U700", "Vas", "V700", "R500", "V925", "P500", "shum500", "shum850", "rhum", "shum400", "P850", "shum300", "V500"
col.name <- c("shum700", "R850", "U700", "Vas", "V700", "R500", "V925", "P500", 
              "shum500", "shum850", "rhum", "shum400", "P850", "shum300", "V500", "clazz")
data.train.ft <- data.frame(data.train$shum700, data.train$R850, data.train$U700, data.train$Vas, data.train$V700, data.train$R500,
                            data.train$V925, data.train$P500, data.train$shum500, data.train$shum850, data.train$rhum, data.train$shum400,
                            data.train$P850, data.train$shum300, data.train$V500, data.train$clazz)
data.test.ft <- data.frame(data.test$shum700, data.test$R850, data.test$U700, data.test$Vas, data.test$V700, data.test$R500,
                           data.test$V925, data.test$P500, data.test$shum500, data.test$shum850, data.test$rhum, data.test$shum400,
                           data.test$P850, data.test$shum300, data.test$V500, data.test$clazz)
colnames(data.train.ft) <- col.name
colnames(data.test.ft) <- col.name
# feature selection
n.ft <- length(col.name) -1
for(i in c(951:1000)) { # (2^n.ft -1)
    # info features for debug
    xbar.bi <- as.binary(i, n=n.ft)
    index.feature <- which(xbar.bi)
    select.ft <-  paste(col.name[index.feature],collapse=",")
    info <- sprintf("features: %s || step = %d || mask: %s", select.ft, i, paste(xbar.bi,collapse=" "))
    print(info)
    # begin training model with tuning parameters for SVM
    c_value <-c(1,10,100,1000)
    sig_value <- c(0.001,0.01,0.1)
    for(i_c in c_value){
      for(i_sig in sig_value){
        data.train.x <- data.matrix(data.train.ft[, index.feature])
        data.train.y <- data.train.ft$clazz
        data.test.x <- data.matrix(data.test.ft[, index.feature])
        data.test.y <- data.test.ft$clazz
        svp <- ksvm(data.train.x, data.train.y, type="C-svc", kernel='rbf', kpar=list(sigma=i_sig), C=i_c)
        # predict labels on test
        train.data.ypred = predict(svp, data.train.x)
        test.data.ypred = predict(svp, data.test.x)
        # table(data.test.y, data.ypred)
        # compute accuracy
        train.rain.acc <- c(sum(train.data.ypred[which(data.train.y==1)] == 1)/(sum(train.data.ypred==1)))
        train.dry.acc <- c(sum(train.data.ypred[which(data.train.y==-1)]==-1)/(sum(train.data.ypred==-1)))
        train.acc <- c(sum(train.data.ypred==data.train.y)/length(data.train.y))
        test.rain.acc <- c(sum(test.data.ypred[which(data.test.y==1)] == 1)/(sum(test.data.ypred==1)))
        test.dry.acc <- c(sum(test.data.ypred[which(data.test.y==-1)]==-1)/(sum(test.data.ypred==-1)))
        test.acc <- c(sum(test.data.ypred==data.test.y)/length(data.test.y))
        c <- c(i_c)
        sigma <- c(i_sig)
        df.result <- data.frame(select.ft, c, sigma, train.rain.acc, train.dry.acc, train.acc, test.rain.acc, test.dry.acc, test.acc)
        line <- toJSON(df.result)
        write(line,file=svm.rs.log,append=TRUE)
      }
    }
}
