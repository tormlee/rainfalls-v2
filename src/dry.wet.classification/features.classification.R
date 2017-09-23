# load library
library(kernlab)
library(jsonlite)
library(caret)
require(gdata)
library(binaryLogic)
data.train <- read.csv("data/tuning/rainfall_train.csv", header=TRUE)
data.test <- read.csv("data/tuning/rainfall_test.csv", header=TRUE)
svm.rs.log <- "result/svm.wet.dry.classification.ft.log"
xft.try <- c(2:(ncol(data.train) -2))
xft.choose <- c()
# feature selection
features <- names(data.train)
cat("range feature recusive: "); print(features[xft.try])
while(TRUE){
  step <- 0
  acc.train.gl <- 0
  acc.test.gl <- 0
  acc.eps <- 0.1
  run.signal <- TRUE
  n.xft.choose <- length(xft.choose)
  for(i in xft.try) {
    step <- step + 1
    ft.ind <- c(i, xft.choose)
    n.ft <- length(xft.try)
    # info features for debug
    name.ft <- features[ft.ind]
    actual.ft.name <- paste(name.ft, collapse = ",")
    info <- sprintf("features: %s || step = %d/%d", actual.ft.name, step, n.ft)
    print(info)
    # data
    data.train.x <- data.matrix(data.train[, ft.ind])
    data.train.y <- data.train$clazz
    data.test.x <- data.matrix(data.test[, ft.ind])
    data.test.y <- data.test$clazz
    # begin training model with tuning parameters for SVM
    c_value <- c(1, 10) # c(1,10,100,1000)
    sig_value <-  c(0.1) # c(0.001,0.01,0.1)
    pass.ft.signal <- FALSE
    for(i_c in c_value){
      for(i_sig in sig_value){
        svp <- ksvm(data.train.x, data.train.y, type="C-svc", kernel='rbf', kpar=list(sigma=i_sig), C=i_c)
        print(sprintf("c = %d, sigma = %f", i_c, i_sig))
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
        train.test.bar <- abs(train.acc - test.acc)
        cat("features: "); print(actual.ft.name)
        if(train.test.bar <  acc.eps & train.acc > acc.train.gl & test.acc > acc.test.gl){
          if(pass.ft.signal == FALSE){
            xft.try <- xft.try[-which(xft.try == i)]
            xft.choose <- c(xft.choose, i)
            pass.ft.signal <- TRUE
          }
          acc.train.gl <- train.acc
          acc.test.gl <- test.acc
          # logging info
          c <- c(i_c)
          sigma <- c(i_sig)
          df.result <- data.frame(info, c, sigma, train.rain.acc, train.dry.acc, train.acc, test.rain.acc, test.dry.acc, test.acc)
          line <- toJSON(df.result)
          print(line)
          write(line,file=svm.rs.log,append=TRUE) 
        }
      }
    }
  }
  print("--------------------------------------------")
  if((length(xft.choose) - n.xft.choose) == 0 | length(xft.try) == 0){
    print("END OF PROCESS")
    break
  }
}
