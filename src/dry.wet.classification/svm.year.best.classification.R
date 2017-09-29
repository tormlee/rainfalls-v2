# load library
library(kernlab)
library(jsonlite)
library(caret)
require(gdata)
library(binaryLogic)
data.train <- read.csv("data/tuning/rainfall_train.csv", header=TRUE)
data.test <- read.csv("data/tuning/rainfall_test.csv", header=TRUE)
# features uswrf,Temp,temp700,temp850,
# ft <- "shum700, R850, R500, shum500, shum850, V700, rhum,  shum400, P850, shum300, V500, shum925, mslp, dlwrf, uswrf, V850, U700, U850, P500, dswrf, clazz"
ft <- "shum700, R850, U700, Vas, V700, R500, V925, P500, rhum, P850, U850, shum850, shum500, U925, V850, temp700, U500, shum300, ulwrf, Temp, shum400, temp850, V500, mslp, dswrf, shum925, temp500, clazz" # 
ft.choose <- unlist(strsplit(ft, ", "))
col.name <- colnames(data.train)
ind.choose <- c()
for(ft_i in c(1:length(ft.choose))){
  ind <- which(col.name == ft.choose[ft_i])
  # print(ind)
  # print(ft.choose[ft_i])
  ind.choose <- c(ind.choose, ind)
}
ind.choose <- sort(ind.choose)
data.train.ft <- data.train[, ind.choose]
data.test.ft <- data.test[, ind.choose]
colnames(data.train.ft) <- ft.choose
colnames(data.test.ft) <- ft.choose
# modeling
c_value <- c(1) # c(1,10,100,1000)
sig_value <- c(0.1) # c(0.001,0.01,0.1)
for(i_c in c_value){
    for(i_sig in sig_value){
      data.train.x <- data.matrix(data.train.ft[, -ncol(data.train.ft)])
      data.train.y <- data.train.ft$clazz
      data.test.x <- data.matrix(data.test.ft[, -ncol(data.train.ft)])
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
      df.result <- data.frame(c, sigma, train.rain.acc, train.dry.acc, train.acc, test.rain.acc, test.dry.acc, test.acc)
      line <- toJSON(df.result)
      print(line)
    }
}
