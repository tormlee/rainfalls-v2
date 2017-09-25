start <- Sys.time()
system.time(x.x <- square.cal(10000000))
end <- Sys.time()
time <- end - start



square.cal <- function(x){
  multiply <- 1
  for(i in c(2:x)){
    multiply <- multiply * i
  }
  return (multiply)
}



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
loop <- 0
step <- 0
while(TRUE){  
  acc.train.gl <- 0
  acc.test.gl <- 0
  acc.eps <- 0.1
  run.signal <- TRUE
  n.xft.choose <- length(xft.choose)
  for(i in xft.try) {
    print(i)
    step <- step + 1
    ft.ind <- c(i, xft.choose)
    n.ft <- length(xft.try)
    # info features for debug
    name.ft <- features[ft.ind]
    actual.ft.name <- paste(name.ft, collapse = ",")
    info <- sprintf("features: %s || step = %d/%d", actual.ft.name, step, n.ft)
    print(info)
    # begin training model with tuning parameters for SVM
        cat("features: "); print(actual.ft.name)
            xft.try <- xft.try[-which(xft.try == i)]
            xft.choose <- c(xft.choose, i)
            pass.ft.signal <- TRUE
  }
  loop <- loop + 1 
  cat("------------ loop: "); cat(loop); print(" --------------")
  if((length(xft.choose) - n.xft.choose) == 0 | length(xft.try) == 0){
    print("END OF PROCESS")
    break
     }
  }