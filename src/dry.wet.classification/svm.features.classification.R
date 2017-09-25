# load library
source("src/data.analyse/svm.classification.R")
library(jsonlite)
library(caret)
require(gdata)
library(binaryLogic)
data.train <- read.csv("data/tuning/rainfall_train.csv", header=TRUE)
data.test <- read.csv("data/tuning/rainfall_test.csv", header=TRUE)
svm.rs.log <- "result/svm.wet.dry.classification.ft.log"
# feature selection
xft.try.ind <- c(2:(ncol(data.train) -2))
xft.choose.ind <- c()
features <- names(data.train)
cat("range feature recusive: "); print(features[xft.try.ind])
loop <- 0
acc.train.max <- 0
acc.test.max <- 0
svm.classifier.max <- NULL
acc.epsilon <- 0.1
while(TRUE) {
  loop <- loop + 1 
  step <- 0
  run.signal <- TRUE
  n.xft.choose <- length(xft.choose.ind)
  n.ft <- length(xft.try.ind)
  for(i in xft.try.ind) {
    ft.ind <- c(i, xft.choose.ind)
    print(ft.ind)
    # begin training model with tuning parameters for SVM
    c.list <-  c(1) # c(1,10,100,1000)
    sigma.list <-  c(0.001,0.01) # c(0.001,0.01,0.1)
    svm.classifier <- get.classifier.svm(data.train, data.test, ft.ind, acc.epsilon, c.list, sigma.list, "rbf", "C-svc")
    if(svm.classifier$train.acc > acc.train.max & svm.classifier$test.acc > acc.test.max){
      svm.classifier.max <- svm.classifier
      acc.train.max <- svm.classifier.max$train.acc
      acc.test.max <- svm.classifier.max$test.acc
      cat("current best classifier: "); print(svm.classifier)
    }
    # info features for debug
    step <- step + 1
    # actual.ft.name <- paste(features[ft.ind], collapse = ",")
    info <- sprintf("loop: %d, features: %s || step = %d/%d", loop, svm.classifier$features, step, n.ft)
    print(info)
  }
  #
  ft.ind.max <- svm.classifier.max$feature.index
  if(all(ft.ind.max != xft.choose.ind)){
    xft.try.ind <- xft.try.ind[-ft.ind.max]
    xft.choose.ind <- ft.ind.max
    line.write <- sprintf("c = %d, sigma = %f, train.acc = %f, test.acc = %f", svm.classifier.max$c, svm.classifier.max$sigma, svm.classifier.max$train.acc, svm.classifier.max$test.acc)
    print(line.write)
    write(line.write, file=svm.rs.log,append=TRUE) 
  } else {
    print("End of selecting feature process.")
    break
  }
  print("< =========================================================== >")
}
