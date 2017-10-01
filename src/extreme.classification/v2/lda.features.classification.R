# load library
source("src/data.analyse/lda.classification.R")
source("src/utils/common.R")
source("src/utils/file.utils.R")
# load data
data.train <- read.csv("data/tuning/extreme/data.train.extreme.csv", header=TRUE)
data.test <- read.csv("data/tuning/extreme/data.test.extreme.csv", header=TRUE)
lda.rs.log <- "result/extreme/lda.wet.dry.classification.ft.log"
acc.filters <- c("train", "test", "and", "or", "mean", "harmonic-mean")
for(filter in acc.filters) {
  # feature selection
  xft.try.ind <- c(2:(ncol(data.train) -2))
  xft.choose.ind <- c()
  features <- names(data.train)
  cat("range feature recusive: "); print(features[xft.try.ind])
  loop <- 0
  acc.train.max <- 0
  acc.test.max <- 0
  acc.mean.max <- 0
  acc.hmean.max <- 0
  lda.classifier.max <- NULL
  print(filter)
  file.safewrite(filter, file=lda.rs.log, is.append=TRUE) 
  while(TRUE) {
    loop <- loop + 1 
    step <- 0
    run.signal <- TRUE
    n.xft.choose <- length(xft.choose.ind)
    n.ft <- length(xft.try.ind)
    for(i in xft.try.ind) {
      ft.ind <- c(i, xft.choose.ind)
      # begin training model with tuning parameters for lda
      lda.classifier <- get.classifier.lda(data.train, data.test, ft.ind)
      if(filter == "train"){
        if(lda.classifier$train.acc >= acc.train.max){
          lda.classifier.max <- lda.classifier
          print("current best classifier: "); print(lda.classifier)
        }
      } else if(filter == "test"){
        if(lda.classifier$test.acc >= acc.test.max){
          lda.classifier.max <- lda.classifier
          print("current best classifier: "); print(lda.classifier)
        }
      } else if(filter == "and"){
        if(lda.classifier$test.acc >= acc.test.max & lda.classifier$train.acc >= acc.train.max){
          lda.classifier.max <- lda.classifier
          print("current best classifier: "); print(lda.classifier)
        }
      } else if(filter == "or"){
        if(lda.classifier$test.acc >= acc.test.max | lda.classifier$train.acc >= acc.train.max){
          lda.classifier.max <- lda.classifier
          print("current best classifier: "); print(lda.classifier)
        }
      } else if(filter == "mean"){
        if(lda.classifier$mean.acc >= acc.mean.max){
          lda.classifier.max <- lda.classifier
          acc.mean.max <- lda.classifier.max$mean.acc
          print("current best classifier: "); print(lda.classifier)
        }
      } else if(filter == "harmonic-mean"){
        if(lda.classifier$hmean.acc >= acc.hmean.max){
          lda.classifier.max <- lda.classifier
          acc.hmean.max <- lda.classifier.max$hmean.acc
          print("current best classifier: "); print(lda.classifier)
        }
      }
      # setiing acc to current model
      acc.train.max <- lda.classifier.max$train.acc
      acc.test.max <- lda.classifier.max$test.acc
      acc.mean.max <- lda.classifier.max$mean.acc
      acc.hmean.max <- lda.classifier.max$hmean.acc
      # info features for debug
      step <- step + 1
      # actual.ft.name <- paste(features[ft.ind], collapse = ",")
      info <- sprintf("loop: %d, features: %s || step = %d/%d", loop, lda.classifier$features, step, n.ft)
      print(info)
    }
    #
    ft.ind.max <- lda.classifier.max$feature.index
    cat("[ft.ind.max]: "); print(ft.ind.max)
    cat("[xft.choose.ind]: "); print(xft.choose.ind)
    diff.ind <- find.diff(ft.ind.max, xft.choose.ind)
    cat("[diff.ind]: "); print(diff.ind)
    if(length(diff.ind) == 1){
      xft.try.ind <- xft.try.ind[-diff.ind]
      xft.choose.ind <- ft.ind.max
      line.write <- sprintf("features = %s, train.acc = %f, test.acc = %f, train.rain.acc = %f, train.dry.acc = %f, test.rain.acc = %f, test.dry.acc = %f"
                            , lda.classifier.max$features, lda.classifier.max$train.acc, lda.classifier.max$test.acc,
                            lda.classifier.max$train.rain.acc, lda.classifier.max$train.dry.acc, lda.classifier.max$test.rain.acc, lda.classifier.max$test.dry.acc)
      print(line.write)
      file.safewrite(line.write, file=lda.rs.log, is.append=TRUE) 
    } else {
      print("End of selecting feature process.")
      break
    }
    print("< =========================================================== >")
  }
}
