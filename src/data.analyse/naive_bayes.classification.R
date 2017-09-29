# description
# naive bayes
library(caret)
source("src/utils/common.R")
get.classifier.naive_bayes <- function(data.train, data.test, ft.ind){
  get.info <- NULL
  if(ncol(data.train) != ncol(data.test)){
    info <- sprintf("ERROR number column of train and test set are different, ncol(data.train) = %d, ncol(data.test) = %d", ncol(data.train), ncol(data.test))
    print(info)
    quit(save = "default", status = 0, runLast = TRUE)
  } else {
    # data preparation
    data.train.x <- as.data.frame(data.train[, ft.ind])
    data.train.y <- convert.mask.factor(data.train$clazz)
    data.test.x <- as.data.frame(data.test[, ft.ind])
    data.test.y <-convert.mask.factor(data.test$clazz)
    # set name
    ft.name <-paste(colnames(data.train)[ft.ind], collapse = ", ")
    colnames(data.train.x) <- colnames(data.train)[ft.ind]
    colnames(data.test.x) <- colnames(data.train)[ft.ind]
    # train model
    model = train(data.train.x, data.train.y, method='nb', trControl=trainControl(method='cv',number=10))
    train.data.ypred <- predict(model$finalModel, data.train.x)$class
    test.data.ypred <- predict(model$finalModel, data.test.x)$class
    # acc
    train.acc <- c(sum(train.data.ypred==data.train.y)/length(data.train.y))
    test.acc <- c(sum(test.data.ypred==data.test.y)/length(data.test.y))
    # print info
    classifier.info <- sprintf("features: %s, train.acc = %f, test.acc = %f", ft.name, train.acc, test.acc)
    print(classifier.info)
    train.test.dist <- abs(train.acc - test.acc)
    get.info <- list(features = ft.name, feature.index = ft.ind, train.acc = train.acc, test.acc = test.acc, model = model,
                     mean.acc = mean(c(train.acc, test.acc)), hmean.acc = harmonic.mean(c(train.acc, test.acc)))
  }
  return (get.info)
}
