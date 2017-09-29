# description
# model.type="C-svc"
# kernel = "rbf"
get.classifier.svm <- function(data.train, data.test, ft.ind, c.list, sigma.list, kernel, model.type){
  library(kernlab)
  get.info <- NULL
  if(ncol(data.train) != ncol(data.test)){
    info <- sprintf("ERROR number column of train and test set are different, ncol(data.train) = %d, ncol(data.test) = %d", ncol(data.train), ncol(data.test))
    print(info)
    quit(save = "default", status = 0, runLast = TRUE)
  } else {
    # data preparation
    data.train.x <- data.matrix(data.train[, ft.ind])
    data.train.y <- data.train$clazz
    data.test.x <- data.matrix(data.test[, ft.ind])
    data.test.y <- data.test$clazz
    ft.name <-paste(colnames(data.train)[ft.ind], collapse = ", ")
    max.acc.train <- 0
    max.acc.test <- 0
    for(i_c in c.list){
      for(i_sig in sigma.list){
        svp <- ksvm(data.train.x, data.train.y, type=model.type, kernel=kernel, kpar=list(sigma=i_sig), C=i_c)
        # predict labels on test
        train.data.ypred = predict(svp, data.train.x)
        test.data.ypred = predict(svp, data.test.x)
        # compute accuracy
        # train.rain.acc <- c(sum(train.data.ypred[which(data.train.y==1)] == 1)/(sum(train.data.ypred==1)))
        # train.dry.acc <- c(sum(train.data.ypred[which(data.train.y==-1)]==-1)/(sum(train.data.ypred==-1)))
        train.acc <- c(sum(train.data.ypred==data.train.y)/length(data.train.y))
        # test.rain.acc <- c(sum(test.data.ypred[which(data.test.y==1)] == 1)/(sum(test.data.ypred==1)))
        # test.dry.acc <- c(sum(test.data.ypred[which(data.test.y==-1)]==-1)/(sum(test.data.ypred==-1)))
        test.acc <- c(sum(test.data.ypred==data.test.y)/length(data.test.y))
        classifier.info <- sprintf("features: %s, c = %d, sigma = %f, train.acc = %f, test.acc = %f", ft.name, i_c, i_sig, train.acc, test.acc)
        print(classifier.info)
        train.test.dist <- abs(train.acc - test.acc)
        get.info <- list(features = ft.name, feature.index = ft.ind, c = i_c, sigma = i_sig, train.acc = train.acc, test.acc = test.acc, model = svp)
      }
    }
  }
  return (get.info)
}