set.seed(32)
# load the library
library(mlbench)
library(caret)
require(e1071)
#
data.rainfall <- read.csv("data/tuning/rainfall_tuning.csv", header=TRUE)
# drop.ft <- c("date", "rainfall")
df.feature <- data.rainfall[,-c(1, 35)]
df.feature$clazz <- as.factor(df.feature$clazz)

# feture selection using Learning Vector Quantization
# prepare training scheme
lvqcontrol <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(clazz~., data=df.feature, method="lvq", preProcess="scale", trControl=lvqcontrol)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

# feature selection using Recursive Feature Elimination 
# define the control using a random forest selection function
rcontrol <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(df.feature[,1:33], df.feature[,34], sizes=c(1:33), rfeControl=rcontrol)
# summarize the results
print(results)
# list the chosen features
predictors(results)


