# read and combine features as a data frame, dataset was collected from 01/01/1964 to 31/12/2013
require(gdata)
data = read.xls("data/original_data/rainfall_v2_1964_2013.xlsx", sheet = 1, header = TRUE)
# View(rainfall)
rows <- nrow(data)
cols <- ncol(data)
clazz <- character(0)
for(i in c(1:rows)){
  precipitation <- data[i, cols]
    if(precipitation > 0){
      clazz <- append(clazz, 1) # rain_day: 1
    } else {
      clazz <- append(clazz, -1) # dry_day: -1
    }
}
data.tuning <- data.frame(data, clazz)
write.table(data.tuning, file="data/tuning/rainfall_tuning.csv", row.names=FALSE, sep=",", quote=FALSE)
### divide train (from: 1964, to: 1999) and test data (from: 1999, to: 2013)
# train -> c(1, 13149)
data.train <- data.tuning[c(1:13149), ]
write.table(data.train, file="data/tuning/rainfall_train.csv", row.names=FALSE, sep=",", quote=FALSE)
# test -> c(13150, 17898)
data.test <- data.tuning[-c(1:13149), ]
write.table(data.test, file="data/tuning/rainfall_test.csv", row.names=FALSE, sep=",", quote=FALSE)
