
library(doMC)
library(xgboost)
registerDoMC(cores = 8)

paste0("hi", "_", gsub("[ :]", "_", date()))
data <- filter(data, type == "train")
index <- createDataPartition(y = data$status_group, p = 0.8, list = F)

train <- data[index, ]
# train <- data2
train.id <- train$id
train.type <- train$type
train.date_recorded <- train$date_recorded
train$type <- NULL
train$id <- NULL
train$date_recorded <- NULL



test <- filter(data, type == "test")
test.id <- test$id
test.type <- test$type
test.date_recorded <- test$date_recorded
test$type <- NULL
test$id <- NULL
test$date_recorded <- NULL


ctrl <- trainControl(method = "cv", number = 9)

# gbm
model.gbm <- train(status_group ~ ., data = train, method = "gbm", trControl = ctrl, tuneLength = 10, verbose = F)

title <- paste0("gbm", "_", gsub("[ :]", "_", date()))
saveRDS(model.gbm, title)
saveRDS(data, paste("dataset_", title))

# rf
model.rf <- train(status_group ~ ., data = train, method = "rf", trControl = ctrl, tuneLength = 10)

title <- paste0("rf", "_", gsub("[ :]", "_", date()))
saveRDS(model.rf, title)
saveRDS(data, paste("dataset_", title))

# xgboost


data <- readRDS("data_sparse.RDS")

train.spare <- filter(data, type == 'train')
train.sparse.id <- train.sparse$id
train.sparse.type <- train.sparse$type
train.sparse$id <- NULL
train.sparse$type <- NULL

test.sparse <- filter(data, type == 'train')
test.sparse.id <- test.sparse$id
test.sparse.type <- test.sparse$type
test.sparse$id <- NULL
test.sparse$type <- NULL

train.xg <- train.sparse
train.xg.status_group <- factor(train.xg$status_group)
train.xg.y <- as.numeric(train.xg.status_group) - 1
train.xg$status_group <- NULL


for (i in 1:ncol(train.xg)) {
  if(is.factor(train.xg[, i])) {
    train.xg[, i] <- as.numeric(train.xg[, i])
  }
}
train.xg.matrix <- as.matrix(train.xg)


test.xg <- test
test.xg.status_group <- factor(test.xg$status_group)
test.xg.y <- as.numeric(test.xg.status_group) - 1
test.xg$status_group <- NULL


for (i in 1:ncol(test.xg)) {
  if(is.factor(test.xg[, i])) {
    test.xg[, i] <- as.numeric(test.xg[, i])
  }
}

test.xg.matrix <- as.matrix(test.xg)

num.class = 3

param <- list("objective" = "multi:softprob",    # multiclass classification 
               "num_class" = num.class,    # number of classes 
               "eval_metric" = "merror",    # evaluation metric 
               "nthread" = 8,   # number of threads to be used 
               "max_depth" = 6,    # maximum depth of tree 
               "eta" = 0.3,    # step size shrinkage 
               "gamma" = 0,    # minimum loss reduction 
               "subsample" = 1,    # part of data instances to grow tree 
               "colsample_bytree" = 1,  # subsample ratio of columns when constructing each tree 
               "min_child_weight" = 2  # minimum sum of instance weight needed in a child 
)


nround.cv = 600
bst_pre.cv <- xgb.cv(param=param, data=train.xg.matrix, label=train.xg.y, nfold=7, nrounds=nround.cv, prediction=TRUE, verbose=FALSE, maximize = TRUE)
min.merror.idx = which.min(bst_pre.cv$dt[, test.merror.mean]) 

model.xgb <- xgboost(param=param, data=train.xg.matrix, label=train.xg.y, nfold=7, nrounds = nround.cv, verbose = FALSE, prediction = TRUE, maximize = TRUE)

title <- paste0("xgb", "_", gsub("[ :]", "_", date()))
saveRDS(model.xgb, title)
saveRDS(data, paste("dataset_", title))
