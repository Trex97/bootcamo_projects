##ML2 

# install.packages("mlbench")  #for data set 
library(mlbench)

##ML101 
#K-nearest Neighbors
point1 <- c(2,3)
point2 <- c(6,8) 
sqrt(sum((point1-point2)**2))

#K-nearest Neighbors
#train KNN model 
library(mlbench)
library(caret)
library(dplyr)

data("BostonHousing")
view(BostonHousing)
# regression Problem 
mean(complete.cases(BostonHousing))  ##check NA value, missing value

#split data 
split_data <- train_test_split(BostonHousing, 0.8)
train_data <- split_data[[1]]
test_data <- split_data[[2]]
nrow(train_data)
nrow(test_data)

#train data
lm_model <- train(medv ~ crim + indus + rm,
                  data = train_data,
                  method = "lm")

lm_model
lm_model$finalModel

#train data
set.seed(99)
ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  verboseIter = TRUE
)

knn_model <- train(medv ~ crim + indus + rm,
                   data = train_data,
                   method = "knn",
                   trControl = ctrl)
plot(knn_model)
knn_model
knn_model$finalModel

# score 

p <- predict(knn_model,
             newdata = test_data)
#Evaluate 

error <- p-test_data$medv
test_rmse <- sqrt(mean(error^2))

RMSE(p,test_data$medv) ## package caret
#train error = 5.32
#test error = 5.54


# feature important 
varImp(knn_model)

#------------------------------------------------
##Try run all varriable 

knn_model <- train(
  medv~.,
  data = train_data,
  method ="knn",
  trControl = ctrl
)
knn_model 

plot(knn_model)

##score 
p <- predict(knn_model,
             newdata = test_data)
#evaluate 
error <- p- test_data$medv
test_rmse <- sqrt(mean(error^2))
# train error = 6.09
# test error = 6.56

# feature important 
varImp(knn_model)
#------------------------------------------------
#try keep top 5 varriable important 

knn_model <- train(
  medv~ nox + lstat +rm + indus + tax,
  data = train_data,
  method ="knn",
  trControl = ctrl
)
knn_model 

plot(knn_model)

##score 
p <- predict(knn_model,
             newdata = test_data)
#evaluate 
error <- p- test_data$medv
test_rmse <- sqrt(mean(error^2))
# train error = 6.09
# test error = 6.56

# feature important 
varImp(knn_model)

#save model and load 
saveRDS(knn_model,"knn_model.RDS")

model <- read_rds("knn_model.RDS")

#------------------------------------------------
#try chacge number sample [tunelength]
#train data
set.seed(99)
ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE
)


knn_model <- train(medv ~ nox + lstat +rm + indus + tax,
                   data = train_data,
                   method = "knn",
                   trControl = ctrl
                   ) ##
#grid search
grid <- data.frame( k =c(9,11,13))
knn_model <- train(medv ~ nox + lstat +rm + indus + tax,
                   data = train_data,
                   method = "knn",
                   trControl = ctrl,
                   tuneGrid = grid)
plot(knn_model)
knn_model
knn_model$finalModel

# score 

p <- predict(knn_model,
             newdata = test_data)
#Evaluate 

error <- p-test_data$medv
test_rmse <- sqrt(mean(error^2))

RMSE(p,test_data$medv) ## package caret
#train error = 5.32
#test error = 5.54


# feature important 
varImp(knn_model)


## build final model 
#choose k = 9
knn_model_final <- train(
  medv~nox+lstat+rm+indus+tax,
  data =train_data,
  method = "knn",
  tuneGrid = data.frame(k=9),
  trControl = trainControl(method = "none")
)

p<-predict(knn_model_final,
           newdata = test_data)
RMSE(p,test_data$medv)

#for choose group model [1.RMSE 2.RSQUARED 3.MAE]
set.seed(68)
train(medv~crim+indus+rm,
      data =train_data,
      method = "knn",
      metic = "Rsquared")

