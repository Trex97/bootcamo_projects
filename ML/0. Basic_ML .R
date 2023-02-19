
#install.packages("caret")
## must have library "caret"
library(caret)
library(dplyr)

head(mtcars)

##statistic
model1 <- lm(mpg~hp+wt,data =  mtcars)
model1 


## caret package train()
model2 <- train(mpg~hp+wt, data = mtcars , method = "lm")

#show full model 
model2$finalModel 

## ML Basic pipline 

# 1. prepare/split data 
sample(1:6,size = 1)

# can replace value 
sample(1:6,size = 10 , replace = T)

set.seed(48)
n <- nrow(mtcars)
id <- sample(1:n , size = n*0.8)
train_data <- mtcars[id, ]
test_data <- mtcars[-id, ]

nrow(train_data)
nrow(test_data)

# 2. train data 
model <- train(mpg~hp+wt,
               data = train_data,
               method = "lm")
model
model$finalModel
# 3. score model / prediction 
p_mpg <- predict(model , newdata = test_data)

# 4. evaluate model 

error <- p_mpg - test_data$mpg
test_rmse <- sqrt(mean(error^2))

test_rmse

#-----------------------------------------------------------
## revise method  defult = bootstrap

# 2. train data 
#ctrl = control
ctrl <- trainControl(method = "boot" , number = 100)

model <- train(mpg~hp+wt,
               data = train_data,
               method = "lm",
               trControl = ctrl)
model
model$finalModel

## revise method = "LOOCV"

ctrl <- trainControl(method = "LOOCV")

model <- train(mpg~hp+wt,
               data = train_data,
               method = "lm",
               trControl = ctrl)
model
model$finalModel
## cv = k-fold cCross validation.
## revise method = "cv"
set.seed(25)
ctrl <- trainControl(method = "cv",
                     number = 5,  # k = group = 5 
                     verboseIter = TRUE)

model <- train(mpg~hp+wt,
               data = train_data,
               method = "lm",
               trControl = ctrl)
model
model$finalModel

## if split group = 10 
set.seed(25)
ctrl <- trainControl(method = "cv",
                     number = 10,  # k = group = 5 
                     verboseIter = TRUE)

model <- train(mpg~hp+wt,
               data = train_data,
               method = "lm",
               trControl = ctrl)
model
model$finalModel


#-----------------------------------------------------------
# 3. score model / prediction 
p_mpg <- predict(model , newdata = test_data)

#-----------------------------------------------------------
# 4. evaluate model 

error <- p_mpg - test_data$mpg
test_rmse <- sqrt(mean(error^2))

test_rmse
#-----------------------------------------------------------

## **super golden test**

set.seed(42)
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 5,# k = group = 5 
                     verboseIter = TRUE)

model <- train(mpg~hp+wt,
               data = train_data,
               method = "lm",
               trControl = ctrl)
model
model$finalModel

#-----------------------------------------------------------
# 3. score model / prediction 
p_mpg <- predict(model , newdata = test_data)

#-----------------------------------------------------------
# 4. evaluate model 

error <- p_mpg - test_data$mpg
test_rmse <- sqrt(mean(error^2))

test_rmse
#-----------------------------------------------------------
# 5 save model (batch prediction)
saveRDS(model,"LinearReg_model.RDS")

nov_data <- data.frame(
  id= 1:3,
  hp = c(200,150,188),
  wt = c(2.5,1.9,3.2)
)

nov_predic <- predict(model, newdata =nov_data)
nov_predic
# Load model 
model <- readRDS("LinearReg_Model.RDS")

## Create a new dataset with prediction 
nov_data$pred <- nov_predic

write_csv(nov_data,"resultNov.csv")
#-------------------------------------------------------
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
  number = 5,
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
                   trControl = ctrl,
                   tuneLength = 7) ##
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

