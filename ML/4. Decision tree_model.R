#4.rpart = decision tree 

set.seed(42)
tree_model <- train(diabetes~.,
                    data = train_data,
                    method = "rpart",
                    metric = "Accuracy",
                    trControl = crtl)
p3 <- predict(rf_model, newdata = test_data)

mean(p3 == test_data$diabetes)

#---------------------------------------
#Decision tree can use with regression and classification 
#install.packages("rpart.plot")
library(rpart.plot)

decisiontree <- train(diabetes~.,
      data = train_data,
      method = "rpart")

rpart.plot(decitiontree$finalModel)
varImp(decisiontree)
