#5, logistic regression

set.seed(42)
logit_model <- train(diabetes~.,
                     data = train_data,
                     method = "glm",
                     metric = "Accuracy",
                     trControl = crtl)
p4 <- predict(rf_model, newdata = test_data)

mean(p4 == test_data$diabetes)


## evaluate models 
#for check model is matter
list_models <- list(
  knn = model,
  logistic = logit_model,
  tree = tree_model,
  randomForst = rf_model
)

result <- resamples(list_models)
summary(result)
coefficients(logit_model$finalModel)


##predict diabetes 

set.seed(42)
ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = T)
logistic_model <- train(diabetes ~ .,
                        data = train_data,
                        method = "glm" ,
                        metric = "Accuracy",
                        trControl = ctrl)

# evaluate model 
p<- predict(logit_model,newdata = test_data)
confusionMatrix(p,test_data$diabetes,
                positive = "pos",
                mode = "prec_recall")






