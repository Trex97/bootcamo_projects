#3. random_forest 

df <- PimaIndiansDiabetes
head(df)
glimpse(df)
df %>% 
  count(diabetes) %>%
  mutate(pct = n/sum(n))

mean(complete.cases(df))

##build model 
set.seed(42)
crtl <- trainControl(method = "cv",
                     number = 5 , 
                     verboseIter = T) 

rf_model <- train(diabetes~.,
                  data = train_data,
                  method = "rf",
                  metric = "Accuracy",
                  trControl = crtl)
p2 <- predict(rf_model, newdata = test_data)

mean(p2 == test_data$diabetes)

##build model 2 
set.seed(42)
ctrl <- trainControl(method = "cv",
                     number = 5 , 
                     verboseIter = T )
grid <- data.frame(mtry =c(3,5,7,8))

rf_model <-train(diabetes~.,
                 data = train_data,
                 method = "rf",
                 trControl = ctrl,
                 tuneGrid = grid)
rf_model












#4.rpart = dicition tree 

set.seed(42)
tree_model <- train(diabetes~.,
                  data = train_data,
                  method = "rpart",
                  metric = "Accuracy",
                  trControl = crtl)
p3 <- predict(rf_model, newdata = test_data)

mean(p3 == test_data$diabetes)

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



