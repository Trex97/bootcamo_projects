# 6.Lasso model 

##predict diabetes 
##glmnet model: lasso,ridge 

set.seed(42)
ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = T)
grid <- expand.grid(alpha=c(0,1),
                    lambda = seq(0,1,by = 0.005))

#alpha = 0# ridge
#alpha = 1# Lasso

glmnet_model <- train(diabetes ~ .,
                        data = train_data,
                        method = "glmnet" ,
                        metric = "Accuracy",
                        trControl = ctrl,
                        tuneGrid = grid )

# evaluate model 
p<- predict(logit_model,newdata = test_data)
confusionMatrix(p,test_data$diabetes,
                positive = "pos",
                mode = "prec_recall")
