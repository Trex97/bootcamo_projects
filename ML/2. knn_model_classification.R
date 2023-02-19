
##Classification 

data("PimaIndiansDiabetes")


df <- PimaIndiansDiabetes
head(df)
glimpse(df)
df %>% 
  count(diabetes) %>%
  mutate(pct = n/sum(n))

mean(complete.cases(df))

#build model 

split_data <- train_test_split(df)
train_data <- split_data[[1]]
test_data <- split_data[[2]]
nrow(train_data)
nrow(test_data)

set.seed(42)
crtl <- trainControl(method = "cv",
                     number = 5 , 
                     verboseIter = T) 

model <- train(diabetes~.,
               data = train_data,
               method = "knn",
               metric = "Accuaracy",
               trControl = ctrl)
model
p<- predict(model,newdata = test_data)
mean(p== test_data$diabetes)


