library(titanic)

head(titanic_train)

#drop NA 
df <- na.omit(titanic_train)
nrow(titanic_train)
nrow(df)

#sprit data 
set.seed(50)
n <- nrow(df)
id <- sample(1:n,n*0.7)
head(id)
# Bulid data for train_model 
train_data <- df[id, ]
# Bulid data for test_model 
test_data <- df[-id, ]
#check n in data 
nrow(train_data)
nrow(test_data)

tibble(df)
head(df)
library(dplyr)

## varriable sex to factor 
df$sex_ <-df %>%
  factor(df$Sex ,
         levels = c('male','female'),
         labels = c(0,1))
head(df)

??titanic_train

## Build model Logistic regression Survived and Pclass 
logit_model <- glm(Survived~Pclass , data=train_data ,family = 'binomial' )
summary(logit_model)

## train Model 
train_model <- predict(logit_model,data = train_data,type = 'response')

train_data$train_model <- train_model
train_data$predic <- if_else(train_model >= 0.5,1,0)
head(train_data)

train_data$Survived == train_data$predic

mean_error_train <- mean(train_data$Survived == train_data$predic)

## test model 

test_model <- predict(logit_model,newdata = test_data,type = 'response')
test_data$predic <- if_else(test_model >= 0.5,1,0)

head(test_data)

test_data$Survived==test_data$predic

mean_error_test <- mean(test_data$Survived==test_data$predic)

##check error between train_model and test_model 
mean_error_train 
mean_error_test

## run on all data 

df_model  <- predict(logit_model,newdata =df , type =  'response')

df$predic <- if_else(df_model >= 0.5 ,1,0)

mean(df$Survived==df$predic)

#confussion matrix 

con_m <- table(df$Survived,df$predic , dnn = c("survived","predic"))

##model Evaluation 
##Acuracy 
cat("Accuracy:",(con_m[1,1]+con_m[2,2])/sum(con_m))
cat("Precition:",con_m[2,2]/sum(con_m[2, ]))
cat("recall:", con_m[2,2]/sum(con_m[ ,2]))
cat("F1 score:", 2*((0.42*0.66)/(0.42+0.66)))
#---------------------------------------------------
.
.
.
#----------------Model more than 1 Variable ------------------
library(dplyr)
## Logistic regression 

tibble(titanic_train)
## remove na 
df <- na.omit(titanic_train)
nrow(titanic_train)
nrow(df)

df$sex_ <- df %>%
  factor(x = df$Sex ,
         levels = c('male','female'),
         labels = c(0,1))

head(df)
## Bulid train data and test data 

n <- nrow(df)

set.seed(50)
id <- sample(1:n,n*0.7)
length(id)

train_data <- df[id, ]
tibble(train_data)
test_data <- df[-id , ]
tibble(test_data)

## Bulid model 

head(df)

data_extrat <- df%>% 
  select(PassengerId,Survived,Pclass,sex_,Age,Fare)

logit_model <- glm(Survived~Pclass+Age+sex_ , 
                   data = test_data ,
                   family = 'binomial')

summary(logit_model)

## train_model 
train_model <- predict(logit_model,data = train_data, type ='response')

train_data$predic <- if_else(train_model >= 0.5,1,0)
head(train_data)
(mean(train_data$Survived==train_data$predic))

## test_model 
test_model <- predict(logit_model,newdata = test_data,type = 'response')
head(test_model)

test_data$predic <- if_else(test_model >= 0.5,1,0)
head(test_data)

(mean(test_data$Survived==test_data$predic))

## run model on data 

df_model <- predict(logit_model,newdata = df,type = 'response')

df$predic <- if_else(df_model >= 0.5,1,0)
head(df)

(mean(df$Survived==df$predic))
