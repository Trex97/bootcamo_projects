library(readxl)
library(caret)
library(tidyverse)
library(dplyr)
library(broom)
library(DAAG) ##For vif model 


#Import data 
data_survey<- read_excel("~/Desktop/Local governance- logistic/Data edit 15 02 2566.xlsx",sheet = "Sheet2")
View(data_survey)


df <- data_survey 
view(df)

#Create id for split data 
n <- nrow(data_survey)
set.seed(50)
id <- sample(1:n,n*0.7)
head(id)

#Build train data 
train_data <- df[id, ]
nrow(train_data)
#Build test data 
test_data <- df[-id, ]
nrow(test_data)

glimpse(df)
summary(df)

## build model with all variable 
logit_model  <- glm(outcome~.,data =df ,family = 'binomial')
summary(logit_model)

# Create data frame: Coef, P-value, Ood Confidence 95%
summary_model <-tidy(logit_model)
view(summary_model)

#Change P-value to 2 digit 
summary_model$p.value <- round(summary_model$p.value,2)

#Create Odds from coef
summary_model$odd <- exp(summary_model$estimate)

#Create output sig - not sig 
summary_model$sig <- if_else(summary_model$p.value < 0.05 ,"sig","not sig")

#Create confidence interval 95%  
tidy(confint(logit_model,level = 0.95))
confident95 <- round(confint(logit_model,level = 0.95),2)
summary_model <- summary_model %>% 
  mutate(confident95)

##Overall table summary model 
view(summary_model)

#For check AIC BIC 
glance(logit_model)

model <- lm(mpg ~ disp + hp + wt + drat, data = mtcars)
logit_model
vif(logit_model)




---------------------------------------------------------

predict(logit_model,data= df,type = 'response')
## take exp for check OR 
exp(coef(logit_model))
coef_model <- data.frame(logit_model$coefficients)

or_with_coef <- cbind(exp(coef(logit_model)),coef_model)
view(or_with_coef)
aa<-exp(cbind(OR = coef(logit_model), confint(logit_model)))
view(aa)
coefe <- data.frame(logit_model$coefficients)
total <- cbind(exp(coef(logit_model)),coefe)
view(total)
exp(coef(logit_model))

cbind(exp(coef(logit_model)),confint(logit_model, level = 0.95))
confint(logit_model, level = 0.95)

fit_model <- predict(logit_model, data = data_survey , type = 'response')
data_survey$predict <- if_else(fit_model > 0.5 ,1,0)
view(data_survey)
mean_error <- mean(data_survey$outcome==data_survey$predict)

glimpse(aa)
aa <- data_survey %>%
  mutate(outcome_ts = if_else(outcome=='No',0,1)) 


logit_model  <- glm(outcome_ts~.,data = aa,family = 'binomial')
summary(logit_model)








##
library(readxl)
df<- read_excel("~/Desktop/Local governance- logistic/Data edit 15 02 2566.xlsx",sheet = "Sheet2")
View(data_survey)
glimpse(df)

#split data 

split_data <- train_test_split(df)
train_data <- split_data[[1]]
test_data <- split_data[[2]]
nrow(train_data)
nrow(test_data)

glm_model <- train(outcome~.,
                   data = train_data,
                   method = "glm")
glm_model
varImp(glm_model)

p <- predict(glm_model,
             newdata = test_data)
RMSE(p,test_data$outcome)


