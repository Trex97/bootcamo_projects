###################CODE FOR CHECK SEX - AGE miss IN TELEMEDICINE DATA ##########################
############################# 16 - 06- 2023 ####################################################
######################### "TELEMEDICINE PROJECT" ##################################################


#create data.frame variable= pid,sex only
er <- data.frame(pid = tele$pid,sex = tele$sex)
er_unique <- unique(er)  

## dup = 71 patiens 
summary(er_unique) #count number of unique pid-sex = 110224
summary(duplicated(er_unique$pid)) # count number of unique pid  = 110153 
list_dup  <- data.frame((er_unique$pid)) #list pid 



data <- er_unique 
data <- data %>%
  mutate(unique_id = group_indices(., pid))  #mutate unique_id 


check <- data %>%
  group_by(unique_id) %>%
  count() #check = list pid-visit  if patient have sex more than 1 gender variable n != 1 

list_patient_miss_gender  <- check %>%
  filter(n>1) #filter patient gender more than 1 in list 

list_patient_miss_gender  <- list_patient_miss_gender$unique_id #keep onliy variable unique_id  71 patient 

data <- data %>%
  mutate(aa = case_when(
    unique_id  %in%  list_patient_miss_gender ~ 1,
    TRUE ~ 0
  ))  # variable list_patient_miss_gender join data for filter all observation 

list_sex_missing <- data %>%
  filter(aa == 1)  # keep only aa ==1 

class(list_sex_missing$pid) #check class


list_pid_missing_sex <- list_sex_missing$pid # list pid 


tele <- tele %>% 
  mutate(missing_sex = case_when(
    pid %in% list_pid_missing_sex ~ 1,
    TRUE ~ 0
  )) #gen variable missing_sex = if missing_sex =1 is pid have sex problem.  

tele %>%
  group_by(missing_sex) %>%
  count() # missing_sex = 498 n 

list_pid <- tele %>%
  filter(missing_sex == 1)  # keep missing_sex =1 only 
  
#write_csv(list_pid,"list_pid_sex_missing.csv")




table <- list_pid %>%
  group_by(pid) %>%
  count(sex)
#write_csv(table,"table_count_sex_missing.csv")





# revise sex miss  --------------------------------------------------------

revise_sex <- tele %>% 
  filter(missing_sex == 1 ) %>%
  select(pid,unique_id,unique_var, sex ,gender , missing_sex) %>%
  arrange(unique_id)

tele <- revise_sex %>%
  group_by(pid) %>%
  mutate(new_sex = names(which.max(table(sex)))) %>%
  ungroup()


# revise age miss  --------------------------------------------------------

#select variable 
revise_age <- tele %>%
  select(pid,unique_id,unique_var,age,new_date,year) %>%
  arrange(unique_id,new_date)


# gen age_diff = age - age(next row) by pid 
revise_age <- revise_age %>%
  group_by(pid) %>%
  mutate(age_diff = as.numeric(age - lag(age, default = first(age)))) %>%
  ungroup()
  

# check age_diff
revise_age %>%
  group_by(unique_id) %>%
  count(age_diff)

# this command for list pid or unique_id to fine in {revise_age}
#1. look at age_diff value more than 1 
do <- revise_age %>%
  filter(age_diff >= 1) %>%
  group_by(unique_id,age_diff) %>%
  count()

list <- do$unique_id

do <- revise_age %>%
  filter(unique_id %in% list) 


#2. look at age_diff all value 

do <- revise_age %>%
  group_by(age_diff) %>%
  count()
do <- revise_age %>%
  group_by(unique_id,age_diff) %>%
  count()

list <- do$unique_id

do <- revise_age %>%
  filter(unique_id %in% list)  %>%
  arrange(unique_id,new_date)


#  look at age_diff value less than 1 
do <- revise_age %>%
  filter(age_diff < 0 ) %>%
  group_by(unique_id,age_diff) %>%
  count()

list <- do$unique_id

do <- revise_age %>%
  filter(unique_id %in% list) 
  

install.packages("gt")
library(gt)
tibble(tele)





do <- revise_age %>%
  filter(age_diff==3)

list <- do$unique_id


do <- revise_age %>%
  filter(unique_id %in% list) 


