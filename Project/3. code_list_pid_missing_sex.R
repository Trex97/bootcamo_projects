##################CODE FOR CHECK SEX - AGE miss IN TELEMEDICINE DATA ##########################
############################# 24 - 08 - 2023 ####################################################
######################### "TELEMEDICINE PROJECT" ##################################################


#create data.frame variable= pid,sex only
er <- data.frame(pid = tele$pid,sex = tele$sex)
er_unique <- unique(er)  

## dup = 71 patiens 
summary(er_unique) #count number of unique pid-sex = 110224
summary(duplicated(er_unique$pid)) # count number of unique pid  = 110153 
list_dup  <- data.frame((er_unique$pid)) #list pid 


#this part for check 110224 obs, how many duplicate
data <- er_unique 
data <- data %>%
  mutate(unique_id = group_indices(., pid))  #mutate unique_id 

#check = list pid-visit  if patient have sex more than 1 gender variable n != 1 
check <- data %>%
  group_by(unique_id) %>%
  count() 

#filter patient gender more than 1 in list 
list_patient_miss_gender  <- check %>%
  filter(n>1) 

#keep only variable unique_id  71 patient
list_patient_miss_gender  <- list_patient_miss_gender$unique_id 

# variable list_patient_miss_gender join data for filter all observation.
data <- data %>%
  mutate(list_miss_gender = case_when(
    unique_id  %in%  list_patient_miss_gender ~ 1,
    TRUE ~ 0
  ))
# keep only list_miss_gender ==1 
list_sex_missing <- data %>%
  filter(list_miss_gender == 1)  

#count for check patient have sex problem.
n <-list_sex_missing %>%
  group_by(unique_id) %>%
  count()

# list pid 
list_pid_missing_sex <- list_sex_missing$pid 

#### this code for export list patien have problem. #####
tele <- tele %>%
  mutate(missing_sex = case_when(
    pid %in% list_pid_missing_sex ~ 1,
    TRUE ~ 0
  )) #gen variable missing_sex = if missing_sex =1 is pid have sex problem.

# tele %>%
#   group_by(missing_sex) %>%
#   count() # missing_sex = 498 n
# 
# list_pid <- tele %>%
#   filter(missing_sex == 1)  # keep missing_sex =1 only
# #write_csv(list_pid,"list_pid_sex_missing.csv")
# 
# table <- list_pid %>%
#   group_by(pid,unique_id,sex) %>%
#   count(sex)
#write_csv(table,"table_count_sex_missing.csv")
###########################################################


# revise sex miss  --------------------------------------------------------
# count pid have only patient have equal sex 

# count data miss sex 
data_all_sex_miss <- tele %>%
  group_by(missing_sex) %>%
  count() # missing_sex = 498 n

# keep missing_sex =1 only
data_all_sex_miss <- tele %>%
  filter(missing_sex == 1) 

#count each sex from unique patient.
data_all_sex_miss <- data_all_sex_miss %>%
  group_by(pid,unique_id,sex) %>%
  count()

#create variable for check who patien has count sex equal. example sex 1 = 2 , sex2 = 2
data_all_sex_miss <- data_all_sex_miss %>%
  group_by(unique_id) %>%
  mutate(check = ifelse(all(n == first(n)), 1, 0)) %>%
  ungroup()

data_all_sex_miss %>%
  count(check)

#create variable for keep unique 
data_all_sex_miss <- data_all_sex_miss %>%
  mutate(unique_var = ifelse(duplicated(pid), 0, 1)) 

##filter for create list patient count sex equal 
data_all_sex_miss <- data_all_sex_miss %>%
  filter(check == 1 & unique_var == 1)
list_pid_sex_equal <- data_all_sex_miss$unique_id

#------------------------------------------------------------------------------
#revise sex1 = revise pid have equal sex 

#main data set for join 
revise_sex <- tele %>% 
  filter(missing_sex == 1 ) %>%
  select(pid,unique_id,unique_var,new_date, sex , missing_sex) %>%
  arrange(unique_id, new_date)

#revise_sex1
revise_sex1 <- tele %>% 
  filter(missing_sex == 1 ) %>%
  select(pid,unique_id,unique_var,new_date, sex , missing_sex) %>%
  arrange(unique_id, new_date)

#query list patien have equale sex 
revise_sex1 <- revise_sex1 %>%
  filter(unique_id %in% list_pid_sex_equal)

#replace sex = most recent sex visit 
revise_sex1 <- revise_sex1 %>%
  group_by(pid) %>%
  mutate(new_sex = sex[new_date == max(new_date)]) %>%
  ungroup()

# revise table for join 
# change to unique list and have 2 variable 1. unique_id (primary key) 2. new_sex (value)
revise_sex1 <- revise_sex1 %>%
  mutate(unique_var = ifelse(duplicated(pid), 0, 1)) %>%
  filter(unique_var == 1 ) %>%
  select(unique_id,new_sex)

##test 
# test <- left_join(revise_sex,revise_sex1, by = "unique_id")
# test <- test %>%
#   group_by(unique_id)
# ##
#------------------------------------------------------------------------------


#revise_sex2 = patient have visit more than 2 

#main
revise_sex <- tele %>% 
  filter(missing_sex == 1 ) %>%
  select(pid,unique_id,unique_var,new_date, sex , missing_sex) %>%
  arrange(unique_id, new_date)

revise_sex2 <- tele %>% 
  filter(missing_sex == 1 ) %>%
  select(pid,unique_id,unique_var,new_date, sex , missing_sex) %>%
  arrange(unique_id, new_date)

#create variable for cut (pid from revise_sex1) if 1  = have sex equal , 0 = more than 2 visit 
revise_sex2 <- revise_sex2 %>%
  mutate(cut_obs =case_when(
    revise_sex2$unique_id %in% revise_sex1$unique_id ~ 1,
    TRUE ~ 0
  ))

## cut obs out 22 obs 
##498 - 22 = 476 (case 2 visit only)
## 498 - ... = 450 
revise_sex2 <- revise_sex2 %>% 
  filter(cut_obs != 1)

revise_sex2 <- revise_sex2 %>%
  group_by(pid) %>%
  mutate(new_sex = names(which.max(table(sex)))) %>%
  ungroup()

revise_sex2 <- revise_sex2 %>%
  filter(unique_var == 1 ) %>%
  select(unique_id,new_sex)

## combine revise_sex1, revise_sex2  for create data table for join.
revise_sex3  <- rbind(revise_sex1,revise_sex2)

test <- left_join(revise_sex,revise_sex3, by = "unique_id")
test <- test %>%
  mutate(for_check = if_else(sex == new_sex,0,1))

##create tele2 for clean variable sex 
tele2 <- tele 
tele2 <- left_join(tele2,revise_sex3, by = "unique_id")

#check sex original 
tele2 %>%
  group_by(sex) %>%
  count()
#check new_sex
tele2 %>%
  group_by(new_sex) %>%
  count()

#create new_sex2 for combind sex=new_sex
tele2$new_sex<- as.numeric(tele2$new_sex)
tele2 <- tele2 %>%
  group_by(unique_id) %>%
  mutate(new_sex2 = ifelse(all(!is.na(new_sex)), first(new_sex), first(sex)))


#summary(unique(tele2$pid,tele2$new_sex2))
check  <- tele2 %>%
  group_by(unique_id,new_sex2) %>%
  count()
#   Length     Class      Mode 
#110153 character character 

check2 <-  tele2%>%
  group_by(missing_sex) %>%
  count() 

check2 <- tele2 %>%
  filter(missing_sex ==1) %>%
  select(unique_id,sex, new_sex2, missing_sex)






# Part age miss  --------------------------------------------------------

#select variable 
revise_age <- tele %>%
  select(pid,unique_id,unique_var,age,new_date,year) %>%
  arrange(unique_id, new_date)

# gen age_diff = age - age(next row) by pid  #259047 obs
revise_age <- revise_age %>%
  group_by(pid) %>%
  mutate(age_diff = as.numeric(age - lag(age, default = first(age)))) %>%
  ungroup()



# check age_diff
revise_age %>%
  group_by(unique_id) %>%
  count(age_diff)

do <-revise_age %>%
  group_by(unique_id, age_diff) %>%
  count()

# keep pid if age_diff is not 0 and 1 
list_pid_age_miss <- revise_age %>%
  filter(age_diff != c(0,1))

write.csv(revise_age,"list_age_miss.csv")  

# this command for list pid or unique_id to fine in {revise_age}
#1. look at age_diff value more than 1 
do <- revise_age %>%
  filter(age_diff >=1) %>%
  group_by(unique_id,age_diff) %>%
  count()

list <- do$unique_id

length(list)

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






do <- revise_age %>%
  filter(age_diff==3)

list <- do$unique_id


do <- revise_age %>%
  filter(unique_id %in% list) 

