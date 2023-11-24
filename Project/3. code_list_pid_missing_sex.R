##################CODE FOR CHECK SEX - AGE miss IN TELEMEDICINE DATA ##########################
############################# 01 - 09 - 2023 ####################################################
######################### "TELEMEDICINE PROJECT" ##################################################


#create data.frame variable= pid,sex only
er <- data.frame(pid = tele$pid,sex = tele$sex)
er_unique <- unique(er)  

## dup = 71 patiens 
summary(er_unique) #count number of unique pid-sex = 110224
# pid                 sex       
# Length:110224      Min.   :1.000  
# Class :character   1st Qu.:1.000  
# Mode  :character   Median :2.000  
# Mean   :1.573  
# 3rd Qu.:2.000  
# Max.   :2.000  
summary(duplicated(er_unique$pid)) # count number of unique pid  = 110153 
# Mode   FALSE    TRUE 
# logical  110153      71
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

tele %>%
  group_by(missing_sex) %>%
  count() # missing_sex = 498 n
#   missing_sex      n
# 1           0 258549
# 2           1    498
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
#revise sex1 = revise pid can't use function mode (sex equal)

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

revise_sex2 %>%
  group_by(cut_obs) %>%
  count()
# cut_obs     n
# <dbl>    <int>
#  0        450
#  1         48

## cut obs out 48 obs 
## 498 - 22 = 476 (case sex equal visit ) (not current code)
## 498 - 48 = 450 (case sex equal visit ) (current code)
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
  mutate(new_sex2 = ifelse(all(!is.na(new_sex)), (new_sex), (sex)))


#summary(unique(tele2$pid,tele2$new_sex2))
check  <- tele2 %>%
  group_by(unique_id,new_sex2) %>%
  count()
#   Length     Class      Mode 
#110153 character character 

tele2%>%
  group_by(new_sex2) %>%
  count() 
# new_sex2      n
# 1        1 122598
# 2        2 136449

tele2%>%
  group_by(missing_sex) %>%
  count() 

check2 <- tele2 %>%
  filter(missing_sex ==1) %>%
  select(unique_id,sex, new_sex2, missing_sex)





# Part age miss  --------------------------------------------------------

#select variable 
# ***MAIN***
revise_age <- tele %>%
  select(pid,unique_id,tran_id,unique_var,age,new_date,year) %>%
  arrange(unique_id, new_date)

# gen age_diff = age - age(next row) by pid  #259047 obs
revise_age <- revise_age %>%
  group_by(pid) %>%
  mutate(age_diff = as.numeric(age - lag(age, default = first(age)))) %>%
  ungroup()

do <- revise_age %>%
  group_by(age_diff) %>%
  count()


# check age_diff
revise_age %>%
  group_by(unique_id) %>%
  count(age_diff)

do <-revise_age %>%
  group_by(unique_id, age_diff) %>%
  count()

do <- do %>%
  group_by(unique_id,age_diff) %>%
  filter(age_diff >2) %>%
  count()

#------------------------------------------------------------------
# # keep pid if age_diff is not 0 and 1 
# list_pid_age_miss <- revise_age %>%
#   filter(age_diff != c(0,1))
# write.csv(revise_age,"list_age_miss.csv")  

# this command for list pid or unique_id 

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


#3. look at age_diff value less than 0
do <- revise_age %>%
  filter(age_diff < 0 ) %>%
  group_by(unique_id,tran_id,age_diff,new_date) %>%
  count()
list <- do$unique_id
do <- revise_age %>%
  filter(unique_id %in% list) 


#------------------------------------------------------------------

# Part 1 
do <- revise_age %>%
  filter(age_diff < 0 ) %>%
  group_by(unique_id,tran_id,age_diff,new_date) %>%
  count()

list_agediff_less_0 <- do$unique_id

revise_age1 <- revise_age %>%
  filter(revise_age$unique_id %in% list_agediff_less_0)

revise_age1 <- revise_age1 %>%
  group_by(pid) %>%
  mutate(year_diff = as.numeric(year - lag(year, default = first(year)))) %>%
  ungroup()

# step1
#------------------------------------------------------------------------------------------
revise_age1 <- revise_age1 %>%
  group_by(pid,year) %>%
  mutate(new_age2 = as.numeric(names(which.max(table(age))))) %>%
  ungroup()

revise_age1 <- revise_age1 %>%
  group_by(pid) %>%
  mutate(age_diff2 = as.numeric(new_age2 - lag(new_age2, default = first(new_age2)))) %>%
  ungroup()

revise_age1 <- revise_age1 %>%
  select(unique_id,tran_id,unique_var,new_date,year,year_diff,age,age_diff,new_age2,age_diff2)

revise_age1 %>%
  group_by(age_diff2) %>%
  count()
#------------------------------------------------------------------------------------------
# do <- revise_age1 %>%
#   group_by(age_diff) %>%
#   count()
# 
# do2 <- revise_age1 %>%
#   group_by(age_diff2) %>%
#   count()
# glimpse(tele)
#------------------------------------------------------------------------------------------
# step2
revise_age1 <- revise_age1 %>%
  group_by(unique_id,year) %>%
  mutate(new_age3 = ifelse(year_diff >= 1 & age_diff2 < 0, (new_age2)-(age_diff2)+1 ,(new_age2)))

revise_age1 <- revise_age1 %>%
  group_by(unique_id) %>%
  mutate(age_diff3 = as.numeric(new_age3 - lag(new_age3, default = first(new_age3)))) %>%
  ungroup()

revise_age1 %>%
  group_by(age_diff3) %>%
  count()
#------------------------------------------------------------------------------------------
# step3
revise_age1 <- revise_age1 %>%
  group_by(unique_id,year) %>%
  mutate(new_age4 = first(new_age3))

revise_age1 <- revise_age1 %>%
  group_by(unique_id) %>%
  mutate(age_diff4 = as.numeric(new_age4 - lag(new_age4, default = first(new_age4)))) %>%
  ungroup()

revise_age1 %>%
  group_by(age_diff4) %>%
  count()
#------------------------------------------------------------------------------------------
# step4
revise_age1 <- revise_age1 %>%
  group_by(unique_id,year) %>%
  mutate(new_age5 = ifelse(year_diff >= 1 & age_diff4 < 0, (new_age4)-(age_diff4)+1 ,(new_age4)))

revise_age1 <- revise_age1 %>%
  group_by(unique_id) %>%
  mutate(age_diff5 = as.numeric(new_age5 - lag(new_age5, default = first(new_age5)))) %>%
  ungroup()

revise_age1 %>%
  group_by(age_diff5) %>%
  count()

#------------------------------------------------------------------------------------------
# step5
revise_age1 <- revise_age1 %>%
  group_by(unique_id,year) %>%
  mutate(new_age6 = first(new_age5))

revise_age1 <- revise_age1 %>%
  group_by(unique_id) %>%
  mutate(age_diff6 = as.numeric(new_age6 - lag(new_age6, default = first(new_age6)))) %>%
  ungroup()

revise_age1 %>%
  group_by(age_diff6) %>%
  count()
#------------------------------------------------------------------------------------------
# step6 (case special) unique_id 26044
revise_age1 <- revise_age1 %>%
  mutate(new_age7 = case_when(
    unique_id == 26044 & year == 2021 ~ 20 ,
    TRUE ~ (new_age6)))

revise_age1 <- revise_age1 %>%
  group_by(unique_id) %>%
  mutate(age_diff7 = as.numeric(new_age7 - lag(new_age7, default = first(new_age7)))) %>%
  ungroup()
  
revise_age1 %>%
  group_by(age_diff7) %>%
  count()
#------------------------------------------------------------------------------------------
#Prepare table for join 
revise_age_table <- revise_age1 %>%
  select(tran_id,new_age7)


revise_age <- left_join(revise_age,revise_age_table, by = "tran_id")


revise_age <- revise_age %>%
  group_by(tran_id) %>%
  mutate(new_age_ts = ifelse(all(!is.na(new_age7)), (new_age7), (age))) %>%
  ungroup()

revise_age <- revise_age %>%
  group_by(unique_id) %>%
  mutate(age_diff_new_age_ts = as.numeric(new_age_ts - lag(new_age_ts, default = first(new_age_ts)))) %>%
  ungroup()

revise_age %>%
  group_by(age_diff_new_age_ts) %>%
  count()

# Finish Part1
#------------------------------------------------------------------------------------------
# Part 2 
# step1 

revise_age2 <- revise_age %>%
  select(unique_id,tran_id,unique_var,age,new_date,year,new_age_ts,age_diff_new_age_ts) %>%
  arrange(unique_id, new_date)

revise_age2 <- revise_age2 %>%
  group_by(unique_id) %>%
  mutate(year_diff = as.numeric(year - lead(year, default = last(year)))) %>%
  ungroup()

revise_age2 <- revise_age2 %>%
  select(unique_id,tran_id,unique_var,age,new_date,year,year_diff,new_age_ts,age_diff_new_age_ts) %>%
  arrange(unique_id, new_date)

revise_age2 %>%
  group_by(age_diff_new_age_ts) %>%
  count()


do <- revise_age2 %>%
  filter(age_diff_new_age_ts >= 2) %>%
  arrange(unique_id, new_date)

list_agediff_more_1 <- do$unique_id

 
revise_age2 <- revise_age2 %>%
  filter(revise_age2$unique_id %in% list_agediff_more_1 )

#------------------------------------------------------------------------------------------
# Part2 - step1 
revise_age2 <- revise_age2 %>%
  group_by(unique_id,year) %>%
  mutate(new_age2 = as.numeric(names(which.max(table(age))))) %>%
  ungroup()
  
revise_age2 <- revise_age2 %>%
  group_by(unique_id) %>%
  mutate(age_diff2 = as.numeric(new_age2 - lead(new_age2, default = last(new_age2)))) %>%
  ungroup()  

revise_age2 %>%
  group_by(age_diff2) %>%
  count()
#------------------------------------------------------------------------------------------
# Part2 - step2

revise_age2 <- revise_age2 %>%
  group_by(unique_id,year) %>%
  mutate(new_age3 = ifelse(year_diff <= -1 & age_diff2 < -1 , (new_age2)-(age_diff2)+(year_diff) ,(new_age2))) %>%
  ungroup()

revise_age2 <- revise_age2 %>%
  group_by(unique_id) %>%
  mutate(age_diff3 = as.numeric(new_age3 - lead(new_age3, default = last(new_age3)))) %>%
  ungroup()  

revise_age2 %>%
  group_by(age_diff3) %>%
  count()
#------------------------------------------------------------------------------------------
# Part2 - step3

revise_age2 <- revise_age2 %>%
  group_by(unique_id,year) %>%
  mutate(new_age4 = last(new_age3))

revise_age2 <- revise_age2 %>%
  group_by(unique_id) %>%
  mutate(age_diff4 = as.numeric(new_age4 - lead(new_age4, default = last(new_age4)))) %>%
  ungroup()  

revise_age2 %>%
  group_by(age_diff4) %>%
  count()
#------------------------------------------------------------------------------------------
# Part2 - step4

revise_age2 <- revise_age2 %>%
  group_by(unique_id,year) %>%
  mutate(new_age5 = ifelse(year_diff <= -1 & age_diff4 < -1 , (new_age4)-(age_diff4)+(year_diff) ,(new_age4))) %>%
  ungroup()

revise_age2 <- revise_age2 %>%
  group_by(unique_id) %>%
  mutate(age_diff5 = as.numeric(new_age5 - lead(new_age5, default = last(new_age5)))) %>%
  ungroup()  

revise_age2 %>%
  group_by(age_diff5) %>%
  count()

#------------------------------------------------------------------------------------------
# Part2 - step5

revise_age2 <- revise_age2 %>%
  group_by(unique_id,year) %>%
  mutate(new_age6 = last(new_age5))

revise_age2 <- revise_age2 %>%
  group_by(unique_id) %>%
  mutate(age_diff6 = as.numeric(new_age6 - lead(new_age6, default = last(new_age6)))) %>%
  ungroup()  

revise_age2 %>%
  group_by(age_diff6) %>%
  count()


#finish Part 2 
#------------------------------------------------------------------------------------------


#Prepare table for join 
revise_age_table2 <- revise_age2 %>%
  select(tran_id,new_age6)


revise_age <- left_join(revise_age,revise_age_table2, by = "tran_id")


revise_age <- revise_age %>%
  group_by(tran_id) %>%
  mutate(new_age_ts2 = ifelse(all(!is.na(new_age6)), (new_age6), (new_age_ts))) %>%
  ungroup()

revise_age <- revise_age %>%
  group_by(unique_id) %>%
  mutate(age_diff_new_age_ts2 = as.numeric(new_age_ts2 - lag(new_age_ts2, default = first(new_age_ts2)))) %>%
  ungroup()

revise_age %>%
  group_by(age_diff_new_age_ts2) %>%
  count()
#------------------------------------------------------------------------------------------
# Final Part 

revise_age_final <- revise_age %>%
  select(tran_id,new_age_ts2)

tele2 <- left_join(tele2,revise_age_final, by = "tran_id")



#------------------------------------------------------------------------------------------
#check check check check check check check check check check check check check check check 
do <- tele2 %>%
  group_by(new_age_ts2) %>%
  count()
do <- tele2 %>%
  select(unique_id,tran_id,unique_var,new_date,year,age,new_age_ts2) %>%
  arrange(unique_id,new_date)

do <- do %>%
  group_by(unique_id) %>%
  mutate(age_diff_new_age_ts2 = as.numeric(new_age_ts2 - lag(new_age_ts2, default = first(new_age_ts2)))) %>%
  ungroup()

do %>%
  group_by(age_diff_new_age_ts2) %>%
  count()
#   age_diff_new_age_ts2      n
# 1                    0 230218
# 2                    1  28567
# 3                    2    262

## check sex

do <- tele2 
# variable sex old
do <- do %>%
  group_by(unique_id) %>%
  mutate(sexcheck  = as.numeric(sex - lag(sex, default = first(sex)))) %>%
  ungroup()
do %>%
  group_by(sexcheck) %>%
  count()
#   sexcheck      n
# 1       -1     27
# 2        0 258956
# 3        1     64
# variable sex current.
do <- do %>%
  group_by(unique_id) %>%
  mutate(new_sex2  = as.numeric(new_sex2 - lag(new_sex2, default = first(new_sex2)))) %>%
  ungroup()

do %>%
  group_by(new_sex2) %>%
  count()
#     new_sex2      n
#   1        0 259047

##NOTE (A - lag(A), default = first(new_sex2)) =  A - previous A 
##NOTE (A - lead(A), default = last(new_sex2)) =  A - next A 
#check check check check check check check check check check check check check check check 
#------------------------------------------------------------------------------------------
glimpse(tele2)


