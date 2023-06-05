
# Package install  --------------------------------------------------------
#install.packages(c("tidyverse",
#                   "patchwork",
#                   "lubridate"))
#library(plotly)
#library(corrplot)

library(readr)
library(tidyverse)
library(glue)
library(lubridate)
library(dplyr)
library(data.table)
library(patchwork)

# Import data  ------------------------------------------------------------
setwd("~/Desktop/Project_telemed_year2")
df <- readRDS("~/Desktop/Project_telemed_year2/1. Data telemedicine/5. Data_P_toon /NHSOnew1.rds")


# Change variable to lower case -------------------------------------------
names(df) <-tolower(names(df))
glimpse(df)


# Count sub_fund ----------------------------------------------------------
count_subfund <- df %>%
  count(sub_fund)

# Filter subfund = TELEMED only -------------------------------------------
tele <- df %>%
  filter(sub_fund =="TELEMED")


# Function Check NA  ------------------------------------------------------

check_na <- function(x) {
  sum(is.na(x))
}
apply(tele,MARGIN = 2,check_na)

pdx <- tele %>%
  count(pdx)

# Gen variable for unique  ------------------------------------------------

#count summary unique 
tele <- tele %>%
  mutate(unique_id = group_indices(., pid))
glimpse(tele)
print(max(tele$unique_id)) #have patient 110153 person 

# create variable for count unique first service 
tele <- tele %>%
  mutate(unique_var = ifelse(duplicated(pid), 0, 1))

#***Question 1. WHO
#**1.1 Total number visit - unique patient
visit_ <-tele %>%
  count()
unique_ <- max(tele$unique_id)
table_visit_unique <- data.frame(c(number_of_visit = visit_,
                                   number_of_patient = unique_))
table_visit_unique 
#  number_of_visit.n number_of_patient
#1            259047            110153

#**1.2 Number of visit - sex , Number of unique - sex 
tele <- tele %>%
  mutate(gender = factor(sex,
                         levels =c(1,2),
                         labels = c('male','female')))
tele %>%
  count(gender)
#   gender      n
#1:   male 122649
#2: female 136398

tele %>%
  filter(unique_var == 1 ) %>%
  count(gender)
#   gender     n
#1:   male 47039
#2: female 63114
  
#**1.3 Mean age of patient 

#function mode 
Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

Modes(tele$age)

summarise_age_visit <- tele %>%
  summarise(
    min = min(age),
    max = max(age),
    mean = mean(age),
    sd = sd(age),
    median = median(age),
    q1 = quantile(age,0.25),
    q2 = quantile(age,0.50),
    q3 = quantile(age,0.75),
    iqr = IQR(age),
    mode = Modes(tele$age))
round(summarise_age_visit)
# min max mean sd median q1 q2 q3 iqr mode
#   0 123   51 22     55 35 55 68  33   65


summarise_age_unique <- tele %>%
  filter(unique_var == 1) %>%
  summarise(
    min = min(age),
    max = max(age),
    mean = mean(age),
    sd = sd(age),
    median = median(age),
    q1 = quantile(age,0.25),
    q2 = quantile(age,0.50),
    q3 = quantile(age,0.75),
    iqr = IQR(age),
    mode = Modes(tele$age))
round(summarise_age_unique)
#  min max mean sd median q1 q2 q3 iqr mode
#    0 123   54 21     59 42 59 69  27   65

#**1.4 Age group
tele <- tele %>%
  mutate(age_group = case_when(
    age >=0 & age <=5 ~ "1",
    age >=6 & age<= 14 ~ "2",
    age >=15 & age<= 29 ~ "3",
    age >= 30 & age <= 59 ~ "4",
    age >= 60 ~ "5"
  ))
tele <- tele %>%
  mutate(age_group = factor(age_group,
                            levels = c("1","2","3","4","5"),
                            labels = c("age group 0-5","age group 6-15","age group 15 -19","age group 30-59","age group 60+"),
                            ordered = T))

#* Age group of visit 
age_group_visit <- tele %>%
  count(age_group) %>%
  mutate(precent = glue("{round((n/sum(n))*100)}%"))
#age_group      n precent
#1:    age group 0-5   8092      3%
#2:   age group 6-15  16345      6%
#3: age group 15 -19  27291     11%
#4:  age group 30-59  99138     38%
#5:    age group 60+ 108181     42%

#*Age group of unique patient 
age_group_unique <- tele %>%
  filter(unique_var==1) %>%
  count(age_group) %>%
  mutate(precent = glue("{round((n/sum(n))*100)}%"))
#age_group     n precent
#1:    age group 0-5  2436      2%
#2:   age group 6-15  6274      6%
#3: age group 15 -19  9423      9%
#4:  age group 30-59 38816     35%
#5:    age group 60+ 53204     48%

#* Table age_group-sex
gender_table <- table(tele$age_group, tele$gender)
percentage_table <- round(prop.table(gender_table, margin = 2) * 100)

result <- cbind(gender_table, percentage_table)
colnames(result) <- c("Male", "Female", "Male %", "Female %")
rownames(result) <- unique(tele$age_group)
result <- data.frame(result)

#                   Male Female Male.. Female..
#age group 0-5     5086   3006      4        2
#age group 60+    10499   5846      9        4
#age group 30-59  16557  10734     13        8
#age group 15 -19 49237  49901     40       37
#age group 6-15   41270  66911     34       49


#***Question 2. WHAT
#* 2.1 TOP PDX 
tele %>%
  group_by(pdx) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(20)


#* 2.2  22 Disease group (ICD10 chapters)
tele<- tele %>%
  mutate(block_char = substr(tele$pdx,1,1),
         block_num = substr(tele$pdx,2,3))

tele$block_num <- as.numeric(tele$block_num)

tele <- tele %>%
  mutate(chapter = case_when(
    block_char == "A" | block_char == "B" & block_num >=00 & block_num <= 99 ~ "1",
    block_char == "C" | block_char == "D" & block_num >=00 & block_num <= 48 ~ "2",
    block_char == "D" & block_num >=50 & block_num <= 89 ~ "3",
    block_char == "E" & block_num >=00 & block_num <= 90 ~ "4",
    block_char == "F" & block_num >=00 & block_num <= 99 ~ "5",
    block_char == "G" & block_num >=00 & block_num <= 99 ~ "6", 
    block_char == "H" & block_num >=00 & block_num <= 59 ~ "7",
    block_char == "H" & block_num >=60 & block_num <= 95 ~ "8",
    block_char == "I" & block_num >=00 & block_num <= 99 ~ "9",
    block_char == "J" & block_num >=00 & block_num <= 99 ~ "10",
    block_char == "K" & block_num >=00 & block_num <= 93 ~ "11",
    block_char == "L" & block_num >=00 & block_num <= 99 ~ "12",
    block_char == "M" & block_num >=00 & block_num <= 99 ~ "13",
    block_char == "N" & block_num >=00 & block_num <= 99 ~ "14",
    block_char == "O" & block_num >=00 & block_num <= 99 ~ "15",
    block_char == "P" & block_num >=00 & block_num <= 96 ~ "16",
    block_char == "Q" & block_num >=00 & block_num <= 99 ~ "17",
    block_char == "R" & block_num >=00 & block_num <= 99 ~ "18",
    block_char == "S" | block_char == "T"& block_num >=00 & block_num <= 98 ~ "19",
    block_char == "V" | block_char == "Y"& block_num >=01 & block_num <= 98 ~ "20",
    block_char == "Z" & block_num >=00 & block_num <= 99 ~ "21",
    block_char == "U" | block_char == "T"& block_num >=00 & block_num <= 99 ~ "22",
  ))
  
tele$chapter <- as.numeric(tele$chapter) # revise type of varriable 
  
check <- tele %>%
  group_by(chapter)%>%
  count()

check <- tele %>%
  filter(chapter == 10) %>%
  group_by(pdx) %>%
  count() %>%
  arrange()

tele <- tele %>%
  mutate(title = case_when(
    chapter == 1 ~ "Certain infectious and parasitic diseases",
    chapter == 2 ~ "Neoplasms",
    chapter == 3 ~ "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
    chapter == 4 ~ "Endocrine, nutritional and metabolic diseases",
    chapter == 5 ~ "Mental and behavioural disorders",
    chapter == 6 ~ "Diseases of the nervous system",
    chapter == 7 ~ "Diseases of the eye and adnexa",
    chapter == 8 ~ "Diseases of the ear and mastoid process",
    chapter == 9 ~ "Diseases of the circulatory system",
    chapter == 10 ~ "Diseases of the respiratory system",
    chapter == 11 ~ "Diseases of the digestive system",
    chapter == 12 ~ "Diseases of the skin and subcutaneous tissue",
    chapter == 13 ~ "Diseases of the musculoskeletal system and connective tissue",
    chapter == 14 ~ "Diseases of the genitourinary system",
    chapter == 15 ~ "Pregnancy, childbirth and the puerperium",
    chapter == 16 ~ "Certain conditions originating in the perinatal period",
    chapter == 17 ~ "Congenital malformations, deformations and chromosomal abnormalities",
    chapter == 18 ~ "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
    chapter == 19 ~ "Injury, poisoning and certain other consequences of external causes",
    chapter == 20 ~ "External causes of morbidity and mortality",
    chapter == 21 ~ "Factors influencing health status and contact with health services",
    chapter == 22 ~ "Codes for special purposes",
  ))

check <- tele %>%
  group_by(title)%>%
  count()



#* 2.3  22 Disease group (NHSO policy for telemedicine)
#1. Hypertension	Disease gr. For burden of disease??? -> I10,I11,I119,I12,I129,I13,I131,I132,I139,I15,I151,I152,I158,I159		
#2. DM	Disease gr. For burden of disease???		-> E10,E11,E12,E13,E14
#3. Mental health	 (Mental and behavioural disorders)	->	F00–F99
#4. Asthma	"Disease gr. For burden of disease??? -> J40,J41,J411,J418,J42,J43,J431,J432,J438,J439,J44,J441,J448,J449,J45,J451,J458,J459,J46,J47
#5. Cancers	 (Neoplasms)		-> C00–D48
#6. Other diseases			-> Other

tele <- tele %>%
  mutate(NHSO_policy= case_when(
    pdx == "I10"|pdx == "I11"|pdx == "I119"|pdx == "I12"|pdx == "I129"|pdx == "I13"|pdx == "I131"|pdx == "I132"|pdx == "I139"|pdx == "I15"|pdx == "I151"|pdx == "I152"|pdx == "I158"|pdx == "I159" ~ 1 ,
    pdx == "E10"|pdx == "E11"|pdx == "E12"|pdx == "E13"|pdx == "E14" ~ 2 ,
    chapter == 5 ~ 3,
    pdx == "J40"|pdx == "J41"|pdx == "J411"|pdx == "J418"|pdx == "J42"|pdx == "J43"|pdx == "J431"|pdx == "J432"|pdx == "J438"|pdx == "J439"|pdx == "J44"|pdx == "J441"|pdx == "J448"|pdx == "J449"|pdx == "J45"|pdx == "J451"|pdx == "J459"|pdx == "J46"|pdx == "J47" ~ 4 ,
    chapter == 2 ~ 5 ,
    TRUE ~ 6
  ))


tele %>%
  group_by(NHSO_policy) %>%
  count()

tele %>%
  filter(NHSO_policy == 1) %>%
  group_by(pdx) %>%
  count()
