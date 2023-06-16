
# Package install  --------------------------------------------------------
#install.packages(c("tidyverse",
#                   "patchwork",
#                   "lubridate"))
#library(plotly)
#library(corrplot)
library(ggplot2)
library(readr)
library(tidyverse)
library(glue)
library(lubridate)
library(dplyr)
library(data.table)
library(patchwork)

# Import data  ------------------------------------------------------------
setwd("C:/Users/thanayut.s/Desktop/TELEMEDICINE_YEAR2")
df <- readRDS("C:/Users/thanayut.s/Desktop/TELEMEDICINE_YEAR2/NHSOnew1.rds")


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
  arrange(pid,new_date) %>%
  mutate(unique_id = group_indices(., pid))
glimpse(tele)
print(max(tele$unique_id)) #have patient 110153 person 

# create variable for count unique first service 
tele <- tele %>%
  arrange(unique_id,new_date) %>%
  mutate(unique_var = ifelse(duplicated(pid), 0, 1))

sum(tele$unique_var)


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
    age >=6 & age<= 24 ~ "2",
    age >=25 & age<= 59 ~ "3",
    age >= 60 ~ "4"
  ))


tele <- tele %>%
  mutate(age_group = factor(age_group,
                            levels = c("1","2","3","4"),
                            labels = c("age group 0-5","age group 6-24","age group 25-59","age group 60+"),
                            ordered = T))

#* Age group of visit 
age_group_visit <- tele %>%
  count(age_group) %>%
  mutate(precent = glue("{round((n/sum(n))*100,2)}%"))
#         age_group      n precent
#1:   age group 0-5   8092   3.12%
#2:  age group 6-24  33123  12.79%
#3: age group 25-59 109651  42.33%
#4:   age group 60+ 108181  41.76%

#*Age group of unique patient 
age_group_unique <- tele %>%
  filter(unique_var==1) %>%
  count(age_group) %>%
  mutate(precent = glue("{round((n/sum(n))*100,2)}%"))
#         age_group     n precent
#1:   age group 0-5  2436   2.21%
#2:  age group 6-24 12509  11.36%
#3: age group 25-59 42004  38.13%
#4:   age group 60+ 53204   48.3%

#* Table age_group-sex
gender_table <- table(tele$age_group, tele$gender)
percentage_table <- round(prop.table(gender_table, margin = 1) * 100)

result <- cbind(gender_table, percentage_table)
colnames(result) <- c("Male", "Female", "Male %", "Female %")
rownames(result) <- unique(tele$age_group)
result <- data.frame(result)

#                 Male Female Male.. Female..
#age group 0-5    5086   3006      4        2
#age group 60+   20052  13071     16       10
#age group 25-59 56241  53410     46       39
#age group 6-24  41270  66911     34       49


#***Question 2. WHAT
#* 2.1 TOP PDX 
tele %>%
  group_by(pdx) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(20)
#1 I10   32729
#2 E119  21574
#3 F2000  9035
#4 F2003  6280
#5 F840   5965
#6 F900   4188
#7 N40    4023
#8 F2002  3014
#9 F322   2744
#10 J459   2576
#11 H903   2458
#12 F062   2347
#13 F809   2268
#14 E785   2191
#15 F200   2162
#16 F412   2098
#17 F419   2010
#18 C509   1981
#19 F321   1945
#20 I251   1849


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

tele$chapter <- as.numeric(tele$chapter) # revise type of variable 

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
  count() %>%
  arrange(desc(n))
#1 Mental and behavioural disorders                     94727
#2 Diseases of the circulatory system                   43847
#3 Endocrine, nutritional and metabolic diseases        33255
#4 Neoplasms                                            13960
#5 Factors influencing health status and contact with … 12468
#6 Diseases of the musculoskeletal system and connecti… 12217
#7 Diseases of the nervous system                       11247
#8 Diseases of the genitourinary system                  8248
#9 Diseases of the respiratory system                    7382
#10 Certain infectious and parasitic diseases             3667




#* 2.3  22 Disease group (NHSO policy for telemedicine)
#1. Hypertension	Disease gr. For burden of disease??? -> I10,I11,I119,I12,I129,I13,I131,I132,I139,I15,I151,I152,I158,I159		
#2. DM	Disease gr. For burden of disease???		-> E10,E11,E12,E13,E14
#3. Mental health	 (Mental and behavioural disorders)	->	F00–F99
#4. Asthma	"Disease gr. For burden of disease??? -> J40,J41,J411,J418,J42,J43,J431,J432,J438,J439,J44,J441,J448,J449,J45,J451,J458,J459,J46,J47
#5. Cancers	 (Neoplasms)		-> C00–D48
#6. Other diseases			-> Other

#tele <- tele %>%
#  mutate(NHSO_policy= case_when(
#    pdx == "I10"|pdx == "I11"|pdx == "I119"|pdx == "I12"|pdx == "I129"|pdx == "I13"|pdx == "I131"|pdx == "I132"|pdx == "I139"|pdx == "I15"|pdx == "I151"|pdx == "I152"|pdx == "I158"|pdx == "I159" ~ 1 ,
#    pdx == "E10"|pdx == "E11"|pdx == "E12"|pdx == "E13"|pdx == "E14" ~ 2 ,
#    chapter == 5 ~ 3,
#    pdx == "J40"|pdx == "J41"|pdx == "J411"|pdx == "J418"|pdx == "J42"|pdx == "J43"|pdx == "J431"|pdx == "J432"|pdx == "J438"|pdx == "J439"|pdx == "J44"|pdx == "J441"|pdx == "J448"|pdx == "J449"|pdx == "J45"|pdx == "J451"|pdx == "J459"|pdx == "J46"|pdx == "J47" ~ 4 ,
#    chapter == 2 ~ 5 ,
#    TRUE ~ 6
#  ))

tele <- tele %>%
  mutate(NHSO_policy= case_when(
    pdx %in% c("I10","I111","I112","I113","I114","I115","I116","I117","I118","I119","I121","I122","I123","I124","I125","I126","I127","I128","I129","I133","I134","I135","I136","I137","I138","I139","I14","I15","I150","I151","I152","I158","I159") ~ 1,
    pdx %in% c("E109","E119","E129","E139","E149","E100","E110","E120","E130","E140","E101","E111","E121","E131","E141","E102","E112","E122","E132","E142","E103","E113","E123","E133","E143","E104","E114","E124","E134","E144","E105","E115","E125","E135","E145","E106","E116","E126","E136","E146","E107","E117","E127","E137","E147","E118","E128","E138","E148")~ 2,
    chapter == 5 ~ 3,
    pdx %in% c("J45","J46","J450","J451","J452","J458","J459") ~ 4,
    chapter == 2 ~ 5 ,
    TRUE ~ 6
  ))

check <- tele %>%
  filter(block_char == "J") %>%
  group_by(pdx) %>%
  count()


tele %>%
  group_by(NHSO_policy) %>%
  count()

#NHSO_policy     n
#1           1 32917
#2           2 25967
#3           3 94727
#4           4  2706
#5           5 13960
#6           6 88770

tele <- tele %>%
  mutate(NHSO_policy_des = case_when(
    NHSO_policy == 1 ~ "Hypertension",
    NHSO_policy == 2 ~ "Diabetes",
    NHSO_policy == 3 ~ "Mental health",
    NHSO_policy == 4 ~ "Asthma",
    NHSO_policy == 5 ~ "Cancers",
    NHSO_policy == 6 ~ "Other",
  ))




tele %>%
  group_by(NHSO_policy,NHSO_policy_des) %>%
  count()

check <- tele %>%
  group_by(NHSO_policy) %>%
  count() 
sum(check$NHSO_policy)
# result from after new code ICD10.
#1           1  32921
#2           2     57
#3           3  94727
#4           4   4051
#5           5  13960
#6           6 113331


policy <- tele %>%
  group_by(NHSO_policy,NHSO_policy_des,year_month) %>%
  count()

library(tidyverse)
library(hrbrthemes)
library(kableExtra)
options(knitr.table.format = "html")
library(babynames)
library(streamgraph)
library(viridis)
library(DT)
library(plotly)
figure_policy1 <- policy %>%
  ggplot( aes(x=year_month, y=n, group=NHSO_policy_des, color=NHSO_policy_des,fill = NHSO_policy_des)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_color_viridis(discrete = TRUE) +
  scale_color_manual(values=c("darkred", "darkblue", "darkgreen", "orange","purple","gold")) +
  geom_text(aes(label = scales::comma(n)), vjust = -0.5, size = 3)+
  theme(legend.position="none", plot.title = element_text(size=30)) +
  ggtitle("disease treand") +
  theme_minimal() + 
  theme(axis.text = element_text(angle = 90, hjust = 1))


policy %>%
  ggplot( aes(x=year_month, y=n, group=NHSO_policy_des, color=NHSO_policy_des,fill = NHSO_policy_des)) +
  geom_col() + 
  scale_color_viridis(discrete = TRUE) +
  geom_text(aes(label = scales::comma(n)), vjust = -0.5, size = 3)+
  theme(legend.position="none", plot.title = element_text(size=30)) +
  ggtitle("disease treand") +
  theme_minimal() + 
  theme(axis.text = element_text(angle = 90, hjust = 1))

policy2  <- policy  %>%
  mutate(NHSO_policy_des_2= NHSO_policy_des)

figure_policy2  <-  policy2 %>%
  ggplot( aes(x=year_month, y=n)) +
  geom_line(data = policy2 , aes(group = NHSO_policy_des_2), color="darkblue", size=1.5, alpha=0.5) +
  geom_line(aes(color= NHSO_policy_des_2), color="#69b3a2", size=1.2 ) +
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    panel.grid = element_blank()
  ) +
  theme(axis.text = element_text(angle = 90, hjust = 1))+
  ggtitle("disease trend") +
  facet_wrap(~ NHSO_policy_des_2)

figure_policy1/figure_policy2

figure_policy1/result_alldisease






#tele %>%
#  filter(NHSO_policy == 1) %>%
#  group_by(pdx) %>%
#  count()

#*2.4 Age group - 6 desease group 

table(tele$NHSO_policy_des,tele$age_group)
#               age group 0-5 age group 6-24 age group 25-59 age group 60+
# Asthma                   82            498            1118          1008
# Cancers                 106            688            5533          7633
# Diabetes                  1            239           10148         15579
# Hypertension              2             79           10556         22280
# Mental health          3066          20278           53741         17642
# Other                  4835          11341           28555         44039



#***Question 3. WHERE
#* 3.1 Health region 
tele %>%
  group_by(zone)%>%
  count()
#   zone      n
#   <int>  <int>
#1     1   1745
#2     2  10403
#3     3    804
#4     4    516
#5     5  18874
#6     6   9965
#7     7   3033
#8     8  21852
#9     9  12693
#10    10   1463
#11    11  55069
#12    12   4362
#13    13 118268

#* 3.2 Province 
tele %>%
  group_by(province_name)%>%
  count() %>%
  arrange(desc(n))
#   province_name      n
#1 กรุงเทพฯ       118268
#2 สุราษฎร์ธานี      54258
#3 กาญจนบุรี        13924
#4 ชัยภูมิ           11589
#5 บึงกาฬ           6900
#6 อุดรธานี          6497
#7 สมุทรปราการ      6429
#8 ตาก             6231
#9 สุพรรณบุรี         4912
#10 หนองคาย         3123


#* 3.3 HNAME
tele %>%
  group_by(hname)%>%
  count() %>%
  arrange(desc(n))
#   hname                         n
#1 รพ.สวนสราญรมย์             52952
#2 รพ.รามาธิบดี  มหาวิทยาลัยมหิดล 41085
#3 สถาบันกัลยาณ์ราชนครินทร์       19175
#4 รพ.เวชการุณย์รัศมิ์            14192
#5 รพ.พหลพลพยุหเสนา           13924
#6 รพ.ราชพิพัฒน์                 7772
#7 รพ.ศิริราช                   6844
#8 รพ.ยุวประสาทไวทโยปถัมภ์       6429
#9 สถาบันประสาทวิทยา            6232
#10 รพ.สิรินธร                   5973

#* 3.4 Health region - 6 disease group 
table(zone = tele$zone,NHSO_policy = tele$NHSO_policy_des)

# NHSO_policy
# zone Asthma Cancers Diabetes Hypertension Mental health Other
# 1      20      98      143          376            70  1038
# 2      64      50     1814         4441          1640  2394
# 3      12       8       97          303           154   230
# 4       0       3        0            0            74   439
# 5     760     254     1393         3467           909 12091
# 6      20       9     1065         1657          6350   864
# 7      67      66      773          780           225  1122
# 8     348     539     7802         6115           983  6065
# 9     662      20     5849         4718           289  1155
# 10      1     295      360          150            66   591
# 11      5       2      249          263         52848  1702
# 12     30      35      696         1843            54  1704
# 13    717   12581     5726         8804         31065 59375

#tele %>%
#  group_by(zone,NHSO_policy) %>%
#  count()


#* 3.5 Province - 6 disease group 
head(table(Province = tele$province_name,NHSO_policy = tele$NHSO_policy_des))

# NHSO_policy
# Province    Asthma Cancers Diabetes Hypertension Mental health Other
# กรุงเทพฯ      717   12581     5726         8804         31065 59375
# กาญจนบุรี      758     254      172          369           908 11463
# กาฬสินธุ์        14       5      155           91             4   114
# กำแพงเพชร      0       4        3           95             0    26
# ขอนแก่น         5      43      452          431           189   207
# จันทบุรี         13       2       42           63             9   283

#* 3.6 HNAME - 6 disease group 
check <- table(hospital_name = tele$hname,NHSO_policy = tele$NHSO_policy)
head(check)

#***Question 4. WHEN
# copy variable date_adm for separate 
tele <- tele %>%
  mutate(date_adm2 = date_adm)
#separate date_adm 
tele <- tele %>%
  separate(date_adm2,c("d","m","y"))
glimpse(tele)
#change thai year to global year 
tele <- tele %>%
  mutate(yy = as.numeric(y)) %>%
  mutate(yy= yy -543 )

#change month from 1,2,3 to 01,02,03
tele <- tele %>%
  mutate(mm = as.numeric(m)) 
tele$mm <- sprintf("%02d",tele$mm)

#change date from 1,2,3 to 01,02,03
tele <- tele %>%
  mutate(dd = as.numeric(d)) 
tele$dd <- sprintf("%02d",tele$dd)

##glue new format for new_date 
tele <- tele %>%
  mutate(new_date = glue("{dd}-{mm}-{yy}"))
glimpse(tele)

##re type variable to date 
tele$new_date <- dmy(tele$new_date)
glimpse(tele)

##create new variable day, month, year, weekday, fullmonth 
tele <- tele %>%
  mutate(day = day(new_date),
         month = month(new_date),
         year = year(new_date),
         full_month = months(new_date,abbr = F),
         full_month2 = months(new_date,abbr = T),
         week_day = weekdays(new_date,abbr = FALSE))
glimpse(tele)



#* 4.1 Number of visits grouped by month
tele <- tele %>%
  mutate(year_month =format(tele$new_date, "%Y-%m"))

month_visit<-tele %>%
  group_by(year_month) %>%
  count()

# Convert the year_month variable to a Date type
month_visit$year_month <- ym(month_visit$year_month)

# Create a new variable for the year
month_visit$year <- format(month_visit$year_month, "%Y")

histogram <- ggplot(data = month_visit, mapping = aes(x = year_month,y = n ,fill = n)) +
  geom_col() +
  theme_minimal() +
  xlab("Timeline") +
  scale_fill_gradient(low = "yellow", high = "red") +
  ylab("Visit") +
  geom_text(aes(label = scales::comma(n)), vjust = -0.5, size = 3) +
  scale_x_date(date_labels = "%b-%y", breaks = '1 month', expand = c(0.001, 5)) +
  theme(axis.text = element_text(angle = 90, hjust = 1))+
  geom_smooth(aes(group = year, color = year), method = "lm", se = FALSE) +
  scale_color_manual(values = c("#69b3a2", "#FF69B4", "#ADFF2F"))


#*4.2 Number of visits <-->  6 disease gr. grouped by month
#*
# Libraries
library(tidyverse)
library(hrbrthemes)
library(kableExtra)
options(knitr.table.format = "html")
library(babynames)
library(streamgraph)
library(viridis)
library(DT)
library(plotly)


policy <- tele %>%
  group_by(NHSO_policy,NHSO_policy_des,year_month) %>%
  count()

figure_hypertension <- policy %>%
  mutate( highlight=ifelse(NHSO_policy_des == "Hypertension", "Hypertension", "Other")) %>%
  ggplot( aes(x=year_month, y=n, group=NHSO_policy_des, color=highlight, size=highlight)) +
  geom_line() +
  scale_color_manual(values = c("#69b3a2", "lightgrey")) +
  scale_size_manual(values=c(1.5,0.8)) +
  theme(legend.position="none") +
  theme_minimal() +
  ylab("Number of service")+
  theme(axis.text = element_text(angle = 90, hjust = 1))+
  geom_label( x="2022-08", y=7000, label="Hypertension service", size=5, color="#69b3a2") +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    axis.title.x = element_blank()
  )

figure_Asthma <- policy %>%
  mutate( highlight=ifelse(NHSO_policy_des == "Asthma", "Asthma", "Other")) %>%
  ggplot( aes(x=year_month, y=n, group=NHSO_policy_des, color=highlight, size=highlight)) +
  geom_line() +
  scale_color_manual(values = c("#69b3a2", "lightgrey")) +
  scale_size_manual(values=c(1.5,0.8)) +
  theme(legend.position="none") +
  theme_minimal() +
  ylab("Number of service")+
  theme(axis.text = element_text(angle = 90, hjust = 1))+
  geom_label( x="2022-08", y=7000, label="Asthma service", size=5, color="#69b3a2") +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

figure_Cancers <- policy %>%
  mutate( highlight=ifelse(NHSO_policy_des == "Cancers", "Cancers", "Other")) %>%
  ggplot( aes(x=year_month, y=n, group=NHSO_policy_des, color=highlight, size=highlight)) +
  geom_line() +
  scale_color_manual(values = c("#69b3a2", "lightgrey")) +
  scale_size_manual(values=c(1.5,0.8)) +
  theme(legend.position="none") +
  theme_minimal() +
  ylab("Number of service")+
  theme(axis.text = element_text(angle = 90, hjust = 1))+
  geom_label( x="2022-08", y=7000, label="Cancers service", size=5, color="#69b3a2") +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

figure_Diabetes <- policy %>%
  mutate( highlight=ifelse(NHSO_policy_des == "Diabetes", "Diabetes", "Other")) %>%
  ggplot( aes(x=year_month, y=n, group=NHSO_policy_des, color=highlight, size=highlight)) +
  geom_line() +
  scale_color_manual(values = c("#69b3a2", "lightgrey")) +
  scale_size_manual(values=c(1.5,0.8)) +
  theme(legend.position="none") +
  theme_minimal() +
  ylab("Number of service")+
  theme(axis.text = element_text(angle = 90, hjust = 1))+
  geom_label( x="2022-08", y=7000, label="Diabetes service", size=5, color="#69b3a2") +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

figure_mental <- policy %>%
  mutate( highlight=ifelse(NHSO_policy_des == "Mental health", "Mental health", "Other")) %>%
  ggplot( aes(x=year_month, y=n, group=NHSO_policy_des, color=highlight, size=highlight)) +
  geom_line() +
  scale_color_manual(values = c("#69b3a2", "lightgrey")) +
  scale_size_manual(values=c(1.5,0.8)) +
  theme(legend.position="none") +
  theme_minimal() +
  ylab("Number of service")+
  theme(axis.text = element_text(angle = 90, hjust = 1))+
  geom_label( x="2022-08", y=7000, label="Mental health service", size=5, color="#69b3a2") +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  )


figure_Other <- policy %>%
  mutate( highlight=ifelse(NHSO_policy_des == "Other", "1r", "Other")) %>%
  ggplot( aes(x=year_month, y=n, group=NHSO_policy_des, color=highlight, size=highlight)) +
  geom_line() +
  scale_color_manual(values = c("#69b3a2", "lightgrey")) +
  scale_size_manual(values=c(1.5,0.8)) +
  theme(legend.position="none") +
  theme_minimal() +
  ylab("Number of service")+
  theme(axis.text = element_text(angle = 90, hjust = 1))+
  geom_label( x="2022-08", y=7000, label="Other service", size=5, color="#69b3a2") +
  theme(
    legend.position="none",
    plot.title = element_text(size=14),
    axis.title.x = element_blank()
  )

result_alldisease <- figure_Asthma+figure_Cancers+figure_Diabetes+figure_hypertension+figure_mental+figure_Other


glimpse(tele)



# GIF Part ----------------------------------------------------------------


#GIF. disease trend animation
# libraries:
# library(ggplot2)
# library(gganimate)
# library(babynames)
# library(hrbrthemes)
# 
# 
# policy3 <- tele %>%
#   group_by(NHSO_policy,NHSO_policy_des,year) %>%
#   count()
# 
# test <- policy3 %>%
#   ggplot(aes(x = year, y = n, group = NHSO_policy_des, color = NHSO_policy_des)) +
#   geom_line(size = 1.5) +
#   geom_point() +
#   scale_color_viridis(discrete = TRUE) +
#   ggtitle("Trend telemedicine service") +
#   theme_minimal() +
#   ylab("Number of service") +
#   transition_reveal(year)
# 
# 
# # Save at gif:
# anim_save("gif_disease.gif", animation = test)






# Special part - unique patient by disease --------------------------------

# unique patient by disease
tele <- tele %>%
  group_by(pid, NHSO_policy_des) %>%
  mutate(unique_var_pid_pdx = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup()

# select variable 
test <- tele %>%
  select(pid,unique_id,NHSO_policy_des,unique_var_pid_pdx)

#group unique for check people uses telemedicine more than 1 desease
test2 <- test %>%
  group_by(unique_id,NHSO_policy_des) %>%
  count(unique_var_pid_pdx)

test3 <- test2 %>%
  filter(unique_var_pid_pdx == 1 , n >1 )

#count unique patient telemedicine service (unique pid-disease)
count_unique_pid_disease <- tele %>%
  filter(unique_var_pid_pdx == 1) %>%
  group_by(NHSO_policy_des) %>%
  count()

#numper of patien (unique pid-disease)
#unique pid = 110,153 ,unique(pid,disease =116,100)
sum(count_unique_pid_disease$n) #116100


#*Descriptive each disease
#*ASTHMA


des_asthma_unique <- tele %>%
  filter(unique_var_pid_pdx == 1) %>%
  filter(NHSO_policy_des == "Asthma") %>%
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

patient_asthma <- tele %>%
  filter(unique_var_pid_pdx == 1) %>%
  filter(NHSO_policy_des == "Asthma") %>%
  count() 

gender_asthma <- tele %>%
  filter(unique_var_pid_pdx == 1) %>%
  filter(NHSO_policy_des == "Asthma") %>%
  group_by(gender) %>%
  count()

agegroup_asthma <- tele %>%
  filter(unique_var_pid_pdx == 1) %>%
  filter(NHSO_policy_des == "Asthma") %>%
  group_by(age_group) %>%
  count()




##* WHERE -> trand top 5 pdx by month 


count_pdx_top5 <- tele %>%
  group_by(pdx) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(5)

list_pdx_top5 <- count_pdx_top5$pdx

data_pdx_top5 <- tele %>%
  filter(pdx %in% list_pdx_top5 )

glimpse(data_pdx_top5)

data_pdx_top5 <- data_pdx_top5 %>%
  group_by(pdx,year_month) %>%
  count()

data_pdx_top5 %>%
  ggplot( aes(x=year_month, y=n, group=pdx, color=pdx)) +
  geom_line(size =1 )+
  theme_minimal()+
  theme(axis.text = element_text(angle = 90, hjust = 1))


#top 5 each motnh 
top5_pdx_month <- tele %>%
  select(year_month,pdx) %>%
  group_by(year_month, pdx) %>% 
  count()

top5_pdx_month2 <- top5_pdx_month %>%
  group_by(year_month) %>%
  top_n(5, n) %>%
  arrange(year_month, desc(n))

top5_pdx_month2 %>%
  ggplot( aes(x=year_month, y=n, group=pdx, color=pdx)) +
  geom_line(size =1.5 )+
  theme_minimal()+
  theme(axis.text = element_text(angle = 90, hjust = 1))

