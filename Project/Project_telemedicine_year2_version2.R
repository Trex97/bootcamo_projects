
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

tele <- tele %>%
  mutate(NHSO_policy= case_when(
    pdx == "I10"|pdx == "I11"|pdx == "I119"|pdx == "I12"|pdx == "I129"|pdx == "I13"|pdx == "I131"|pdx == "I132"|pdx == "I139"|pdx == "I15"|pdx == "I151"|pdx == "I152"|pdx == "I158"|pdx == "I159" ~ 1 ,
    pdx == "E10"|pdx == "E11"|pdx == "E12"|pdx == "E13"|pdx == "E14" ~ 2 ,
    chapter == 5 ~ 3,
    pdx == "J40"|pdx == "J41"|pdx == "J411"|pdx == "J418"|pdx == "J42"|pdx == "J43"|pdx == "J431"|pdx == "J432"|pdx == "J438"|pdx == "J439"|pdx == "J44"|pdx == "J441"|pdx == "J448"|pdx == "J449"|pdx == "J45"|pdx == "J451"|pdx == "J459"|pdx == "J46"|pdx == "J47" ~ 4 ,
    chapter == 2 ~ 5 ,
    TRUE ~ 6
  ))


check <- tele %>%
  group_by(NHSO_policy) %>%
  count() 
sum(check$NHSO_policy)

#1           1  32921
#2           2     57
#3           3  94727
#4           4   4051
#5           5  13960
#6           6 113331

#tele %>%
#  filter(NHSO_policy == 1) %>%
#  group_by(pdx) %>%
#  count()

#*2.4 Age group - 6 desease group 

table(tele$age_group,tele$NHSO_policy)
#                       1     2     3     4     5     6
#age group 0-5        2     0  3066    80   106  4838
#age group 6-15       7     0 10159   301   216  5662
#age group 15 -19   179     1 17323   234   747  8807
#age group 30-59  10450    15 46537  1256  5258 35622
#age group 60+    22283    41 17642  2180  7633 58402

glimpse(tele)

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
table(zone = tele$zone,NHSO_policy = tele$NHSO_policy)
#NHSO_policy
#zone     1     2     3     4     5     6
#1    376     0    70    25    98  1176
#2   4441     0  1640   106    50  4166
#3    303     0   154    17     8   322
#4      0     0    74     0     3   439
#5   3467     0   909   791   254 13453
#6   1657     0  6350    30     9  1919
#7    780     0   225    80    66  1882
#8   6116    29   983   481   539 13704
#9   4718     0   289   935    20  6731
#10   150     1    66     3   295   948
#11   263     0 52848     8     2  1948
#12  1843     0    54    53    35  2377
#13  8807    27 31065  1522 12581 64266

#tele %>%
#  group_by(zone,NHSO_policy) %>%
#  count()


#* 3.5 Province - 6 disease group 
table(Province = tele$province_name,NHSO_policy = tele$NHSO_policy)
#* 3.6 HNAME - 6 disease group 
check <- table(hospital_name = tele$hname,NHSO_policy = tele$NHSO_policy)
check <- data.frame(check)
write_csv(check,"check.csv")


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
  xlab("Date") +
  scale_fill_gradient(low = "yellow", high = "red") +
  ylab("Month-Year") +
  geom_text(aes(label = scales::comma(n)), vjust = -0.5, size = 3) +
  scale_x_date(date_labels = "%b-%y", breaks = '1 month', expand = c(0.001, 0)) +
  theme(axis.text = element_text(angle = 90, hjust = 1))+
  geom_smooth(aes(group = year, color = year), method = "lm", se = FALSE) +
  scale_color_manual(values = c("darkblue", "darkgreen", "pink2"))





#trend for each year
#trend_plots <- lapply(unique(month_visit$year), function(year) {
#  data <- month_visit[month_visit$year == year, ]
#  plot <- ggplot(data = data, mapping = aes(x = year_month, y = n)) +
#    geom_line(color = "blue") +
#    scale_x_date(date_labels = "%b-%y", breaks = '1 month', expand = c(0.001, 0)) +
#    ylab("Visit") +
#    ggtitle(year)
#  plot
#})
#library(gridExtra)
#combined_plot <- grid.arrange(histogram, grobs = trend_plots, nrow = 2)


















