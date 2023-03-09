#install.packages(c("tidyverse",
#                   "patchwork",
#                   "lubridate"))
library(readr)
library(tidyverse)
library(glue)
#library(plotly)
#library(patchwork)
#library(corrplot)
library(lubridate)

setwd("~/Desktop/Project_telemed_year2/1.Data telemedicine")
##Import data 
df <- read.table('tmp_TELEMED_20230308.txt',sep='|', header=TRUE ,fill = T)
View(df)
glimpse(df)

##rename variable to lower case 
names(df) <-tolower(names(df))
glimpse(df)

##build new variable 
#df <- data_tele
#glimpse(df)

## Count sub_fund
count_subfund <-df %>%
  group_by(sub_fund) %>%
  count()


##filter data 
tele <- df %>%
  filter(sub_fund == "TELEMED")



## unique patient telemed
list_unique <- data.frame(pid = unique(tele$pid),
            n = 1)

list_unique
head(list_unique)

## data frame  visit-unique patient tele
count_visit <- tele %>%
  count()
count_unique <- count(list_unique)

a <- data.frame(c(visit = count_visit,
           patient = count_unique))

##create data_unique only 
data_unique <- tele %>%
  mutate(unique_count =if_else(pid == list_unique$pid,1,0))

data_unique %>%
  summary()

data_unique %>%
  filter(unique_count == 1) %>%
  count()

## Count visit by unique patient 
visit_person_unique <- tele %>%
  group_by(pid) %>%
  count()



visit_person_unique %>%
  summary()

test  <- head(visit_person_unique,100)

ggplot(data=visit_person_unique , mapping = aes(x= n,y=pid))+
  geom_point()+geom_abline()

##-----------------------------------
#tele <- tele %>% 
#  mutate(d = substr(tele$date_adm,1,2),
#         m = substr(tele$date_adm,4,6),
#         y = substr(tele$date_adm,7,10))
#glimpse(tele)

#view(tele)

#tele <- tele %>% 
#  mutate(d = substr(tele$DATE_ADM,1,2),
#         m = substr(tele$DATE_ADM,4,6),
#         y = substr(tele$DATE_ADM,7,10))

## copy variable date_adm for separate 
tele <- tele %>%
  mutate(date_adm2 = date_adm)
##separate date_adm 
tele <- tele %>%
  separate(date_adm2,c("m","d","y"))
glimpse(tele)


##glue new format for new_date 
tele <- tele %>%
  mutate(new_date = glue("{d}-{m}-{y}"))
glimpse(tele)

##re type variable to date 
tele$new_date <- dmy(tele$new_date)

##create new variable day, month, year, weekday, fullmonth 
tele <- tele %>%
  mutate(day = day(new_date),
         month = month(new_date),
         year = year(new_date),
         full_month = month(new_date,label = T,abbr = F),
         week_day = wday(new_date,label = TRUE , abbr = FALSE))
  
view(tele)

tele1 <- tele1 %>%
  mutate(yy =as.numeric(y))%>%
  mutate(yy = 2500+yy)%>%
  mutate(yy = yy-543) %>%
  mutate(new_date = glue("{d}-{m}-{yy}"))%>%
  mutate(new_date = as.character(new_date))

tele1 %>% 
  select(DATE_ADM,d,m,yy,new_date)

glimpse(tele1)

tele1$new_date<- dmy(tele1$new_date)

tele1 <- tele1 %>%
  mutate(date = day(new_date),
         month = month(new_date),
         year = year(new_date),
         week_day = wday(new_date,label = T,abbr = F)) 
tele1 <- data.frame(tele1)
ggplot(data= sample_n(tele1,1000),mapping = aes(x=new_date,y = TRAN_ID))+
  geom_smooth()+ geom_point()

ggplot(data=sample_n(tele1,1000),mapping = aes(x=new_date,))+
  geom_point()

tele1 <- tele1 %>%
  tibble()%>%
  mutate(SEX = if_else(SEX==1,"Male","Female"))

tele1 %>%
  count(SEX)

ggplot(data= tele1,mapping = aes(x= week_day,fill =SEX))+
  geom_bar()


view(tele1)

list(tele1$week_day)

glimpse(tele1)
aa <- tele1 %>% 
  tibble%>%
  filter(week_day == c("Monday","Tursday","Wednesday","Thursday","Friday"))%>%
  group_by(new_date)%>%
  summarise(n=n())

aa$n <- as.numeric(aa$n)
class(aa$n)
ggplot(data=aa,mapping = aes(x=new_date,y=n))+
  geom_smooth()+geom_point()

ggplot(data=aa,mapping = aes(x=new_date,y=n))+
  geom_line()+
  theme_minimal()+
  scale_x_date(date_labels="%d-%b-%Y",breaks = '1 month',expand = c(0.01,0))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

tele1 %>% 
  filter(week_day ==c("Monday","Tuesday","Wednesday","Thursday","Friday"))%>%
  group_by(week_day) %>%
  summarise(n=n())







