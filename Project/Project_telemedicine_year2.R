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

setwd("C:/Users/Au_ba/OneDrive/เดสก์ท็อป/telemed_year2")
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
count_subfund

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
a
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
glimpse(tele)
##create new variable day, month, year, weekday, fullmonth 
tele <- tele %>%
  mutate(day = day(new_date),
         month = month(new_date),
         year = year(new_date),
         full_month = month(new_date,label = T,abbr = F),
         week_day = wday(new_date,label = TRUE , abbr = FALSE))
glimpse(tele)
view(tele)

## Count visit by hospital(hnname)
tele %>%
  group_by(hname)%>%
  count()

#tele1 <- tele1 %>%
#  mutate(yy =as.numeric(y))%>%
#  mutate(yy = 2500+yy)%>%
#  mutate(yy = yy-543) %>%
#  mutate(new_date = glue("{d}-{m}-{yy}"))%>%
#  mutate(new_date = as.character(new_date))

tele %>%
  select(new_date) %>%
  summary()

#ggplot(data= sample_n(tele1,1000),mapping = aes(x=new_date,y = TRAN_ID))+
#  geom_smooth()+ geom_point()


#ggplot(data= sample_n(tele,1000) ,mapping = aes(x=new_date,y = tran_id))+
#  geom_line()+
#  geom_smooth()+ 
#  geom_point()

## group data count visit by new_date 
time_line_visit <- tele %>%
  group_by(new_date)%>%
  count()

## plot trend use telemedicine 
ggplot(data = time_line_visit, mapping = aes(x=new_date, y = n)) +
  geom_line()+
  geom_smooth()+
  theme_minimal()+
  scale_x_date(date_labels = "%d-%b-%y",breaks = '1 month' ,expand = c(0.001,0))+
  theme(axis.text = element_text(angle = 90 , hjust = 1))

ggplot(data = sample_n(tele,2000), mapping = aes(x=new_date, y = tran_id)) +
  geom_line()+
  geom_smooth()+
  theme_minimal()+
  scale_x_date(date_labels = "%d-%b-%y",breaks = '1 month' ,expand = c(0.001,0))+
  theme(axis.text = element_text(angle = 90 , hjust = 1))

#
#tele1 <- tele1 %>%
#  tibble()%>%
#  mutate(SEX = if_else(SEX==1,"Male","Female"))

#tele1 %>%
#  count(SEX)

#ggplot(data= tele1,mapping = aes(x= week_day,fill =SEX))+
#  geom_bar()
