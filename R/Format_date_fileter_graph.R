library(tidyverse)
library(glue)
library(plotly)
library(patchwork)
library(corrplot)
library(lubridate)
library(readr)
library(ggplot2)
covid_data <- read_csv("Desktop/Data bootcamp/0. Project/2. Project_Health Financing /owid-covid-data.csv")
View(covid_data)

glimpse(covid_data)


??lubridate
data <- covid_data %>% 
  select(continent,location,date,total_cases,new_cases)%>%
  filter(location == "Thailand") %>%
  filter(total_cases > 0 & new_cases > 0) %>%
  mutate(day = day(data$date),
         month = month(data$date,label=T),
         year = year(data$date),
         week_day = wday(data$date,label=T,abbr=F),
         month_full =month(ymd(data$date),label=T,abbr = F))
glimpse(data)
view(data)



#analysis data 

data %>% 
  tibble() %>% 
  filter(date)
  summarise(sum = sum(new_cases))


##8 annotate
ggplot(data = data,mapping = aes(x=date,y=new_cases))+
  geom_line()+ 
  scale_x_date(date_labels="%d-%b-%y",breaks = '1 month',expand = c(0.01,0))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  annotate(geom="text",x= ymd("2021-11-01"),col ="blue",angle = 0, y=30000, label = "AA")+
  annotate(geom = "curve", x =ymd("2021-10-01"), y = 30000, xend = ymd("2021-08-14"), yend = 25000, 
    curvature = 0.3, arrow = arrow(length = unit(2, "mm")))+
  annotate("rect", xmin = ymd("2020-04-01"), xmax = ymd("2021-07-01"), ymin = 0, ymax = 45000,fill= "red",
               alpha = .2)+
  annotate("rect", xmin = ymd("2022-02-01"), xmax = ymd("2022-08-01"), ymin = 0, ymax = 45000,fill= "green",
           alpha = .2)+
  annotate(geom="text",x= ymd("2021-05-15"),col ="blue",angle = 0, y=40000, label = "First Wave",size=6)+
  geom_vline(xintercept = ymd("2021-02-01"),color = "red" )
