library(readxl)
library(dplyr)
library(skimr)
library(ggplot2)
library(glue)
library(lubridate)

data_table <- read_excel("Data table .xlsx", 
                          sheet = "Sheet1")
View(data_table)

df1 <- skim(data_table)

glimpse(data_table)


##----- REVISE TYPE OF VARRIABLE
data_table$year <- as.factor(data_table$year)

data_table <- data_table %>%
  mutate(month_year = as.character(glue("01-{month}-{year}")))


data_table$new_date <- dmy(data_table$month_year)

data_table <- data_table %>%
  mutate(day = day(new_date),
         month = month(new_date),
         year = year(new_date),
         full_month = months(new_date,abbr =F)
         full_month2 = months(new_date,abbreviate = T),
         week_day = weekdays(new_date,abbr=F)
         )

ggplot(data= data_table, mapping = aes(x=))
