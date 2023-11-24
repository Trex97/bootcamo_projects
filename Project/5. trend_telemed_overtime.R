library(readxl)
library(dplyr)
library(skimr)
library(ggplot2)
library(glue)
library(lubridate)
library(tidyverse)

data_table <- read_excel("Downloads/Data table_ts .xlsx", 
                         sheet = "Nvisitovertime")
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
         full_month = months(new_date,abbr =F) ,
         full_month2 = months(new_date,abbreviate = T),
         week_day = weekdays(new_date,abbr=F)
  )


data_table <- data_table %>%
  mutate(year_month =format(data_table$new_date, "%b-%Y"))

glimpse(data_table)

library(tidyr)

#Example---------------------------------------------------------
# # สร้างตัวแปร data สำหรับเก็บข้อมูล
# data <- data.frame(
#   year = c(2021),
#   month = c("May"),
#   NHSO = c(9789),
#   Rama = c(21564),
#   Rajavithi = c(60),
#   Phang_nga = c(18),
#   Suansaranrom = c(7)
# )
# 
# # ใช้ tidyr::pivot_longer() เพื่อแปลงข้อมูลให้อยู่ในรูปแบบ long format
# data_long <- data %>%
#   tidyr::pivot_longer(
#     cols = -c(year, month),
#     names_to = "Hospital",
#     values_to = "Value"
#   )
# 
# # แสดงข้อมูลหลังจากแปลง
# print(data_long)
#---------------------------------------------------------

glimpse(data_table)

data_table2 <- data_table %>%
  select(new_date,NHSO,Rama,Rajavithi,`Phang-nga`,Suansaranrom)
# ใช้ tidyr::pivot_longer() เพื่อแปลงข้อมูลให้อยู่ในรูปแบบ long format
data_long <- data_table2 %>%
  tidyr::pivot_longer(
    cols = -c(new_date),
    names_to = "Hospital",
    values_to = "Value"
  )




glimpse(data_long) 
data_long <- data_long %>%
  filter(Hospital != "NHSO")

all <- ggplot(data = data_long, mapping = aes(x = new_date, y = Value, color = Hospital, group = Hospital, fill = Hospital, shape = Hospital)) +
  geom_line(size = 1.3) +
  geom_point(size =2)+
  scale_color_manual(values = c("NHSO" = "#7facd6", "Rama" = "#f8956e", "Rajavithi" = "#fec304", "Phang-nga" = "#c0b8da", "Suansaranrom" = "#96d7c6"),
                     name = "Description",
                     labels = c(
                       "NHSO" = "NHSO", 
                       "Rama" = "T1", 
                       "Rajavithi" = "T2", 
                       "Phang-nga" = "SN", 
                       "Suansaranrom" = "SP"
                     )) +
  labs(title = "",
       x = "Month-Year",
       y = "Number of Visits") +
  theme_minimal() +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b-%y", breaks = '1 month', expand = c(0.001, 5)) +
  annotate("rect", xmin = ymd("2021-04-01"), xmax = ymd("2021-05-01"), ymin = 0, ymax = 40000, fill = "#A7A7A7", alpha = 0.2) +
  geom_rect(aes(xmin = ymd("2021-04-01"), xmax = ymd("2021-05-01"), ymin = 0, ymax = 40000), fill = NA, color = "#A7A7A7", linetype = "dashed") +
  annotate("rect", xmin = ymd("2021-06-01"), xmax = ymd("2021-08-01"), ymin = 0, ymax = 40000, fill = "#A7A7A7", alpha = 0.2) +
  geom_rect(aes(xmin = ymd("2021-06-01"), xmax = ymd("2021-08-01"), ymin = 0, ymax = 40000), fill = NA, color = "#A7A7A7", linetype = "dashed") +
  annotate("rect", xmin = ymd("2021-12-01"), xmax = ymd("2022-03-01"), ymin = 0, ymax = 40000, fill = "#A7A7A7", alpha = 0.2) +
  geom_rect(aes(xmin = ymd("2021-12-01"), xmax = ymd("2022-03-01"), ymin = 0, ymax = 40000), fill = NA, color = "#A7A7A7", linetype = "dashed") +
  annotate(geom = "text", x = ymd("2021-04-15"), col = "black", angle = 0, y = 42000, label = "Alpha",size = 5) +
  annotate(geom = "text", x = ymd("2021-07-01"), col = "black", angle = 0, y = 42000, label = "Delta",size = 5) +
  annotate(geom = "text", x = ymd("2022-01-15"), col = "black", angle = 0, y = 42000, label = "Omicron",size = 5) + 
  theme(
    plot.caption = element_text(face = "bold"),
    text = element_text(face = "bold")  # ทำให้ข้อความทั้งหมดใน plot เป็นตัวหนา
  )




all1 <- ggplot(data = data_long, mapping = aes(x = new_date, y = Value, color = Hospital, group = Hospital, fill = Hospital, shape = Hospital)) +
  geom_line(size = 1.3) +
  geom_point(size =2)+
  scale_color_manual(values = c("NHSO" = "#7facd6", "Rama" = "#f8956e", "Rajavithi" = "#fec304", "Phang-nga" = "#c0b8da", "Suansaranrom" = "#96d7c6")) +
  labs(title = "",
       x = "Month-Year",
       y = "Number of Visits") +
  theme_minimal() +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b-%y", breaks = '1 month', expand = c(0.001, 5)) +
  annotate("rect", xmin = ymd("2021-04-01"), xmax = ymd("2021-05-01"), ymin = 0, ymax = 40000, fill = "#A7A7A7", alpha = 0.2) +
  geom_rect(aes(xmin = ymd("2021-04-01"), xmax = ymd("2021-05-01"), ymin = 0, ymax = 40000), fill = NA, color = "#A7A7A7", linetype = "dashed") +
  annotate("rect", xmin = ymd("2021-06-01"), xmax = ymd("2021-08-01"), ymin = 0, ymax = 40000, fill = "#A7A7A7", alpha = 0.2) +
  geom_rect(aes(xmin = ymd("2021-06-01"), xmax = ymd("2021-08-01"), ymin = 0, ymax = 40000), fill = NA, color = "#A7A7A7", linetype = "dashed") +
  annotate("rect", xmin = ymd("2021-12-01"), xmax = ymd("2022-03-01"), ymin = 0, ymax = 40000, fill = "#A7A7A7", alpha = 0.2) +
  geom_rect(aes(xmin = ymd("2021-12-01"), xmax = ymd("2022-03-01"), ymin = 0, ymax = 40000), fill = NA, color = "#A7A7A7", linetype = "dashed") +
  annotate(geom = "text", x = ymd("2021-04-15"), col = "black", angle = 0, y = 42000, label = "Alpha") +
  annotate(geom = "text", x = ymd("2021-07-01"), col = "black", angle = 0, y = 42000, label = "Delta") +
  annotate(geom = "text", x = ymd("2022-01-15"), col = "black", angle = 0, y = 42000, label = "Omicron")

## trend Suansaranrom Hospital 


suan_trend <- data_long %>%
  filter(new_date >= "2020-10-01" & new_date <= "2023-06-01") %>%
  filter(Hospital == "Suansaranrom")
  
sum(suan_trend$Value, na.rm = T )

ggplot(data = suan_trend, mapping = aes(x = new_date, y = Value, fill = Value )) +
  geom_col() +
  labs(title = "Number of Visits Over Time",
       x = "Timeline",
       y = "Number of Visit") +
  theme_minimal() +
  scale_fill_gradient(low = "#DBD65C", high = "#5614B0") +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b-%y", breaks = '1 month', expand = c(0.001, 5)) +
  annotate("rect", xmin = ymd("2021-04-01"), xmax = ymd("2021-05-01"), ymin = 0, ymax = 5000, fill = "#A7A7A7", alpha = 0.2) +
  geom_rect(aes(xmin = ymd("2021-04-01"), xmax = ymd("2021-05-01"), ymin = 0, ymax = 5000), fill = NA, color = "#A7A7A7", linetype = "dashed") +
  annotate("rect", xmin = ymd("2021-06-01"), xmax = ymd("2021-08-01"), ymin = 0, ymax = 5000, fill = "#A7A7A7", alpha = 0.2) +
  geom_rect(aes(xmin = ymd("2021-06-01"), xmax = ymd("2021-08-01"), ymin = 0, ymax = 5000), fill = NA, color = "#A7A7A7", linetype = "dashed") +
  annotate("rect", xmin = ymd("2021-12-01"), xmax = ymd("2022-03-01"), ymin = 0, ymax = 5000, fill = "#A7A7A7", alpha = 0.2) +
  geom_rect(aes(xmin = ymd("2021-12-01"), xmax = ymd("2022-03-01"), ymin = 0, ymax = 5000), fill = NA, color = "#A7A7A7", linetype = "dashed") +
  annotate(geom = "text", x = ymd("2021-02-15"), col = "black", angle = 0, y = 5200, label = "Alpha wave") +
  annotate(geom = "text", x = ymd("2021-07-01"), col = "black", angle = 0, y = 5200, label = "Delta wave") +
  annotate(geom = "text", x = ymd("2022-01-15"), col = "black", angle = 0, y = 5200, label = "Omicron wave")





#-------------------------------------------------------------
## Create table for onevisit each hospital 
data_table_onevisit <- read_excel("Downloads/Data table_ts .xlsx", 
                         sheet = "onevisit")

##----- REVISE TYPE OF VARRIABLE
data_table_onevisit$year <- as.factor(data_table_onevisit$year)

data_table_onevisit <- data_table_onevisit %>%
  mutate(month_year = as.character(glue("01-{month}-{year}")))


data_table_onevisit$new_date <- dmy(data_table$month_year)

data_table_onevisit <- data_table_onevisit %>%
  mutate(day = day(new_date),
         month = month(new_date),
         year = year(new_date),
         full_month = months(new_date,abbr =F) ,
         full_month2 = months(new_date,abbreviate = T),
         week_day = weekdays(new_date,abbr=F)
  )


data_table_onevisit <- data_table_onevisit %>%
  mutate(year_month =format(data_table_onevisit$new_date, "%b-%Y"))

glimpse(data_table_onevisit)

library(tidyr)

data_table2_onevisit <- data_table_onevisit %>%
  select(new_date,NHSO,Rama,Rajavithi,`Phang-nga`,Suansaranrom)
# ใช้ tidyr::pivot_longer() เพื่อแปลงข้อมูลให้อยู่ในรูปแบบ long format
data_long_onevisit <- data_table2_onevisit %>%
  tidyr::pivot_longer(
    cols = -c(new_date),
    names_to = "Hospital",
    values_to = "Value"
  )

glimpse(data_long_onevisit)
#---------------------------------------------------------


## 1.  NHSO  nvisitOvertime - onevisit 
test1 <- data_long %>%
  filter(Hospital == "NHSO")

test2 <- data_long_onevisit %>%
  filter(Hospital == "NHSO")

test1 <- test1 %>%
  filter(new_date>"2020-11-01" & new_date < "2023-05-01")
test2 <- test2 %>%
  filter(new_date>"2020-11-01" & new_date < "2023-05-01")

# สร้างกราฟ
H0 <- ggplot() +
  # เพิ่ม bar plot สำหรับ dataset 1
  geom_bar(data = test2, aes(x = new_date, y = Value), stat = "identity", fill = "#424242", alpha = 0.5) +
  # เพิ่ม line plot สำหรับ dataset 2
  geom_line(data = test1, aes(x = new_date, y = Value, group = 1), color = "#7facd6", size = 1.5) +
  # เพิ่มป้ายกำกับแกน x และ y
  labs(x = "Month-Year", y = "Total number") +
  scale_x_date(date_labels = "%b-%Y", breaks = '1 month', expand = c(0.001, 5))+
  # เพิ่มชื่อกราฟ
  ggtitle("") +
  theme_minimal()+
  theme(axis.text = element_text(angle = 90, hjust = 1))+
  # เพิ่มป้ายกำกับสำหรับแต่ละชุดข้อมูล
  scale_fill_manual(values = c("blue"), name = "Dataset 1") +
  scale_color_manual(values = c("red"), name = "Dataset 2") + 
  scale_y_continuous(limits = c(0, 40000))


## 2. Rama Hospital nvisitOvertime - onevisit 
test1 <- data_long %>%
  filter(Hospital == "Rama")

test2 <- data_long_onevisit %>%
  filter(Hospital == "Rama")


# สร้างกราฟ
H1 <- ggplot() +
  # เพิ่ม bar plot สำหรับ dataset 1
  geom_bar(data = test2, aes(x = new_date, y = Value), stat = "identity", fill = "#424242", alpha = 0.5) +
  # เพิ่ม line plot สำหรับ dataset 2
  geom_line(data = test1, aes(x = new_date, y = Value, group = 1), color = "#f8956e",size = 1.5) +
  # เพิ่มป้ายกำกับแกน x และ y
  labs(x = "Year", y = "Number of visits") +
  # เพิ่มชื่อกราฟ
  ggtitle("Rama Hospital") +
  theme_minimal()+
  # เพิ่มป้ายกำกับสำหรับแต่ละชุดข้อมูล
  scale_fill_manual(values = c("blue"), name = "Dataset 1") +
  scale_color_manual(values = c("red"), name = "Dataset 2") +
  scale_y_continuous(limits = c(0, 40000)) 

H1.1 <- ggplot() +
  # เพิ่ม bar plot สำหรับ dataset 1
  geom_bar(data = test2, aes(x = new_date, y = Value), stat = "identity", fill = "#424242", alpha = 0.5) +
  # เพิ่ม line plot สำหรับ dataset 2
  geom_line(data = test1, aes(x = new_date, y = Value, group = 1), color = "#f8956e",size = 1.5) +
  # เพิ่มป้ายกำกับแกน x และ y
  labs(x = "Year", y = "Number of visits") +
  # เพิ่มชื่อกราฟ
  ggtitle("Rama Hospital") +
  theme_minimal()+
  # เพิ่มป้ายกำกับสำหรับแต่ละชุดข้อมูล
  scale_fill_manual(values = c("blue"), name = "Dataset 1") +
  scale_color_manual(values = c("red"), name = "Dataset 2")


glimpse(data_long_onevisit)

## 3. Rajavithi Hospital nvisitOvertime - onevisit 
test1 <- data_long %>%
  filter(Hospital == "Rajavithi")

test2 <- data_long_onevisit %>%
  filter(Hospital == "Rajavithi")


# สร้างกราฟ
H2 <- ggplot() +
  # เพิ่ม bar plot สำหรับ dataset 1
  geom_bar(data = test2, aes(x = new_date, y = Value), stat = "identity", fill = "#424242", alpha = 0.5) +
  # เพิ่ม line plot สำหรับ dataset 2
  geom_line(data = test1, aes(x = new_date, y = Value, group = 1), color = "#fec304",size = 1.5) +
  # เพิ่มป้ายกำกับแกน x และ y
  labs(x = "Year", y = "Number of visits") +
  # เพิ่มชื่อกราฟ
  ggtitle("Rajavithi Hospital") +
  theme_minimal()+
  # เพิ่มป้ายกำกับสำหรับแต่ละชุดข้อมูล
  scale_fill_manual(values = c("blue"), name = "Dataset 1") +
  scale_color_manual(values = c("red"), name = "Dataset 2") +
  scale_y_continuous(limits = c(0, 40000)) 

H2.1 <- ggplot() +
  # เพิ่ม bar plot สำหรับ dataset 1
  geom_bar(data = test2, aes(x = new_date, y = Value), stat = "identity", fill = "#424242", alpha = 0.5) +
  # เพิ่ม line plot สำหรับ dataset 2
  geom_line(data = test1, aes(x = new_date, y = Value, group = 1), color = "#fec304",size = 1.5) +
  # เพิ่มป้ายกำกับแกน x และ y
  labs(x = "Year", y = "Number of visits") +
  # เพิ่มชื่อกราฟ
  ggtitle("Rajavithi Hospital") +
  theme_minimal()+
  # เพิ่มป้ายกำกับสำหรับแต่ละชุดข้อมูล
  scale_fill_manual(values = c("blue"), name = "Dataset 1") +
  scale_color_manual(values = c("red"), name = "Dataset 2") 

## 4. Phang-nga Hospital nvisitOvertime - onevisit 
test1 <- data_long %>%
  filter(Hospital == "Phang-nga")

test2 <- data_long_onevisit %>%
  filter(Hospital == "Phang-nga")


# สร้างกราฟ
H3 <- ggplot() +
  # เพิ่ม bar plot สำหรับ dataset 1
  geom_bar(data = test2, aes(x = new_date, y = Value), stat = "identity", fill = "#424242", alpha = 0.5) +
  # เพิ่ม line plot สำหรับ dataset 2
  geom_line(data = test1, aes(x = new_date, y = Value, group = 1), color = "#c0b8da",size = 1.5) +
  # เพิ่มป้ายกำกับแกน x และ y
  labs(x = "Year", y = "Number of visits") +
  # เพิ่มชื่อกราฟ
  ggtitle("Phang-nga Hospital") +
  theme_minimal()+
  # เพิ่มป้ายกำกับสำหรับแต่ละชุดข้อมูล
  scale_fill_manual(values = c("blue"), name = "Dataset 1") +
  scale_color_manual(values = c("red"), name = "Dataset 2") +
  scale_y_continuous(limits = c(0, 40000)) 

H3.1 <- ggplot() +
  # เพิ่ม bar plot สำหรับ dataset 1
  geom_bar(data = test2, aes(x = new_date, y = Value), stat = "identity", fill = "#424242", alpha = 0.5) +
  # เพิ่ม line plot สำหรับ dataset 2
  geom_line(data = test1, aes(x = new_date, y = Value, group = 1), color = "#c0b8da",size = 1.5) +
  # เพิ่มป้ายกำกับแกน x และ y
  labs(x = "Year", y = "Number of visits") +
  # เพิ่มชื่อกราฟ
  ggtitle("Phang-nga Hospital") +
  theme_minimal()+
  # เพิ่มป้ายกำกับสำหรับแต่ละชุดข้อมูล
  scale_fill_manual(values = c("blue"), name = "Dataset 1") +
  scale_color_manual(values = c("red"), name = "Dataset 2") 

## 4. Phang-nga Hospital nvisitOvertime - onevisit 
test1 <- data_long %>%
  filter(Hospital == "Suansaranrom")

test2 <- data_long_onevisit %>%
  filter(Hospital == "Suansaranrom")

glimpse(data_long_onevisit)

# สร้างกราฟ
H4 <- ggplot() +
  # เพิ่ม bar plot สำหรับ dataset 1
  geom_bar(data = test2, aes(x = new_date, y = Value), stat = "identity", fill = "#424242", alpha = 0.5) +
  # เพิ่ม line plot สำหรับ dataset 2
  geom_line(data = test1, aes(x = new_date, y = Value, group = 1), color = "#96d7c6",size = 1.5) +
  # เพิ่มป้ายกำกับแกน x และ y
  labs(x = "Year", y = "Number of visits") +
  # เพิ่มชื่อกราฟ
  ggtitle("Suansaranrom Hospital") +
  theme_minimal()+
  # เพิ่มป้ายกำกับสำหรับแต่ละชุดข้อมูล
  scale_fill_manual(values = c("blue"), name = "Dataset 1") +
  scale_color_manual(values = c("red"), name = "Dataset 2") +
  scale_y_continuous(limits = c(0, 40000)) 

H4.1 <- ggplot() +
  # เพิ่ม bar plot สำหรับ dataset 1
  geom_bar(data = test2, aes(x = new_date, y = Value), stat = "identity", fill = "#424242", alpha = 0.5) +
  # เพิ่ม line plot สำหรับ dataset 2
  geom_line(data = test1, aes(x = new_date, y = Value, group = 1), color = "#96d7c6",size = 1.5) +
  # เพิ่มป้ายกำกับแกน x และ y
  labs(x = "Year", y = "Number of visits") +
  # เพิ่มชื่อกราฟ
  ggtitle("Suansaranrom Hospital") +
  theme_minimal()+
  # เพิ่มป้ายกำกับสำหรับแต่ละชุดข้อมูล
  scale_fill_manual(values = c("blue"), name = "Dataset 1") +
  scale_color_manual(values = c("red"), name = "Dataset 2")




all-(H0/H1/H2/H3/H4)
(H0/H1/H2/H4/H3)
H0-(H1/H2/H3/H4)
H2-H2.1
H3-H3.1
H4-H4.1

H1.1+H2.1/H3.1/H4.1


glimpse(data_long)
nhso_ <- data_long %>%
  filter(Hospital == "NHSO")

nhso_allhos <- data_long 


all2 <- ggplot(data = nhso_, mapping = aes(x = new_date, y = Value, color = Hospital, group = Hospital, fill = Hospital, shape = Hospital)) +
  geom_line(size = 1.3) +
  geom_point(size =2)+
  scale_color_manual(values = c("NHSO" = "#7facd6", "Rama" = "#f8956e", "Rajavithi" = "#fec304", "Phang-nga" = "#c0b8da", "Suansaranrom" = "#96d7c6")) +
  labs(title = "Number of Visits Over Time",
       x = "Timeline",
       y = "Number of Visits") +
  theme_minimal() +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b-%y", breaks = '1 month', expand = c(0.001, 5)) +
  annotate("rect", xmin = ymd("2021-04-01"), xmax = ymd("2021-05-01"), ymin = 0, ymax = 40000, fill = "#A7A7A7", alpha = 0.2) +
  geom_rect(aes(xmin = ymd("2021-04-01"), xmax = ymd("2021-05-01"), ymin = 0, ymax = 40000), fill = NA, color = "#A7A7A7", linetype = "dashed") +
  annotate("rect", xmin = ymd("2021-06-01"), xmax = ymd("2021-08-01"), ymin = 0, ymax = 40000, fill = "#A7A7A7", alpha = 0.2) +
  geom_rect(aes(xmin = ymd("2021-06-01"), xmax = ymd("2021-08-01"), ymin = 0, ymax = 40000), fill = NA, color = "#A7A7A7", linetype = "dashed") +
  annotate("rect", xmin = ymd("2021-12-01"), xmax = ymd("2022-03-01"), ymin = 0, ymax = 40000, fill = "#A7A7A7", alpha = 0.2) +
  geom_rect(aes(xmin = ymd("2021-12-01"), xmax = ymd("2022-03-01"), ymin = 0, ymax = 40000), fill = NA, color = "#A7A7A7", linetype = "dashed") +
  annotate(geom = "text", x = ymd("2021-02-15"), col = "black", angle = 0, y = 41000, label = "Alpha wave") +
  annotate(geom = "text", x = ymd("2021-07-01"), col = "black", angle = 0, y = 41000, label = "Delta wave") +
  annotate(geom = "text", x = ymd("2022-01-15"), col = "black", angle = 0, y = 41000, label = "Omicron wave") + 
  theme(legend.position = "none")

all3 <- ggplot(data = nhso_allhos, mapping = aes(x = new_date, y = Value, color = Hospital, group = Hospital, fill = Hospital, shape = Hospital)) +
  geom_line(size = 1.3) +
  geom_point(size =2)+
  scale_color_manual(values = c("NHSO" = "#7facd6", "Rama" = "#f8956e", "Rajavithi" = "#fec304", "Phang-nga" = "#c0b8da", "Suansaranrom" = "#96d7c6")) +
  labs(title = "Number of Visits Over Time",
       x = "Timeline",
       y = "Number of Visits") +
  theme_minimal() +
  theme(axis.text = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b-%y", breaks = '1 month', expand = c(0.001, 5)) +
  annotate("rect", xmin = ymd("2021-04-01"), xmax = ymd("2021-05-01"), ymin = 0, ymax = 40000, fill = "#A7A7A7", alpha = 0.2) +
  geom_rect(aes(xmin = ymd("2021-04-01"), xmax = ymd("2021-05-01"), ymin = 0, ymax = 40000), fill = NA, color = "#A7A7A7", linetype = "dashed") +
  annotate("rect", xmin = ymd("2021-06-01"), xmax = ymd("2021-08-01"), ymin = 0, ymax = 40000, fill = "#A7A7A7", alpha = 0.2) +
  geom_rect(aes(xmin = ymd("2021-06-01"), xmax = ymd("2021-08-01"), ymin = 0, ymax = 40000), fill = NA, color = "#A7A7A7", linetype = "dashed") +
  annotate("rect", xmin = ymd("2021-12-01"), xmax = ymd("2022-03-01"), ymin = 0, ymax = 40000, fill = "#A7A7A7", alpha = 0.2) +
  geom_rect(aes(xmin = ymd("2021-12-01"), xmax = ymd("2022-03-01"), ymin = 0, ymax = 40000), fill = NA, color = "#A7A7A7", linetype = "dashed") +
  annotate(geom = "text", x = ymd("2021-02-15"), col = "black", angle = 0, y = 41000, label = "Alpha wave") +
  annotate(geom = "text", x = ymd("2021-07-01"), col = "black", angle = 0, y = 41000, label = "Delta wave") +
  annotate(geom = "text", x = ymd("2022-01-15"), col = "black", angle = 0, y = 41000, label = "Omicron wave")+ 
  theme(legend.position = "none")


