tele %>%
  select(pid,new_date)


# gen variable count service_days from new_datelast - new_date first 
test <- tele %>%
  group_by(pid) %>%
  mutate(service_days = max(new_date) - min(new_date) + 1) %>%
  ungroup()
#gen variable for filter patient use telemedicine more than 1 year or 365 days
test <- test %>%
  mutate(cri = case_when(
    service_days >=365 ~ "more than a year" ,
    TRUE ~ "less than a year"
  ))

# select variable for check 
test2 <- test %>%
  select(pid,unique_id,unique_var,NHSO_policy_des,new_date,service_days,cri)

test3  <- test2 %>%
  filter(unique_var == 1)

# table patients uses telemedicine between less than a year - more than a year.
table(test3$NHSO_policy_des,test3$cri)
#               less than a year more than a year
# Asthma                    1093              204
# Cancers                   5571              779
# Diabetes                 15454              715
# Hypertension             21849              772
# Mental health            17317             4394
# Other                    38083             3922

# ตัวอย่างข้อมูล
data <- data.frame(
  group = rep(c("A", "B", "C"), each = 100),
  value = c(rnorm(100), rnorm(100, mean = 2), rnorm(100, mean = -2))
)

data <- tele %>%
  group_by(pid,NHSO_policy_des) %>%
  count()

# สร้างกราฟ boxplot หลายกล่อง
boxplot <- ggplot(data, aes(x = NHSO_policy_des, y = n, fill = NHSO_policy_des)) +
  geom_boxplot() +
  theme_minimal()

boxplot

glimpse(tele)

province <- tele%>%
  group_by(province_name,zone) %>%
  count() %>%
  arrange(zone) %>%
  arrange(desc(n))
  

