

test <- unique(tele$pid,tele$sex)

summary(test)

er <- data.frame(pid = tele$pid,sex = tele$sex)
er_unique <- unique(er)
summary(er_unique)
summary(duplicated(er_unique$pid))
list_dup  <- data.frame(duplicated(er_$pid))


data <- er_dup
data <- data %>%
  mutate(list_dup = list_dup)

data <- data %>%
  mutate(unique_id = group_indices(., pid))


check <- data %>%
  group_by(unique_id) %>%
  count()

list <- check %>%
  filter(n>1)

list <- list$unique_id

data <- data %>%
  mutate(aa = case_when(
    unique_id %in%  list ~ 1,
    TRUE ~ 0
  ))

list_sex_missing <- data %>%
  filter(aa == 1) 

class(list_sex_missing$pid)


list_pid_missing_sex <- list_sex_missing$pid


tele <- tele %>% 
  mutate(missing_sex = case_when(
    pid %in% list_pid_missing_sex ~ 1,
    TRUE ~ 0
  ))

tele %>%
  group_by(missing_sex) %>%
  count()

list_pid <- tele %>%
  filter(missing_sex == 1) 
  
write_csv(list_pid,"list_pid_sex_missing.csv")




table <- list_pid %>%
  group_by(pid) %>%
  count(sex)
write_csv(table,"table_count_sex_missing.csv")
