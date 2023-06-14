
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

# # Load dataset from github
# data <- babynames %>% 
#   filter(name %in% c("Mary","Emma", "Ida", "Ashley", "Amanda", "Jessica",    "Patricia", "Linda", "Deborah",   "Dorothy", "Betty", "Helen")) %>%
#   filter(sex=="F")
# 
# 
# data %>%
#   mutate( highlight=ifelse(name=="Amanda", "Amanda", "Other")) %>%
#   ggplot( aes(x=year, y=n, group=name, color=highlight, size=highlight)) +
#   geom_line() +
#   scale_color_manual(values = c("#69b3a2", "lightgrey")) +
#   scale_size_manual(values=c(1.5,0.2)) +
#   theme(legend.position="none") +
#   ggtitle("Popularity of American names in the previous 30 years") +
#   theme_ipsum() +
#   geom_label( x=1990, y=55000, label="Amanda reached 3550\nbabies in 1970", size=4, color="#69b3a2") +
#   theme(
#     legend.position="none",
#     plot.title = element_text(size=14)
#   )



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



















# libraries:
library(ggplot2)
library(gganimate)
library(babynames)
library(hrbrthemes)

# Keep only 3 names
don <- babynames %>% 
  filter(name %in% c("Ashley", "Patricia", "Helen")) %>%
  filter(sex=="F")

# Plot
test <- don %>%
  ggplot( aes(x=year, y=n, group=name, color=name)) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Popularity of American names in the previous 30 years") +
  theme_ipsum() +
  ylab("Number of babies born") +
  transition_reveal(year)

animate(test)

# Save at gif:
anim_save("287-smooth-animation-with-tweenr.gif", animation = test)



policy %>%
  group_by(NHSO_policy_des)%>%
  count()


policy$year_month <- as.Date(tele$new_date, format = "%Y-%m")

test <- policy %>%
  ggplot( aes(x=year_month, y=n, group=NHSO_policy_des, color=NHSO_policy_des)) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Trend telemedicine service") +
  theme_minimal() +
  ylab("Number of service") +
  transition_reveal(round(year_month))

animate(test)

# Save at gif:
anim_save("gif_disease.gif", animation = test)









# Convert year_month to a numeric format
policy <- policy %>%
  mutate(year_month_num = as.numeric(format(year_month, "%Y")) + as.numeric(format(year_month, "%m"))/12)

# Create the animation
test <- policy %>%
  ggplot(aes(x = year_month_num, y = n, group = NHSO_policy_des, color = NHSO_policy_des)) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Trend telemedicine service") +
  theme_minimal() +
  ylab("Number of service") +
  transition_reveal(year_month_num)

# Save the animation
anim_save("policy_animation.gif", test)





policy3 <- tele %>%
  group_by(NHSO_policy,NHSO_policy_des,year,new_date) %>%
  count()

policy3 <- policy3 %>%
  mutate(year_month = as.integer(glue("{year(new_date)}{month(new_date)}")))

policy3 <- policy3 %>%
  group_by(new_date) %>%
  mutate(day_count = seq_along(policy3$new_date)) %>%
  ungroup()
policy3 %>%
  group_by(new_date)


test <- policy3 %>%
  ggplot(aes(x = day, y = n, group = NHSO_policy_des, color = NHSO_policy_des)) +
  geom_line(size = 1.5) +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Trend telemedicine service") +
  theme_minimal() +
  ylab("Number of service") +
  transition_reveal(day)


# Save at gif:
anim_save("gif_disease.gif", animation = test)












# count_frequency_of_users


visit_by_pid <- tele %>%
  group_by(pid) %>%
  count()

visit_by_pid <- data.frame(visit_by_pid)

summarise_visit_pid <- visit_by_pid %>%
  summarise(
    min = min(n),
    max = max(n),
    mean = mean(n),
    sd = sd(n),
    median = median(n),
    q1 = quantile(n,0.25),
    q2 = quantile(n,0.50),
    q3 = quantile(n,0.75),
    iqr = IQR(n),
    mode = Modes(visit_by_pid$n))

# ggplot(data = cc ,mapping =  aes(x = pid ,y = n )) +
#   geom_point()

ggplot(data = visit_by_pid, mapping =  aes( y = n )) +
  geom_boxplot()



test  <- head(tele,6) %>%
  mutate(unique_var_pid_pdx = ifelse(duplicated(pid,NHSO_policy_des), 0, 1))%>%
  arrange(unique_id)


test2 <- test %>%
  select(pid,unique_id,pdx,NHSO_policy_des,unique_var_pid_pdx)



# unique patient by disease
test <- tele %>%
  group_by(pid, NHSO_policy_des) %>%
  mutate(unique_var_pid_pdx = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup()


test2 <- test %>%
  select(pid,unique_id,NHSO_policy_des,unique_var_pid_pdx)

test3 <- test2 %>%
  group_by(unique_id) %>%
  count(unique_var_pid_pdx)

test4 <- test3 %>%
  filter(unique_var_pid_pdx == 1 , n >1 )

test5 <- test2 %>%
  filter(unique_var_pid_pdx == 1) %>%
  group_by(NHSO_policy_des) %>%
  count() 


sum(test5$n)


glimpse(test)

test %>%
  filter(unique_var_pid_pdx == 1) %>%
  group_by(NHSO_policy_des) %>%
  summarise(male = sum(gender))
  



