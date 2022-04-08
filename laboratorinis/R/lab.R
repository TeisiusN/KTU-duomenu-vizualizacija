# 15 variantas, ecoActCode: 522920

library(readr)
library(tidyverse)
library(dplyr)

data = read.csv("../data/lab_sodra.csv", fileEncoding = "UTF-8")
data

summary(data)
str(data)
# 1 užduotis
d1 = data %>%
  filter(ecoActCode == 522920)
d1 %>%
  ggplot(aes(x=avgWage)) +
  theme_light() +
  geom_histogram(fill = "blue", col = "black", bins = 100) +
  labs(title = "Average wage of employes")

# 2 užduotis
d1 = d1 %>%
  mutate(month_value=as.integer(substr(month, 5 ,7)))
top5 = d1 %>%
  group_by(name) %>% 
  slice_max(avgWage, n=1) %>% 
  ungroup() %>%
  top_n(avgWage, n=5) %>% 
  select(name)

d2 = d1 %>%
  filter(name %in% top5$name)
d2 %>%
  ggplot(aes(x = month_value, y = avgWage, group = name)) +
  theme_bw() +
  geom_point(aes(colour = name)) +
  scale_x_continuous("month",breaks=1:12,limits=c(1,12)) + 
  scale_y_continuous(labels = scales::comma) + 
  geom_line(aes(colour = name)) +
  scale_color_manual(values = c("tomato4", "blue", "limegreen", "violetred2", "purple3"))
  labs(title = "Average wage of employes", x = "Month", y = "Average wage")
  
# 3 užduotis
d2 %>%
  group_by(name) %>%
  slice_max(numInsured, with_ties = FALSE) %>%
  head(5) %>%
  ggplot(aes(x = reorder(name, -numInsured), y = numInsured)) +
  geom_col(aes(fill = name)) +
  theme_light() +
  labs(title = "Number of insured employees", x = "Company", y = "Count") +
  theme(axis.text.x = element_text(size = 6))
