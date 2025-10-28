

data <- readRDS("~/WP9_S2_PL_FOREIGN_TOURISTS/synthetic data/WP9_S2_Tourism_synt.rds")


# D8R1 - number of tourists
# DRR2 - number of nights provided

# KKR- code of country for foreign tourists



# select only 5 countries with the highest number of tourist

library(dplyr)
top5 <- data %>%
  select(KKR, D8R1) %>%
  group_by(KKR) %>%
  summarise(number_of_tourist = sum(D8R1, na.rm = TRUE)) %>%
  arrange(desc(number_of_tourist)) %>% head(5)

list_of_countries=unique(top5$KKR)

# data for 10 countries
tourists <- data %>%
  filter(KKR %in% list_of_countries)

write.csv(tourists, 'synthetic data/tourists.csv')


# select only 5 countries with the highest number of night spend

top5 <- data %>%
  select(KKR, D8R2) %>%
  group_by(KKR) %>%
  summarise(number_of_tourist = sum(D8R2, na.rm = TRUE)) %>%
  arrange(desc(number_of_tourist)) %>% head(5)

list_of_countries=unique(top5$KKR)

# data for 5 countries
nights <- data %>%
  filter(KKR %in% list_of_countries)

write.csv(nights, 'synthetic data/nights.csv')