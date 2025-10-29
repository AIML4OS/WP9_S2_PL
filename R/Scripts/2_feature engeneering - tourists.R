library(readr)
library(dplyr)
library(lubridate)
library(zoo) # for rolling means

# --- Load data ---
data <- read_csv("synthetic data/tourists.csv")

# Remove unnecessary columns
data$...1 <- NULL
data$D8R2 <- NULL

# --- Create date and time variables ---
data <- data %>%
  mutate(
    year = as.integer(substr(OKRES, 1, 4)),
    month = as.integer(substr(OKRES, 5, 6)),
    date = make_date(year = year, month = month, day = 1),
    quarter = quarter(date, with_year = TRUE)
  )

# --- Monthly averages for each administrative level ---
# Municipality (GMN)
gmina_avg <- data %>%
  group_by(GMN, date) %>%
  summarise(D8R1_mean_gmn = mean(D8R1, na.rm = TRUE), .groups = "drop")

# County (POW)
powiat_avg <- data %>%
  group_by(POW, date) %>%
  summarise(D8R1_mean_pow = mean(D8R1, na.rm = TRUE), .groups = "drop")

# Voivodeship (WON)
woj_avg <- data %>%
  group_by(WON, date) %>%
  summarise(D8R1_mean_won = mean(D8R1, na.rm = TRUE), .groups = "drop")

# --- Quarterly average ---
quarter_avg <- data %>%
  group_by(quarter) %>%
  summarise(D8R1_quarter_mean = mean(D8R1, na.rm = TRUE), .groups = "drop")

# --- Rolling (moving) averages for the last 3, 6, 9, and 12 months ---
rolling_avg <- data %>%
  arrange(GMN, date) %>%
  group_by(GMN) %>%
  mutate(
    mean_3m  = rollmean(D8R1, k = 3, align = "right", fill = NA),
    mean_6m  = rollmean(D8R1, k = 6, align = "right", fill = NA),
    mean_9m  = rollmean(D8R1, k = 9, align = "right", fill = NA),
    mean_12m = rollmean(D8R1, k = 12, align = "right", fill = NA)
  ) %>%
  ungroup()

# --- Ensure unique rows for REGON + date to prevent many-to-many join ---
rolling_avg_unique <- rolling_avg %>%
  group_by(REGON, date) %>%
  summarise(
    mean_3m  = mean(mean_3m, na.rm = TRUE),
    mean_6m  = mean(mean_6m, na.rm = TRUE),
    mean_9m  = mean(mean_9m, na.rm = TRUE),
    mean_12m = mean(mean_12m, na.rm = TRUE),
    .groups = "drop"
  )

# --- Combine everything into one dataset safely ---
data_final <- data %>%
  left_join(gmina_avg, by = c("GMN", "date")) %>%
  left_join(powiat_avg, by = c("POW", "date")) %>%
  left_join(woj_avg, by = c("WON", "date")) %>%
  left_join(quarter_avg, by = "quarter") %>%
  left_join(rolling_avg_unique, by = c("REGON", "date"))

data_final <- data_final %>%
  arrange(REGON,KKR ,date) %>%  # 
  group_by(REGON,KKR) %>%       
  mutate(
    lag_1  = lag(D8R1, n = 1),
    lag_2  = lag(D8R1, n = 2),
    lag_3  = lag(D8R1, n = 3),
    lag_4  = lag(D8R1, n = 4),
    lag_5  = lag(D8R1, n = 5),
    lag_6  = lag(D8R1, n = 6),
    lag_7  = lag(D8R1, n = 7),
    lag_8  = lag(D8R1, n = 8),
    lag_9  = lag(D8R1, n = 9),
    lag_10 = lag(D8R1, n = 10),
    lag_11 = lag(D8R1, n = 11),
    lag_12 = lag(D8R1, n = 12)
  ) %>%
  ungroup()

write.csv(data_final,'synthetic data/tourists_features.csv')
