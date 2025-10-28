library(readr)
data <- read_csv("synthetic data/nights.csv")


data$...1<-NULL
data$D8R1<-NULL

library(dplyr)
library(lubridate)
library(zoo) # for rolling means

# --- Create date and time variables ---
data <- data %>%
  mutate(
    year = as.integer(substr(OKRES, 1, 4)),
    month = as.integer(substr(OKRES, 5, 6)),
    date = make_date(year = year, month = month, day = 1),
    quarter = quarter(date, with_year = TRUE)
  )

# --- Monthly averages for each level ---
# Municipality (GMN)
gmina_avg <- data %>%
  group_by(GMN, date) %>%
  summarise(D8R2_mean_gmn = mean(D8R2, na.rm = TRUE), .groups = "drop")

# County (POW)
powiat_avg <- data %>%
  group_by(POW, date) %>%
  summarise(D8R2_mean_pow = mean(D8R2, na.rm = TRUE), .groups = "drop")

# Voivodeship (WON)
woj_avg <- data %>%
  group_by(WON, date) %>%
  summarise(D8R2_mean_won = mean(D8R2, na.rm = TRUE), .groups = "drop")

# --- Quarterly average ---
quarter_avg <- data %>%
  group_by(quarter) %>%
  summarise(D8R2_quarter_mean = mean(D8R2, na.rm = TRUE), .groups = "drop")

# --- Rolling (moving) averages for the last 3, 6, 9, and 12 months ---
rolling_avg <- data %>%
  arrange(GMN, date) %>%
  group_by(GMN) %>%
  mutate(
    mean_3m  = rollmean(D8R2, k = 3, align = "right", fill = NA),
    mean_6m  = rollmean(D8R2, k = 6, align = "right", fill = NA),
    mean_9m  = rollmean(D8R2, k = 9, align = "right", fill = NA),
    mean_12m = rollmean(D8R2, k = 12, align = "right", fill = NA)
  ) %>%
  ungroup()

# Ensure rolling_avg has exactly one row per REGON + date to avoid many-to-many join
rolling_avg_unique <- rolling_avg %>%
  select(REGON, date, mean_3m, mean_6m, mean_9m, mean_12m) %>%
  distinct(REGON, date, .keep_all = TRUE)

# Join all aggregated features to the main dataset
data_final <- data %>%
  # Join municipality-level averages
  left_join(gmina_avg, by = c("GMN", "date")) %>%
  # Join county-level averages
  left_join(powiat_avg, by = c("POW", "date")) %>%
  # Join voivodeship-level averages
  left_join(woj_avg, by = c("WON", "date")) %>%
  # Join quarterly averages
  left_join(quarter_avg, by = "quarter") %>%
  # Join rolling (moving) averages, using the cleaned unique dataset
  left_join(rolling_avg_unique, by = c("REGON", "date"))


write.csv(data_final,'synthetic data/nights_features.csv')
