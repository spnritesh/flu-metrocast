# Clean list
rm(list = ls())

# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, scoringutils, readr, fs, gt)

# Load data
true_data <- read_csv("target-data/latest-data.csv")
dec_week_1 <- read_csv("model-output/epiENGAGE-ensemble_mean/2025-12-06-epiENGAGE-ensemble_mean.csv")
dec_week_2 <- read_csv("model-output/epiENGAGE-ensemble_mean/2025-12-13-epiENGAGE-ensemble_mean.csv")
dec_week_3 <- read_csv("model-output/epiENGAGE-ensemble_mean/2025-12-20-epiENGAGE-ensemble_mean.csv")
dec_week_4 <- read_csv("model-output/epiENGAGE-ensemble_mean/2025-12-27-epiENGAGE-ensemble_mean.csv")

# MN list
mn_list <- c("minneapolis", "st-cloud", "st-paul", "rochester", "duluth", "minnesota")

# Clean true_data
clean_truth <- true_data %>%
  filter(location %in% mn_list) %>%
  filter(between(target_end_date, as.Date("2025-12-01"), as.Date("2025-12-31"))) %>%
  select(location, target_end_date, observation)

# Clean ensemble_mean_data
ensemble_mean_data <- bind_rows(dec_week_1, dec_week_2, dec_week_3, dec_week_4) %>%
  filter(location %in% mn_list) %>%
  mutate(output_type_id = as.numeric(output_type_id))

# Join data
scoring_df <- ensemble_mean_data %>%
  left_join(clean_truth, by = c("location", "target_end_date")) %>%
  filter(!is.na(observation))

# Calculate scores
forecast_obj <- scoring_df %>%
  as_forecast_quantile(
    forecast_unit = c("location", "target_end_date", "reference_date", "target", "horizon"),
    observed = "observation",
    predicted = "value",
    quantile_level = "output_type_id"
  )

scores <- score(forecast_obj)

# Table prep
wis_results <- scores %>%
  summarise_scores(by = c("location", "target_end_date")) %>%
  select(location, target_end_date, wis_value = any_of(c("wis", "interval_score")))

# Create table
wis_table <- wis_results %>%
  arrange(location, target_end_date) %>%
  gt() %>%
  # Title
  tab_header(
    title = "Weighted Interval Scores (WIS)",
    subtitle = "Minnesota Locations - December 2025"
  ) %>%
  # Column rename
  cols_label(
    location = "Location",
    target_end_date = "Target Date",
    wis_value = "WIS Score"
  ) %>%
  fmt_number(
    columns = "wis_value",
    decimals = 2
  ) %>%
  opt_row_striping() %>%
  tab_options(
    table.font.size = 14,
    heading.title.font.size = 18
  )

# Display the table
print(wis_table)










