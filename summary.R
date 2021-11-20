library(tidyverse)

unemployment <- read.csv("https://raw.githubusercontent.com/info201b-2021-aut/final-project-ydodobara/main/unemployment_data_us.csv?token=AV5HNBPVHJJEXHUCZUTMVOTBT3GAK")

summary_info <- list()

# Average unemployment rate for White people in 2020
summary_info$avg_rate_white_2020 <- unemployment %>%
  filter(Year == "2020") %>%
  summarize(avg_rate = mean(White, na.rm = TRUE)) %>%
  pull(avg_rate)

# Average unemployment rate for Black people in 2020
summary_info$avg_rate_black_2020 <- unemployment %>%
  filter(Year == "2020") %>%
  summarize(avg_rate = mean(Black, na.rm = TRUE)) %>%
  pull(avg_rate)

# Average unemployment rate for Asian people in 2020
summary_info$avg_rate_asian_2020 <- unemployment %>%
  filter(Year == "2020") %>%
  summarize(avg_rate = mean(Asian, na.rm = TRUE)) %>%
  pull(avg_rate)

# Average unemployment rate for Hispanic people in 2020
summary_info$avg_rate_hispanic_2020 <- unemployment %>%
  filter(Year == "2020") %>%
  summarize(avg_rate = mean(Hispanic, na.rm = TRUE)) %>%
  pull(avg_rate)

# Month-Year with highest unemployment rate for White people
summary_info$highest_rate_white <- unemployment %>%
  filter(White == max(White, na.rm = TRUE)) %>%
  pull(Date)

# Month-Year with highest unemployment rate for Black people
summary_info$highest_rate_black <- unemployment %>%
  filter(Black == max(Black, na.rm = TRUE)) %>%
  pull(Date)

# Month-Year with highest unemployment rate for Asian people
summary_info$highest_rate_asian <- unemployment %>%
  filter(Asian == max(Asian, na.rm = TRUE)) %>%
  pull(Date)
  
# Month-Year with highest unemployment rate for Hispanic people
summary_info$highest_rate_hispanic <- unemployment %>%
  filter(Hispanic == max(Hispanic, na.rm = TRUE)) %>%
  pull(Date)

