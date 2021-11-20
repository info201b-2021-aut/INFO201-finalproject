library(tidyverse)
library(dplyr)
library(ggplot2)

unemployment_data <- read.csv("https://raw.githubusercontent.com/info201b-2021-aut/final-project-ydodobara/main/unemployment_data_us.csv?token=AV5GDCQ6NSCEO6CMDSCIE7TBT24CQ")

race_unemployment <- unemployment_data %>%
  select(Year, White, Black, Asian, Hispanic)

race_unemployment_table <- race_unemployment %>%
  group_by(Year) %>%
  summarize(White = mean(White, na.rm = TRUE), Black = mean(Black, na.rm = TRUE), Asian = mean(Asian, na.rm = TRUE), Hispanic = mean(Hispanic, na.rm = TRUE)) %>%
  select(Year, White, Black, Asian, Hispanic)

race_unemployment_table <- pivot_longer(race_unemployment_table, White : Hispanic, names_to = "Race", values_to = "unemployment_rate")

race_unemployment_plot <- ggplot(race_unemployment_table) +
  geom_line(mapping = aes(x = Year, y = unemployment_rate, group = Race, color = Race)) +
  labs(x = "Year",
       y = "Unemployment Rate",
       title = "Average Unemployment Rate By Race Per Year")
