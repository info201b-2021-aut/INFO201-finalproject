install.packages()
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

unemployment_data <- read.csv("https://raw.githubusercontent.com/info201b-2021-aut/final-project-ydodobara/main/unemployment_data_us.csv?token=AV5GDCQ6NSCEO6CMDSCIE7TBT24CQ")
View(unemployment_data)

#Chart 1: Visualization of AVerage Unemployment Rate by Race per year 

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

View(race_unemployment)
View(unemployment_data)
View(race_unemployment_table)
race_unemployment_plot

#Chart 2: Visualization of Average Unemployment Rate by Academic
degree_unemployment <- unemployment_data %>% 
  select(Year, Primary_School, High_School, Associates_Degree, Professional_Degree) 
View(degree_unemployment)

degree_unemployment_table <- degree_unemployment %>%
  group_by(Year) %>%
  summarize(Primary = mean(Primary_School, na.rm = TRUE), High = mean(High_School, na.rm = TRUE), Associates = mean(Associates_Degree, na.rm = TRUE),  Professional = mean(Professional_Degree, na.rm = TRUE)) %>%
  select(Year, Primary, High, Associates, Professional)

View(degree_unemployment_table)

degree_unemployment_plot <- ggplot(degree_unemployment_table, aes(x=Year),colour=variable) + 
  geom_line(aes(y = Primary), color = "darkred") + 
  geom_line(aes(y = High), color="steelblue") + 
  geom_line(aes(y = Associates), color = "yellow") +
  geom_line(aes(y= Professional), color = "black") +
  labs(x ="Year", y = "Unemployment Rate", title = "Average Unemployment Rate by Degree per Year")

degree_unemployment_plot
