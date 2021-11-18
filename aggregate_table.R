library(tidyverse)

unemployment <- read.csv("https://raw.githubusercontent.com/info201b-2021-aut/final-project-ydodobara/main/unemployment_data_us.csv?token=AV5HNBNQS2G3EMLZ3X2UWQLBT3JP2")

unemployment_by_race <- unemployment %>%
  group_by(Date) %>%
  select(Date, White, Black, Asian, Hispanic)