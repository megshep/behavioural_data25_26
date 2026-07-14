library(dplyr)
library(tidyr)
library(openxlsx)

df <- read.xlsx("all_data_final.xlsx")

ps_long <- df %>%
  pivot_longer(
    cols = c(day1_ps, day2_ps),
    names_to = "day",
    values_to = "processing_speed"
  ) %>%
  mutate(day = as.numeric(gsub("day|_ps", "", day)))

write.xlsx(ps_long, "processing_speed_data.xlsx")
