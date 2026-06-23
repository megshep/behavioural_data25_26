library(tidyr)
library(dplyr)
library(openxlsx)

df <- read.xlsx('participant_data.xlsx')

#check whether all of the columns are numeric, any missing data can change it to character
sapply(df[, paste0("HR", 1:8)], class)

#this will ensure that all of the columns are numeric if we need to change them
df[, paste0("HR", 1:8)] <- lapply(df[, paste0("HR", 1:8)], as.numeric)

#check that this has worked!
sapply(df[, paste0("HR", 1:8)], class)

#this moves the data from wide format to long format
df_long <- df %>% pivot_longer(cols = HR1:HR8 , names_to = "time", values_to ='heart_rate')

#removes HR from the cell, therefore, makes it numeric only and ready to input into my model
df_long <- df_long %>% mutate(time = as.numeric(gsub("HR", "", time)))

write.xlsx(df_long, "HR_long.xlsx")
