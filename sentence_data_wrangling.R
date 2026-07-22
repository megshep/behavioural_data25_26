# packages
library(dplyr)
library(tidyr)

#reading in data and renaming according to task-type
semantic_ps <- read.csv("sentences_x_downloaded.csv")

#deleting all rows that contain data from the fixation screen
semantic_exp <- semantic_ps[!grepl("fixation", semantic_ps$Zone.Name),]

#deleting columns that aren't useful
semantic_exp <- semantic_exp[-c(1:11, 13:14, 16:36, 39:40, 43:47, 49:50) ]

#removing any row that doesn't have any info in the stress column
#this means we will only be left with the 40 trials for each person
semantic_exp <- semantic_exp[semantic_exp$Stress != "", ]

#to check the spreadsheet 
write.csv(semantic_exp, file = "semantic_x_colrem.csv")

write_xlsx(df_wide, "df_wide.xlsx")
