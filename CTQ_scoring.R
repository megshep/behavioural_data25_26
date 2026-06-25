library(dplyr)
library(openxlsx)

df <- read.xlsx('CTQ_scores.xlsx')

# Define the reverse coding function
#as the values are already numbers, we can use as.character rather than as.numeric here
reverse_code <- function(x) {
  recode_vals <- c(`1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1)
  return(recode_vals[as.character(x)])
}

# Vector of column names to reverse
#these are the ones that are specified in the CTQ scoring document
cols_to_reverse <- c('CTQ_5', 'CTQ_7', 'CTQ_13', 'CTQ_19', 'CTQ_28', 'CTQ_2', 'CTQ_26')

#this creates new columns with the reverse-coded variables and changes their name to add an r into it e.g., CTQ_5r
for (col in cols_to_reverse) {
  df[[paste0(col, "r")]] <- reverse_code(df[[col]])
}

# delete the original columns for those that needed to be reverse-coded
df <- df[ , !(names(df) %in% cols_to_reverse)]

#add a column titled physical abuse and sum the necessary rows (repeat for all variables)
#this information is taken from the CTQ document (which items correspond to which subscales)
#na.rm=TRUE ignores any NA values and still totals it if it has scores in other columns
df <- df %>%
  mutate(emotional_abuse = rowSums(across(c(CTQ_3, CTQ_8, CTQ_14, CTQ_18, CTQ_25)), na.rm = TRUE))

df <- df %>%
  mutate(physical_abuse = rowSums(across(c(CTQ_9, CTQ_11, CTQ_12, CTQ_15, CTQ_17)), na.rm = TRUE))

df <- df %>%
  mutate(sexual_abuse = rowSums(across(c(CTQ_20, CTQ_21, CTQ_23, CTQ_24, CTQ_27)), na.rm = TRUE))

df <- df %>%
  mutate(emotional_neglect = rowSums(across(c(CTQ_5r, CTQ_7r, CTQ_13r, CTQ_19r, CTQ_28r)), na.rm = TRUE))

df <- df %>%
  mutate(physical_neglect = rowSums(across(c(CTQ_1, CTQ_2r, CTQ_4, CTQ_6, CTQ_26r)), na.rm = TRUE))

df <- df %>%
  mutate(minimisation_denial = rowSums(across(c(CTQ_10, CTQ_16, CTQ_22)), na.rm = TRUE))

#add a column to total all CTQ scores across all subscales calculated
df <- df %>%
  mutate(CTQ_score = rowSums(across(c(physical_abuse, emotional_abuse, sexual_abuse, emotional_neglect, physical_neglect)), na.rm = TRUE))

#produce a csv
write.xlsx(df, "CTQ_scored.csv", rowNames = FALSE)
