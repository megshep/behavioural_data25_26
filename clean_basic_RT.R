# packages ----
library(dplyr)

#Set working directory
setwd("/Users/megsheppard/Desktop/processing_speed")

#reading in data and renaming 
basic_rt <- read.csv("process_speed_y.csv")

#cleaning BRT task
#deleting all rows that aren't experimental trials
basic_rt_exp <- basic_rt[grep("Experimental trial", basic_rt$display),]

#deleting all rows that contain data from the fixation screen
basic_rt_exp <- basic_rt_exp[!grepl("fixation", basic_rt_exp$Zone.Name),]

#deleting columns that aren't useful
basic_rt_exp <- basic_rt_exp[-c(1:11, 13:14, 16:36, 44:45, 49:50) ]

#correcting for timed out trials, where they were coded as incorrect to NA
basic_rt_exp$Incorrect[basic_rt_exp$Timed.Out == 1] <- NA
basic_rt_exp$Correct[basic_rt_exp$Timed.Out == 1] <- NA
basic_rt_exp$Reaction.Time[basic_rt_exp$Timed.Out == 1] <- NA

#creating a correct rejection column - ppt doesn't respond to incorrect stimuli
basic_rt_exp <- mutate(basic_rt_exp, correct_rejection = ifelse(Timed.Out == 1 & is.na(Response), 1, 0))

#creating a correct hits column - ppt responds to correct stimuli
basic_rt_exp <- basic_rt_exp %>% rename(correct_hits = "Correct")
basic_rt_exp$correct_hits <- ifelse(is.na(basic_rt_exp$correct_hits), 0, basic_rt_exp$correct_hits)

#creating a false alarm column - ppt responds to incorrect stimuli
basic_rt_exp <- basic_rt_exp %>% rename(false_alarm = "Incorrect")
basic_rt_exp$false_alarm <- ifelse(is.na(basic_rt_exp$false_alarm), 0, basic_rt_exp$false_alarm)

#creating a miss column - ppt doesn't respond to correct stimuli
basic_rt_exp <- mutate(basic_rt_exp, miss = ifelse(is.na(Response) & Letters == "H.png", 1, 0))

#creating a total correct hits, false alarms, misses and correct rejection sums and average reaction time
basic_rt_sums <- basic_rt_exp %>% group_by(Participant.Public.ID) %>% 
  summarise(brt_mean_ps = mean(Reaction.Time, na.rm = TRUE), 
            brt_total_correct_hits = sum(correct_hits, na.rm = TRUE),
            brt_total_false_alarms = sum(false_alarm, na.rm = TRUE),
            brt_total_correct_rej = sum(correct_rejection, na.rm = TRUE),
            brt_total_miss = sum(miss, na.rm = TRUE))

#changing file path to save outputs
setwd("/Users/megsheppard/Desktop/processing_speed")

#exporting tables
#"-_ps" = full data downloaded from gorilla
write.csv(basic_rt, file = "basic_rt_full_data_y.csv")

#"-_exp" = relevant experimental data - mainly used to double check whether 'correct' 
#answers were actually correct and whether or not participants completed the tasks, 
#main cleaning edits done here too - i.e. RT data deleted where screen has timed out 
#this is trial-by-trial data, as ppt spans multiple rows
write.csv(basic_rt_exp, file = "basic_rt_clean_trials_y.csv")

#"-_sums" = one participant per row, contains trial averages and totals for each ppt. 
write.csv(basic_rt_sums, file = "basic_rt_ppt_avgs_y.csv")

