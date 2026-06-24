library(lmerTest)
library(openxlsx)
library(ggplot2)
library(dplyr)

df <- read.xlsx('HR_long.xlsx')

#proof of principle model - do people differ in overall stress levels?
#this is not focused on over time..
model1 <- lmer(heart_rate ~ 1 + (1 | Subject_ID), data = df)
summary(model1)

#now I want to see if there are differences in the phases (anticipatory stress, reactivity, pre/post task, recovery)
#split the dataset into separate groups for each timepoint
#calculates what a typical participant looks like at each mmoment during the task
#this is what we use to visualise the typical HR response
df_summary <- df %>% group_by(time) %>% 
  summarise(mean_hr = mean(heart_rate, na.rm = TRUE),
            se = sd(heart_rate, na.rm = TRUE)/sqrt(n()))

#one way to visualise this
ggplot(df_summary, aes(x = time, y = mean_hr, group = 1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_hr - se, ymax = mean_hr + se), width = 0.2) +
  theme_minimal()

#more detailed, with labels
df$phase <- factor(df$time,
                        levels = 1:8,
                        labels = c("Baseline",
                                   "Anticipation_Panel",
                                   "Anticipation_pre_speech",
                                   "Mid_TSST",
                                   "Post_maths",
                                   "Pre_cog",
                                   "Post_cog",
                                   "Recovery"))

ggplot(df, aes(x = phase, y = heart_rate)) +
  stat_summary(fun = mean, geom = "line", group = 1) +
  stat_summary(fun = mean, geom = "point") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
#so now I need to see does heart rate change across the measurement points?
model2 <- lmer(heart_rate ~ time + (1 | Subject_ID), data = df)
summary(model2)

#the next step is to look at whether CTQ score influence phase responses
#so first, i need to define the phase variables
df$phase <- dplyr::case_when(df$time == 1 ~ "baseline",
                             df$time %in% c(2,3)~"anticipation",
                             df$time %in% 4:7 ~ "TSST",
                             df$time == 8 ~"recovery")

#next step, turn it into a factor to run in the later model
df$phase <- factor(df$phase, levels = c("baseline", "anticipation", "TSST", "recovery"))

#now we need to create the piecewise slopes for each of my phases
#these will then be used in the model
#we are only constructing 3 as BL is the reference point
#these basically say 'how far into this phase is this datapoint', to allow 
#the code to recognise how each timepoint contributes to each phase
df$anticipation <- ifelse(df$time ==2,1, ifelse(df$time == 3,2,0))
df$tsst <- ifelse(df$time >=4 & df$time <=7, df$time - 3,0)
df$recovery <- ifelse(df$time == 8,1,0)

#now we can FINALLY run the pure piecewise model 
model3 <- lmer(heart_rate ~ anticipation +tsst+recovery + (1|Subject_ID), data=df)
summary(model3)

#this is now looking at whether CTQ influences any phase of the stress response

df$CTQ_c <- scale(df$CTQ_Total, scale = FALSE)

model3_CTQ_c <- lmer(heart_rate ~ (anticipation +tsst+recovery)*CTQ_c + (1|Subject_ID), data=df)
summary(model3_CTQ_c)
