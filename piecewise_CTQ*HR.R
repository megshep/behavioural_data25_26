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
#this is what we use to visualise the typical HR response and separate it into two separate conditios=ns
df_summary <- df %>% group_by(Condition_S1C2, time) %>%
  summarise(mean_hr = mean(heart_rate, na.rm = TRUE), se = sd(heart_rate, na.rm = TRUE)/sqrt(n()),.groups = "drop")

#one way to visualise this (irrespective of conditions, just overall)
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


#now to visualise both trajectories separate
ggplot(df_summary,aes(x = time, y = mean_hr,
           group = Condition_S1C2)) +
  geom_line() +
  geom_point() + facet_wrap(~Condition_S1C2) +
  labs(x = "Time point", y = "Heart rate",
    title = "Heart rate trajectories by stress condition") +
  theme_classic()

#the more detailed overlayed graph with labels:
ggplot(df,aes(x = phase, y = heart_rate,
           colour = Condition_S1C2,
           group = Condition_S1C2)) + stat_summary(fun = mean,
               geom = "line",
               linewidth = 1) + stat_summary(fun = mean,
               geom = "point",
               size = 2) +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  labs(x = "Phase",
       y = "Heart rate",
       colour = "Condition",
       title = "Heart rate trajectories by stress condition")
 
#so now I need to see does heart rate change across the measurement points, irrespective of condition?
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
df$anticipation <- pmin(pmax(df$time - 1, 0), 2)
df$tsst <- pmin(pmax(df$time - 3,0),4)
df$recovery <- pmax(df$time - 7, 0)

#but so far we've ignored the fact we have a high and low stress condition!
#so first i want to check whether condition influenced the trajectory
condition <- df$Condition_S1C2
model3_con <- lmer(heart_rate ~ (anticipation + tsst + recovery)*condition + (1|Subject_ID), data=df)
summary(model3_con)

#now we can FINALLY run the pure piecewise model 
#this is now looking at whether CTQ influences any phase of the stress response (irrespective of condition)
df$CTQ_c <- scale(df$CTQ_Total, scale = FALSE)
model4_CTQ_c <- lmer(heart_rate ~ (anticipation +tsst+recovery)*CTQ_c + (1|Subject_ID), data=df)
summary(model4_CTQ_c)

#now lets see if the CTQ influences any of the phases of the stress response and whether this differs by high and low stress conditions
model5_CTQ_c <- lmer(heart_rate ~ (anticipation + tsst + recovery)*condition*CTQ_c + (1|Subject_ID),data = df)
summary(model5_CTQ_c)

#ok now we want to see if any of the subscales impact the stress response at all
ea <- df$emotional_abuse
EA_model1 <- lmer(heart_rate ~ (anticipation + tsst + recovery)*ea + (1|Subject_ID), data=df)
summary(EA_model1)

EA_model2 <- lmer(heart_rate ~ (anticipation + tsst + recovery)*ea*condition + (1|Subject_ID), data=df)
summary(EA_model2)

en <- df$emotional_neglect
EN_model1 <- lmer(heart_rate ~ (anticipation + tsst + recovery)*en + (1|Subject_ID), data=df)
summary(EA_model1)

EN_model2 <- lmer(heart_rate ~ (anticipation + tsst + recovery)*en*condition + (1|Subject_ID), data=df)
summary(EA_model2)

pn <- df$physical_neglect
PN_model1 <- lmer(heart_rate ~ (anticipation + tsst + recovery)*pn + (1|Subject_ID), data=df)
summary(PN_model1)

PN_model2 <- lmer(heart_rate ~ (anticipation + tsst + recovery)*pn*condition + (1|Subject_ID), data=df)
summary(PN_model2)

pa <- df$physical_abuse
PA_model1 <- lmer(heart_rate ~ (anticipation + tsst + recovery)*pa + (1|Subject_ID), data=df)
summary(PA_model1)

PA_model2 <- lmer(heart_rate ~ (anticipation + tsst + recovery)*pa*condition + (1|Subject_ID), data=df)
summary(PA_model2)

sa <- df$sexual_abuse
SA_model1 <- lmer(heart_rate ~ (anticipation + tsst + recovery)*sa + (1|Subject_ID), data=df)
summary(SA_model1)

SA_model2 <- lmer(heart_rate ~ (anticipation + tsst + recovery)*sa*condition + (1|Subject_ID), data=df)
summary(SA_model2)

