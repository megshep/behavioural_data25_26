library(lmerTest)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(rstatix)
library(emmeans)

df <- read.xlsx('long_cog_data_sentences.xlsx')

outliers.ps <- df %>% identify_outliers(processing_speed) %>% 
  filter(is.extreme==TRUE) %>% select(-c("is.outlier","is.extreme"))

df <- df %>%
  anti_join(outliers.ps)

ps <- df$processing_speed
day <- df$day
age <- as.numeric(df$Age)
sex <- df$Sex_1M2F
subject_id <- df$Subject_ID
ctq <- as.numeric(df$CTQ_Total)
threat <- df$threat
ea <- df$emotional_abuse
en <- df$emotional_neglect
pn <- df$physical_neglect
pa <- df$physical_abuse
sa <- df$sexual_abuse
condition <- df$Condition_S1C2
sense <- df$sense
stress <- df$sentence_stress

#mean centre threat and CTQ
#dont need to mean centre physical abuse bc a score of 0 represents no instances of abuse (i think but i should check this! )
threat_c <- df$threat - mean(df$threat, na.rm = TRUE)
ctq_c <- ctq - mean(ctq, na.rm=TRUE)

#model 1 
model1 <-lmer(ps ~ day*sense*stress + age + sex + (1|Subject_ID), data=df)
summary(model1)
#estimate means sep for each day
emmeans(model1, ~ sense * stress | day)

#compare the means (pairwise comparisons)
emm <- pairs(emmeans(model1, ~ sense*stress | day))
emm_df <- as.data.frame(emm)

#visualise the significant stress*sense interaction
ggplot(emm_df,aes(x = stress, y = emmean, colour = sense, group = sense)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),width = .1) +
  facet_wrap(~day) +
  labs(x = "Sentence type", y = "Estimated Reaction Time (ms)", colour = "Sentence sense") +
  theme_classic(base_size = 14)


model2 <-lmer(ps ~ day*sense*stress*ctq_c + age + sex + (1|Subject_ID), data=df)
summary(model2)

model3 <-lmer(ps ~ day*sense*stress*pa + age + sex + (1|Subject_ID), data=df)
summary(model3)

model4 <-lmer(ps ~ day*sense*stress*pn + age + sex + (1|Subject_ID), data=df)
summary(model4)

model5 <-lmer(ps ~ day*sense*stress*en + age + sex + (1|Subject_ID), data=df)
summary(model5)

model6 <-lmer(ps ~ day*sense*stress*ea + age + sex + (1|Subject_ID), data=df)
summary(model6)

model7 <-lmer(ps ~ day*sense*stress*sa + age + sex + (1|Subject_ID), data=df)
summary(model7)
