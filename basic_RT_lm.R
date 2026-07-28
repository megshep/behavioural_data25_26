library(lmerTest)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(rstatix)

df <- read.xlsx('processing_speed_data.xlsx')

outliers.ps <- df %>% identify_outliers(processing_speed) %>% 
  filter(is.extreme==TRUE) %>% select(-c("is.outlier","is.extreme"))

df <- df %>%
  anti_join(outliers.ps)

ps <- df$processing_speed
day <- df$day
age <- df$Age
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

ctq_c <- scale(ctq,center = TRUE,scale = FALSE)


#irrespective of stress condition
day_model <- lmer(ps ~ day +age+sex+(1|subject_id))
summary(day_model)

ctq_model <- lmer(ps ~ ctq_c +age+sex+(1|subject_id))
summary(ctq_model)

ea_model <- lmer(ps ~ ea +age+sex+(1|subject_id))
summary(ea_model)

en_model <- lmer(ps ~ en+age+sex+(1|subject_id))
summary(en_model)

pa_model <- lmer(ps ~ pa +age+sex+(1|subject_id))
summary(pa_model)

pn_model <- lmer(ps ~ pn +age+sex+(1|subject_id))
summary(pn_model)

sa_model <- lmer(ps ~ sa +age+sex+(1|subject_id))
summary(sa_model)

#mean centre threat
#dont need to mean centre physical abuse bc a score of 0 represents no instances of abuse (i think but i should check this! )
threat_c <- df$threat - mean(df$threat, na.rm = TRUE)

threatc_model <- lmer(ps ~ threat_c +age+sex+(1|subject_id))
summary(threatc_model)

threatc_pa <- lmer(ps ~ threat_c*pa +age+sex+(1|subject_id))
summary(threatc_pa)

#condition dependent
daycond_model <- lmer(ps ~ day*condition +age+sex+(1|subject_id))
summary(daycond_model)

ctqcond_model <- lmer(ps ~ ctq_c*condition +age+sex+(1|subject_id))
summary(ctqcond_model)

eacond_model <- lmer(ps ~ ea*condition +age+sex+(1|subject_id))
summary(eacond_model)

encond_model <- lmer(ps ~ en*condition+age+sex+(1|subject_id))
summary(encond_model)

pacond_model <- lmer(ps ~ pa*condition +age+sex+(1|subject_id))
summary(pacond_model)

pncond_model <- lmer(ps ~ pn*condition +age+sex+(1|subject_id))
summary(pncond_model)

sacond_model <- lmer(ps ~ sa*condition +age+sex+(1|subject_id))
summary(sacond_model)

threatcondc_model <- lmer(ps ~ threat_c*condition +age+sex+(1|subject_id))
summary(threatcondc_model)
