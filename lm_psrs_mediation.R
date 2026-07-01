library(lm.beta)
library(openxlsx)
library(jtools)
library(lm.beta)

df <- read.xlsx('all_data.xlsx')

df$PSRS_Total <- as.numeric(df$PSRS_Total)

outliers.PSRS<- df %>% identify_outliers(PSRS_Total) %>% 
  filter(is.extreme==TRUE) %>% select(-c("is.outlier","is.extreme"))

df$CTQ_Total <- as.numeric(df$CTQ_Total)
df$Age <- as.numeric(df$Age)


psrs <- df$PSRS_Total
ctq <- df$CTQ_Total
age <- df$Age
sex <- df$Sex_1M2F
ea <- df$emotional_abuse
en <- df$emotional_neglect
pn <- df$physical_neglect
pa <- df$physical_abuse
sa <- df$sexual_abuse

mod1 <- lm(psrs ~ ctq+age+sex, data = df)
summ(mod1)
lm.beta(mod1, complete.standardization = FALSE)

mod2 <- lm(psrs ~ ea+age+sex, data = df)
summ(mod2)
lm.beta(mod2, complete.standardization = FALSE)

mod3 <- lm(psrs ~ en+age+sex, data = df)
summ(mod3)
lm.beta(mod3, complete.standardization = FALSE)

mod4 <- lm(psrs ~ pn+age+sex, data = df)
summ(mod4)
lm.beta(mod4, complete.standardization = FALSE)

mod5 <- lm(psrs ~ pa+age+sex, data = df)
summ(mod5)
lm.beta(mod5, complete.standardization = FALSE)

mod6 <- lm(psrs ~ sa+age+sex, data = df)
summ(mod6)
lm.beta(mod6, complete.standardization = FALSE)


df$ResilienceTotal <- as.numeric(df$ResilienceTotal)
res <- df$ResilienceTotal
resmod1 <- lm(psrs ~ pa+age+sex+res, data = df)
summ(resmod1)
lm.beta(resmod1, complete.standardization = FALSE)

resmod2 <- lm(psrs ~ ea+age+sex+res, data = df)
summ(resmod2)
lm.beta(resmod2, complete.standardization = FALSE)

resmod3 <- lm(psrs ~ pa+age+sex+res+(pa*res), data = df)
summ(resmod3)
lm.beta(resmod3, complete.standardization = FALSE)

#because interaction wasnt significant, i want to see if theres a mediation 
df.med <- na.omit(df[, c("emotional_abuse", "physical_abuse", "ResilienceTotal", "PSRS_Total", "Age", "Sex_1M2F")])
med.model <- lm(ResilienceTotal ~ physical_abuse + Age + Sex_1M2F,data = df.med)
out.model <- lm(PSRS_Total ~ physical_abuse + ResilienceTotal + Age + Sex_1M2F,data = df.med)
med.out <- mediate(med.model,out.model,treat = "physical_abuse", mediator = "ResilienceTotal",
boot = TRUE,sims = 5000)
summary(med.out)

resmod4 <- lm(psrs ~ ea+age+sex+res+(ea*res), data = df)
summ(resmod4)
lm.beta(resmod4, complete.standardization = FALSE)

df.med_ea <- na.omit(df[, c("emotional_abuse", "ResilienceTotal", "PSRS_Total", "Age", "Sex_1M2F")])
med.model_ea <- lm(ResilienceTotal ~ emotional_abuse + Age + Sex_1M2F,data = df.med)
out.model_ea <- lm(PSRS_Total ~ emotional_abuse + ResilienceTotal + Age + Sex_1M2F,data = df.med)
med.out_ea <- mediate(med.model_ea,out.model_ea,treat = "emotional_abuse", mediator = "ResilienceTotal",
                   boot = TRUE,sims = 5000)
summary(med.out_ea)

df$Total_SocialSupport <- as.numeric(df$Total_SocialSupport)
soc <- df$Total_SocialSupport

socmod1 <- lm(psrs ~ pa+age+sex+soc, data = df)
summ(socmod1)
lm.beta(socmod1, complete.standardization = FALSE)

socmod2 <- lm(psrs ~ ea+age+sex+soc, data = df)
summ(socmod2)
lm.beta(socmod2, complete.standardization = FALSE)

socmod3 <- lm(psrs ~ ea+age+sex+soc+(ea*soc), data = df)
summ(socmod3)
lm.beta(socmod2, complete.standardization = FALSE)

