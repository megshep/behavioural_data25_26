library(lmerTest)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(rstatix)

df <- read.xlsx('HR_long.xlsx')

#define key variables, convert from character to numeric wherever needed
df$condition <- factor(df$Condition_S1C2)
df$CTQ_Total <- as.numeric(df$CTQ_Total)
df$ctq <- scale(df$CTQ_Total, scale = FALSE)
df$age <- as.numeric(df$Age)
df$sex <- factor(df$Sex_1M2F)
df$ea <- df$emotional_abuse
df$en <- df$emotional_neglect

#the next step is to look at whether CTQ score influence phase responses
#so first, i need to define the phase variables
df$phase <- dplyr::case_when(df$time == 1 ~ "baseline",
                             df$time %in% c(2,3) ~ "anticipation",
                             df$time %in% 4:7 ~ "TSST",
                             df$time == 8 ~ "recovery")

#next step, turn it into a factor to run in the later model
df$phase <- factor(df$phase,
                   levels = c("baseline", "anticipation", "TSST", "recovery"))

#now we need to create the piecewise slopes for each of my phases
#these will then be used in the model
#we are only constructing 3 as BL is the reference point
#these basically say 'how far into this phase is this datapoint', to allow
#the code to recognise how each timepoint contributes to each phase
df$anticipation <- pmin(pmax(df$time - 1, 0), 2)
df$tsst <- pmin(pmax(df$time - 3, 0), 4)
df$recovery <- pmax(df$time - 7, 0)

#fit the mixed effects model
EN_model_full <- lmer(heart_rate ~ (anticipation + tsst + recovery)*en*condition+age+sex+(1 | Subject_ID),data = df)
summary(EN_model_full)

#Stratified 5-fold Cross Validation (sex × condition)
# stratified for both sex and condition bc gender imbalanced sample needed representing in both conditions for this analysis as interaction with condition
set.seed(123)

#remove missing data ONCE (important for mixed models)
df <- df %>% filter(!is.na(heart_rate),
         !is.na(en),
         !is.na(age),
         !is.na(sex),
         !is.na(condition),
         !is.na(Subject_ID))

#first create a participant-level dataset, ensures each participant only appears once
participants <- df %>% distinct(Subject_ID, sex, condition)

#next assign participants to one of five folds 
#this is stratified by sex and condition so each fold contains roughly the same number of males and females
participants <- participants %>% group_by(sex, condition) %>%
  mutate(fold = sample(rep(1:5, length.out = n()))) %>% ungroup()

#now merge the fold allocation back into the dataset 
#all repeated observations from the same participant will stay in the same fold i.e., separated on px level not an observation leve
df_cv <- left_join(df, participants, by = c("Subject_ID", "sex", "condition"))
rmse <- numeric(5)

#create an empty object to store RMSE values
for(i in 1:5){
  
  #split into training and testing datasets
  train <- df_cv %>% filter(fold != i)
  test  <- df_cv %>% filter(fold == i)
  
  #ensure factor consistency (critical)
  train$condition <- factor(train$condition, levels = levels(df$condition))
  test$condition  <- factor(test$condition,  levels = levels(df$condition))
  train$sex <- factor(train$sex, levels = levels(df$sex))
  test$sex  <- factor(test$sex,  levels = levels(df$sex))
  
  #fit the model using the training data
  model <- lmer(heart_rate ~ (anticipation + tsst + recovery)*en*condition+age+sex+(1 | Subject_ID),data = train)
  
  #creates a design matrix for the test data
  #converts test dataset into numeric format for reg model (cols of predictors)
  #this inckudes interactions and piecewise slopes
  X_test <- model.matrix(delete.response(terms(model)), test)
  
  #extracts fixed-effects coefficients from trained model 
  beta <- fixef(model)
  
  #keep only shared terms e.g., the predictors that exist in both x_test and beta
  common_terms <- intersect(colnames(X_test), names(beta))
  
  #redcues the test matrix to only the shared columns
  #remove any columns that dont have matching coefficients
  X_test2 <- X_test[, common_terms, drop = FALSE]
  
  #filters coefficients - keeps only coefficients that correspond to shared predictors
  #aligns beta with X_test2
   beta2 <- beta[common_terms]
  
   #generates predictors - multiples rows or X_test2 with corresponding coefficients
  #creates predicted heart rate values for each observation
  pred <- as.numeric(X_test2 %*% beta2)
  
  #RMSE
  rmse[i] <- sqrt(mean((test$heart_rate - pred)^2))}

#create a dataframe to show results of CV
data.frame(Fold = 1:5,RMSE = rmse)

mean(rmse)
sd(rmse)

#work out the range of the sample to help contextualise the RMSE
range(df$heart_rate, na.rm = TRUE)

#work out the standard deviation to see variability within the data
sd(df$heart_rate, na.rm = TRUE)
