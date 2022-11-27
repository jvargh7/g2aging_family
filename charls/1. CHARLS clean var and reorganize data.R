setwd("G:\\My Drive\\Crossnation study\\Dataset")

spouseCHARLS <- read.csv("spouseCHARLS.csv")
spouseCHARLS <- spouseCHARLS[, -1]

library(car)
library(tidyverse)
library(tableone)
library(kableExtra)
library(nnet)
library(psych)

colnames(spouseCHARLS)

#-----------------------------------------
#the households have duplicates
spouseCHARLS %>% group_by(hhid) %>% tally()
#remove the duplicated rows with the same household ID
spouseCHARLS <- spouseCHARLS %>% distinct(hhid, .keep_all = T)

#-----------------------------------------
#define sample
#married or partnered and spouse present
#marital status: 1.married; 3.partnered; 4.separated; 5.divorced; 7.widowed; 8.never married
table(spouseCHARLS$MSTAT)
spouseCHARLS <- spouseCHARLS[which(spouseCHARLS$MSTAT == 1 | spouseCHARLS$MSTAT == 3), ]

#heterosexual couple
table(spouseCHARLS$r_gender, spouseCHARLS$s_gender)
spouseCHARLS <- spouseCHARLS[which(spouseCHARLS$r_gender == 1 & spouseCHARLS$s_gender == 2 |
                                   spouseCHARLS$r_gender == 2 & spouseCHARLS$s_gender == 1), ]
nrow(spouseCHARLS)  
table(spouseCHARLS$ifcouplehh)
#N=8,820

#from the flowchart, use those with self-reported htn, for those reported no/NA, use those with complete sbp or dbp
#from Jithin's code:https://github.com/jvargh7/g2aging_family/blob/main/lasi/g2alasi_analytic%20sample.R
table(spouseCHARLS$r_diagnosed_bp, spouseCHARLS$s_diagnosed_bp)
nrow(spouseCHARLS[which(spouseCHARLS$r_diagnosed_bp == 1 & spouseCHARLS$s_diagnosed_bp == 1), ])
nrow(spouseCHARLS)-836
8820-836

legal_age <- spouseCHARLS %>%
  dplyr::filter(r_age >= 20, s_age >= 20)

self_htn_yes <- legal_age %>% 
  dplyr::filter(r_diagnosed_bp == 1, s_diagnosed_bp == 1)

self_htn_other <- legal_age %>% 
  dplyr::filter(!(r_diagnosed_bp == 1 & s_diagnosed_bp == 1)) 

spouseCHARLS <- spouseCHARLS %>%
  dplyr::filter(r_age >= 20, s_age >= 20)

spouseCHARLS <- spouseCHARLS %>%
  mutate(r_eligible = case_when(r_diagnosed_bp == 1 ~ 1,
                                !is.na(r_sbp) & !is.na(r_dbp) ~ 1,
                                TRUE ~ 0),
         s_eligible = case_when(s_diagnosed_bp == 1 ~ 1,
                                !is.na(s_sbp) & !is.na(s_dbp) ~ 1,
                                TRUE ~ 0))

spouseCHARLS <- spouseCHARLS %>% 
  dplyr::filter(r_eligible == 1, s_eligible == 1)
#N=6,514

#-----------------------------------------
#check and recode the variables
#age
#in China, 22+ for men and 20+ for women
table(spouseCHARLS$r_age)
table(spouseCHARLS$s_age)

#education: 1.Less than lower secondary; 2.upper secondary & vocational training; 3.tertiary 
table(spouseCHARLS$r_education)
table(spouseCHARLS$s_education)
spouseCHARLS$r_education_h <- factor(spouseCHARLS$r_education, levels = c("1", "2", "3"))
spouseCHARLS$s_education_h <- factor(spouseCHARLS$s_education, levels = c("1", "2", "3"))

#Hukou status: 1.Agricultual hukou; 2.Non-agricultural hukou; 3.Unified residence hukou; 4.Do not have hukou
#Combine 1, 3 and 4 because 2 is the more privileged Hukou status: 1. Agricultual hukou and others; 2. Non-agricultural hukou
table(spouseCHARLS$r_hukou)
table(spouseCHARLS$s_hukou)
spouseCHARLS$r_hukou <- ifelse(spouseCHARLS$r_hukou == 1 , 1,
                               ifelse(spouseCHARLS$r_hukou == 3 , 1,
                                      ifelse(spouseCHARLS$r_hukou == 4 , 1,
                                             ifelse(spouseCHARLS$r_hukou == 2, 0, NA))))

spouseCHARLS$s_hukou <- ifelse(spouseCHARLS$s_hukou == 1 , 1,
                               ifelse(spouseCHARLS$s_hukou == 3 , 1,
                                      ifelse(spouseCHARLS$s_hukou == 4 , 1,
                                             ifelse(spouseCHARLS$s_hukou == 2, 0, NA))))                               

#residence: 0.Urban; 1.Rural or viallage
table(spouseCHARLS$residence)
spouseCHARLS$residence <- factor(spouseCHARLS$residence, levels = c("0", "1"))

#labor force status
table(spouseCHARLS$r_laborforce)
table(spouseCHARLS$s_laborforce)
spouseCHARLS$r_laborforce <- ifelse(spouseCHARLS$r_laborforce == 1 , "employed",
                                    ifelse(spouseCHARLS$r_laborforce == 2 , "employed",
                                           ifelse(spouseCHARLS$r_laborforce == 3 , "employed",
                                                  ifelse(spouseCHARLS$r_laborforce == 4, "employed",
                                                         ifelse(spouseCHARLS$r_laborforce == 5, "employed",
                                                                ifelse(spouseCHARLS$r_laborforce == 6, "other",
                                                                       ifelse(spouseCHARLS$r_laborforce == 7, "retired",
                                                                              ifelse(spouseCHARLS$r_laborforce == 8, "other", NA))))))))
spouseCHARLS$r_laborforce <- factor(spouseCHARLS$r_laborforce, levels = c("employed", "retired", "other"))
table(spouseCHARLS$r_laborforce)

spouseCHARLS$s_laborforce <- ifelse(spouseCHARLS$s_laborforce == 1 , "employed",
                                    ifelse(spouseCHARLS$s_laborforce == 2 , "employed",
                                           ifelse(spouseCHARLS$s_laborforce == 3 , "employed",
                                                  ifelse(spouseCHARLS$s_laborforce == 4, "employed",
                                                         ifelse(spouseCHARLS$s_laborforce == 5, "employed",
                                                                ifelse(spouseCHARLS$s_laborforce == 6, "other",
                                                                       ifelse(spouseCHARLS$s_laborforce == 7, "retired",
                                                                              ifelse(spouseCHARLS$s_laborforce == 8, "other", NA))))))))
spouseCHARLS$s_laborforce <- factor(spouseCHARLS$s_laborforce, levels = c("employed", "retired", "other"))
table(spouseCHARLS$s_laborforce)

#-----------------------------------------
#hypertension
#sbp
summary(spouseCHARLS$r_sbp)
summary(spouseCHARLS$s_sbp)

#dbp
summary(spouseCHARLS$r_dbp)
summary(spouseCHARLS$s_dbp)

#diagnosed bp
table(spouseCHARLS$r_diagnosed_bp)
table(spouseCHARLS$s_diagnosed_bp)

#-----------------------------------------
#For those who were not diagnosed, 
#then defined hypertension as SBP greater than or equal to 140 mm Hg 
#or DBP greater than or equal to 90 mm Hg
spouseCHARLS$r_htn <- ifelse(spouseCHARLS$r_sbp >= 140 | spouseCHARLS$r_dbp >= 90, 1, 
                             ifelse(spouseCHARLS$r_sbp < 140 & spouseCHARLS$r_dbp < 90, 0, NA))
spouseCHARLS$r_htn <- ifelse(is.na(spouseCHARLS$r_diagnosed_bp)|
                             spouseCHARLS$r_diagnosed_bp == 0, spouseCHARLS$r_htn, 
                             spouseCHARLS$r_diagnosed_bp)
table(spouseCHARLS$r_htn)

spouseCHARLS$s_htn <- ifelse(spouseCHARLS$s_sbp >= 140 | spouseCHARLS$s_dbp >= 90, 1, 
                             ifelse(spouseCHARLS$s_sbp < 140 & spouseCHARLS$s_dbp < 90, 0, NA))
spouseCHARLS$s_htn <- ifelse(is.na(spouseCHARLS$s_diagnosed_bp)|
                             spouseCHARLS$s_diagnosed_bp == 0, spouseCHARLS$s_htn, 
                             spouseCHARLS$s_diagnosed_bp)
table(spouseCHARLS$s_htn)

###########################
#create the joint prevalence of htn in here 
spouseCHARLS <- spouseCHARLS %>%
  mutate(hh_htn = case_when(r_htn == 1 & s_htn == 1 ~ 1,
                            TRUE ~ 0))

table(spouseCHARLS$hh_htn)

#-----------------------------------------
#diagnosed dm
table(spouseCHARLS$r_diagnosed_dm)
table(spouseCHARLS$s_diagnosed_dm)

#weight
summary(spouseCHARLS$r_weight)
summary(spouseCHARLS$s_weight)

#height
summary(spouseCHARLS$r_height)
summary(spouseCHARLS$s_height)

#bmi
summary(spouseCHARLS$r_bmi)
summary(spouseCHARLS$s_bmi)

#waist circumsference
summary(spouseCHARLS$r_waistcircumference)
summary(spouseCHARLS$s_waistcircumference)

#smoking and drinking: smoke ever, smoke now, drink ever
table(spouseCHARLS$r_smokeever)
table(spouseCHARLS$s_smokeever)
spouseCHARLS$r_smoke <- ifelse(spouseCHARLS$r_smokeever == 0 & 
                                 spouseCHARLS$r_smokecurr == 0, "never",
                               ifelse(spouseCHARLS$r_smokeever == 1 & 
                                        spouseCHARLS$r_smokecurr == 0, "former",
                                      ifelse(spouseCHARLS$r_smokecurr == 1, "current", NA)))
table(spouseCHARLS$r_smoke, exclude = NULL)

spouseCHARLS$s_smoke <- ifelse(spouseCHARLS$s_smokeever == 0 & 
                                 spouseCHARLS$s_smokecurr == 0, "never",
                               ifelse(spouseCHARLS$s_smokeever == 1 & 
                                        spouseCHARLS$s_smokecurr == 0, "former",
                                      ifelse(spouseCHARLS$s_smokecurr == 1, "current", NA)))

table(spouseCHARLS$r_alcohol)
spouseCHARLS$r_heavydrinker <- ifelse(spouseCHARLS$r_alcohol >= 8, 1, 0)
table(spouseCHARLS$r_heavydrinker, exclude = NULL)

table(spouseCHARLS$s_alcohol)
spouseCHARLS$s_heavydrinker <- ifelse(spouseCHARLS$s_alcohol >= 8, 1, 0)
table(spouseCHARLS$s_heavydrinker, exclude = NULL)

#physical activity: days/week
table(spouseCHARLS$r_vigorous_pa)
table(spouseCHARLS$s_vigorous_pa)

table(spouseCHARLS$r_moderate_pa)
table(spouseCHARLS$s_moderate_pa)

spouseCHARLS$r_vigorous_pa <- as.numeric(spouseCHARLS$r_vigorous_pa)
spouseCHARLS$s_vigorous_pa <- as.numeric(spouseCHARLS$s_vigorous_pa)

spouseCHARLS$r_moderate_pa <- as.numeric(spouseCHARLS$r_moderate_pa)
spouseCHARLS$s_moderate_pa <- as.numeric(spouseCHARLS$s_moderate_pa)

#children
table(spouseCHARLS$hh_children)

#hhsize
table(spouseCHARLS$hh_size)

#recode hhincome & #hhwealth into hh_incometertile, hh_wealthquintile
summary(spouseCHARLS$hh_income)
summary(spouseCHARLS$hh_wealth)

require(Hmisc)
spouseCHARLS$hh_income <- cut(spouseCHARLS$hh_income, breaks = c(wtd.quantile(spouseCHARLS$hh_income,
                                                                      weights = spouseCHARLS$hh_weight,
                                                                      probs = c(0, 0.33, 0.67, 1.0))),
                            right = TRUE, include.lowest = TRUE,
                            labels = c("Low", "Medium", "High"))

spouseCHARLS$hh_wealth <- cut(spouseCHARLS$hh_wealth, breaks = c(wtd.quantile(spouseCHARLS$hh_wealth,
                                                                      weights = spouseCHARLS$hh_weight,
                                                                      probs = seq(0, 1, by = 0.2))),
                            right=TRUE, include.lowest=TRUE,
                            labels = c("Lowest", "Low", "Medium", "High", "Highest"))

#rename them 
colnames(spouseCHARLS)
names(spouseCHARLS)[49:50] <- c("hh_incometertile", "hh_wealthquintile")
table(spouseCHARLS$hh_incometertile)  
table(spouseCHARLS$hh_wealthquintile)

#length of current marriage: used data from wave of 2013 and added two years to the lengths
spouseCHARLS$hh_lengthmar <- spouseCHARLS$hh_lengthmar + 2
summary(spouseCHARLS$hh_lengthmar)

spouseCHARLS$hh_lengthmar_ge10 <- ifelse(spouseCHARLS$hh_lengthmar>=10, 1, 0)
table(spouseCHARLS$hh_lengthmar_ge10)

#if first marriage
table(spouseCHARLS$hh_firstmar)
spouseCHARLS$hh_firstmar <- ifelse(spouseCHARLS$hh_firstmar == 1, 1, 0)
table(spouseCHARLS$hh_firstmar)

#-----------------------------------------
#reorganize the data using naming w_ and h_
#split the data based on respondent's sex: 1 is male, 2 is female
table(spouseCHARLS$r_gender)
femaledata <- spouseCHARLS[which(spouseCHARLS$r_gender == 2), ]
maledata <- spouseCHARLS[which(spouseCHARLS$r_gender == 1), ]

#rename femaledata
colnames(femaledata)
names(femaledata)[12:63] <- c("w_age", "h_age",
                              "w_education", "h_education",
                              "w_laborforce", "h_laborforce",
                              "w_hukou", "h_hukou",
                              "residence",
                              "w_sbp", "h_sbp",
                              "w_dbp", "h_dbp",
                              "w_diagnosed_bp", "h_diagnosed_bp",
                              "w_diagnosed_dm", "h_diagnosed_dm",                                                            
                              "w_height", "h_height",
                              "w_weight", "h_weight",
                              "w_bmi","h_bmi",
                              "w_waistcircumference", "h_waistcircumference",
                              "w_smokeever", "h_smokeever",
                              "w_smokecurr", "h_smokecurr",
                              "w_alcohol", "h_alcohol",
                              "w_vigorous_pa", "h_vigorous_pa",
                              "w_moderate_pa", "h_moderate_pa",
                              "hh_children", "hh_size", 
                              "hh_incometertile", "hh_wealthquintile", "hh_lengthmar", "hh_firstmar",
                              "w_eligible", "h_eligible",
                              "w_education_h", "h_education_h",
                              "w_htn", "h_htn", "hh_htn",
                              "w_smoke", "h_smoke",
                              "w_heavydrinker", "h_heavydrinker")

#rename maledata
colnames(maledata)
names(maledata)[12:63] <- c("h_age", "w_age",
                            "h_education", "w_education",
                            "h_laborforce", "w_laborforce",
                            "h_hukou", "w_hukou",
                            "residence",
                            "h_sbp", "w_sbp",
                            "h_dbp", "w_dbp",
                            "h_diagnosed_bp", "w_diagnosed_bp",
                            "h_diagnosed_dm", "w_diagnosed_dm",                                                            
                            "h_height", "w_height",
                            "h_weight", "w_weight",
                            "h_bmi","w_bmi",
                            "h_waistcircumference", "w_waistcircumference",
                            "h_smokeever", "w_smokeever",
                            "h_smokecurr", "w_smokecurr",
                            "h_alcohol", "w_alcohol",
                            "h_vigorous_pa", "w_vigorous_pa",
                            "h_moderate_pa", "w_moderate_pa",
                            "hh_children", "hh_size", 
                            "hh_incometertile", "hh_wealthquintile", "hh_lengthmar", "hh_firstmar",
                            "h_eligible", "w_eligible",
                            "h_education_h", "w_education_h",
                            "h_htn", "w_htn", "hh_htn",
                            "h_smoke", "w_smoke",
                            "h_heavydrinker", "w_heavydrinker")

#remerge the femaledata and maledata
coupleCHARLS <- rbind(femaledata, maledata)
colnames(coupleCHARLS)

write.csv(coupleCHARLS, "coupleCHARLS.csv")