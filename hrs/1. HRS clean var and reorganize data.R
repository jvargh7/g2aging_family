setwd("E:/OneDrive/spouse concord China US India/Data")

spouseHRS <- read.csv("spouseHRS.csv")
spouseHRS <- spouseHRS[, -1]

library(car)
library(tidyverse)
library(tableone)
library(kableExtra)
library(nnet)
library(psych)

colnames(spouseHRS)


#----------------------------------
#the households have duplicates
spouseHRS %>% group_by(hhid) %>% tally()
#remove the duplicated rows with the same household ID
spouseHRS <- spouseHRS %>% distinct(hhid, .keep_all = T)


#------------------------------------------------------------------------
#define sample
#married or partnered and spouse present
table(spouseHRS$MSTAT)
spouseHRS <- spouseHRS[which(spouseHRS$MSTAT=="1.Married"|
                               spouseHRS$MSTAT=="3.Partnered"), ]

#heterosexual couple
table(spouseHRS$r_gender, spouseHRS$s_gender)
spouseHRS <- spouseHRS[which(spouseHRS$r_gender=="1.Male" & spouseHRS$s_gender=="2.Female"|
                               spouseHRS$r_gender=="2.Female" & spouseHRS$s_gender=="1.Male"), ]

nrow(spouseHRS)  
table(spouseHRS$ifcouplehh)
#N=6,597

#from the flowchart, use those with self-reported htn, for those reported no/NA, use those with complete sbp or dbp
#from Jithin's code:https://github.com/jvargh7/g2aging_family/blob/main/lasi/g2alasi_analytic%20sample.R
table(spouseHRS$r_diagnosed_bp, spouseHRS$s_diagnosed_bp)
nrow(spouseHRS[which(spouseHRS$r_diagnosed_bp=="1.Yes" & spouseHRS$s_diagnosed_bp=="1.Yes"), ])
nrow(spouseHRS)-1969
3989-1969


legal_age <- spouseHRS %>%
  dplyr::filter(r_age >= 18, s_age >= 18)


self_htn_yes <- legal_age %>% 
  dplyr::filter(r_diagnosed_bp == "1.Yes", s_diagnosed_bp == "1.Yes")

self_htn_other <- legal_age %>% 
  dplyr::filter(!(r_diagnosed_bp == "1.Yes" & s_diagnosed_bp == "1.Yes")) 


spouseHRS <- spouseHRS %>%
  mutate(r_eligible = case_when(r_diagnosed_bp == 1 ~ 1,
                                !is.na(r_sbp) & !is.na(r_dbp) ~ 1,
                                TRUE ~ 0),
         s_eligible = case_when(s_diagnosed_bp == 1 ~ 1,
                                !is.na(s_sbp) & !is.na(s_dbp) ~ 1,
                                TRUE ~ 0))


spouseHRS <- spouseHRS %>% 
  dplyr::filter(r_eligible == 1,s_eligible == 1)
#N=3989




#-----------------------------------------
#check and recode the variables
#age
table(spouseHRS$r_age)
table(spouseHRS$s_age)
#some respondent/spouse are below 45. Should we refine both members to be aged at least 45?
#for most state, 18+ is the legal marriage age

#edu, yrs of edu
table(spouseHRS$r_education)
spouseHRS$r_education <- ifelse(spouseHRS$r_education=="0.None", 0,
                                ifelse(spouseHRS$r_education=="17.17+ yrs", 17, spouseHRS$r_education))
spouseHRS$r_education <- as.numeric(spouseHRS$r_education)

spouseHRS$s_education <- ifelse(spouseHRS$s_education=="0.None", 0,
                                ifelse(spouseHRS$s_education=="17.17+ yrs", 17, spouseHRS$s_education))
spouseHRS$s_education <- as.numeric(spouseHRS$s_education)
table(spouseHRS$s_education)

#edu, harmonized
table(spouseHRS$r_education_h)
spouseHRS$r_education_h <- ifelse(spouseHRS$r_education_h=="1.less than upper secondary", "Less than lower secondary",
                                  ifelse(spouseHRS$r_education_h=="2.upper secondary and vocational training", "Upper secondary and vocational training",
                                         ifelse(spouseHRS$r_education_h=="3.tertiary", "Tertiary", NA)))
spouseHRS$r_education_h <- factor(spouseHRS$r_education_h, levels = c("Less than lower secondary", "Upper secondary and vocational training", "Tertiary"))

spouseHRS$s_education_h <- ifelse(spouseHRS$s_education_h=="1.less than upper secondary", "Less than lower secondary",
                                  ifelse(spouseHRS$s_education_h=="2.upper secondary and vocational training", "Upper secondary and vocational training",
                                         ifelse(spouseHRS$s_education_h=="3.tertiary", "Tertiary", NA)))
spouseHRS$s_education_h <- factor(spouseHRS$s_education_h, levels = c("Less than lower secondary", "Upper secondary and vocational training", "Tertiary"))
table(spouseHRS$s_education_h)

#race/ethnicity
table(spouseHRS$r_race)
table(spouseHRS$r_hispanic)
spouseHRS$r_raceethnic <- ifelse(spouseHRS$r_hispanic=="1.Hispanic", "Hispanic",
                               ifelse(spouseHRS$r_hispanic=="0.Not Hispanic" &
                                        spouseHRS$r_race=="1.White/Caucasian", "NHWhite", 
                                      ifelse(spouseHRS$r_hispanic=="0.Not Hispanic" &
                                               spouseHRS$r_race=="2.Black/African American", "NHBlack",
                                             ifelse(spouseHRS$r_hispanic=="0.Not Hispanic" &
                                                      spouseHRS$r_race=="3.Other", "NHOther", NA))))

spouseHRS$r_raceethnic <- factor(spouseHRS$r_raceethnic, levels = c("NHWhite", "NHBlack", "NHOther", "Hispanic"))
table(spouseHRS$r_raceethnic)

spouseHRS$s_raceethnic <- ifelse(spouseHRS$s_hispanic=="1.Hispanic", "Hispanic",
                                 ifelse(spouseHRS$s_hispanic=="0.Not Hispanic" &
                                          spouseHRS$s_race=="1.White/Caucasian", "NHWhite", 
                                        ifelse(spouseHRS$s_hispanic=="0.Not Hispanic" &
                                                 spouseHRS$s_race=="2.Black/African American", "NHBlack",
                                               ifelse(spouseHRS$s_hispanic=="0.Not Hispanic" &
                                                        spouseHRS$s_race=="3.Other", "NHOther", NA))))

spouseHRS$s_raceethnic <- factor(spouseHRS$s_raceethnic, levels = c("NHWhite", "NHBlack", "NHOther", "Hispanic"))
table(spouseHRS$s_raceethnic)


#labor force status
table(spouseHRS$r_laborforce)
spouseHRS$r_laborforce <- ifelse(spouseHRS$r_laborforce=="1.Works FT" |
                                   spouseHRS$r_laborforce=="2.Works PT", "employed",
                                 ifelse(spouseHRS$r_laborforce=="4.Partly retired" |
                                          spouseHRS$r_laborforce=="5.Retired", "retired", "other"))
spouseHRS$r_laborforce <- factor(spouseHRS$r_laborforce, levels = c("employed", "retired", "other"))

spouseHRS$s_laborforce <- ifelse(spouseHRS$s_laborforce=="1.Works FT" |
                                   spouseHRS$s_laborforce=="2.Works PT", "employed",
                                 ifelse(spouseHRS$s_laborforce=="4.Partly retired" |
                                          spouseHRS$s_laborforce=="5.Retired", "retired", "other"))
spouseHRS$s_laborforce <- factor(spouseHRS$s_laborforce, levels = c("employed", "retired", "other"))
table(spouseHRS$s_laborforce)


#religion
table(spouseHRS$r_religion)
table(spouseHRS$s_religion)

#-------------------------------------------------------------
#hypertension
#sbp
table(spouseHRS$r_sbp)
summary(spouseHRS$r_sbp)
summary(spouseHRS$s_sbp)

#dbp
summary(spouseHRS$r_dbp)
summary(spouseHRS$s_dbp)

#diagnosed bp
table(spouseHRS$r_diagnosed_bp, exclude = NULL)
spouseHRS$r_diagnosed_bp <- ifelse(spouseHRS$r_diagnosed_bp=="0.No", 0, 1)

spouseHRS$s_diagnosed_bp <- ifelse(spouseHRS$s_diagnosed_bp=="0.No", 0, 1)
table(spouseHRS$s_diagnosed_bp, exclude = NULL)

#-----------------------------
#For those who were not diagnosed (meaning self-diagnosed hypertension is NA or 0)
#then defined hypertension as SBP greater than or equal to 140 mm Hg 
#or DBP greater than or equal to 90 mm Hg
spouseHRS$r_htn <- ifelse(spouseHRS$r_sbp>=140 | spouseHRS$r_dbp>=90, 1, 
                          ifelse(spouseHRS$r_sbp<140 & spouseHRS$r_dbp<90, 0, NA))
spouseHRS$r_htn <- ifelse(is.na(spouseHRS$r_diagnosed_bp)|
                                  spouseHRS$r_diagnosed_bp==0, spouseHRS$r_htn, 
                          spouseHRS$r_diagnosed_bp)
table(spouseHRS$r_htn)


spouseHRS$s_htn <- ifelse(spouseHRS$s_sbp>=140 | spouseHRS$s_dbp>=90, 1, 
                          ifelse(spouseHRS$s_sbp<140 & spouseHRS$s_dbp<90, 0, NA))
spouseHRS$s_htn <- ifelse(is.na(spouseHRS$s_diagnosed_bp)|
                            spouseHRS$s_diagnosed_bp==0, spouseHRS$s_htn, 
                          spouseHRS$s_diagnosed_bp)
table(spouseHRS$s_htn)

###########################
#create the joint prevalence of htn in here 
spouseHRS <- spouseHRS %>%
  mutate(hh_htn = case_when(r_htn == 1 & s_htn == 1 ~ 1,
                            TRUE ~ 0))

#----------------------------------
#diagnosed dm
table(spouseHRS$r_diagnosed_dm)
spouseHRS$r_diagnosed_dm <- ifelse(spouseHRS$r_diagnosed_dm=="0.No", 0, 1)

spouseHRS$s_diagnosed_dm <- ifelse(spouseHRS$s_diagnosed_dm=="0.No", 0, 1)
table(spouseHRS$s_diagnosed_dm)

#weight
summary(spouseHRS$r_weight)
summary(spouseHRS$s_weight)

#height
summary(spouseHRS$r_height)
summary(spouseHRS$s_height)

#bmi
summary(spouseHRS$r_bmi)
summary(spouseHRS$s_bmi)

#waistcircumsference. Unite: inches. change into cm.
summary(spouseHRS$r_waistcircumference)
spouseHRS$r_waistcircumference <- spouseHRS$r_waistcircumference*2.54
spouseHRS$s_waistcircumference <- spouseHRS$s_waistcircumference*2.54
hist(spouseHRS$r_waistcircumference)
summary(spouseHRS$s_waistcircumference)

#smoke ever, smoke now
table(spouseHRS$r_smokeever)
table(spouseHRS$r_smokecurr)
spouseHRS$r_smoke <- ifelse(spouseHRS$r_smokeever=="0.No" & 
                              spouseHRS$r_smokecurr=="0.No", "never",
                            ifelse(spouseHRS$r_smokeever=="1.Yes" & 
                                     spouseHRS$r_smokecurr=="0.No", "former",
                                   ifelse(spouseHRS$r_smokecurr=="1.Yes", "current", NA)))
table(spouseHRS$r_smoke, exclude = NULL)
spouseHRS$s_smoke <- ifelse(spouseHRS$s_smokeever=="0.No" & 
                              spouseHRS$s_smokecurr=="0.No", "never",
                            ifelse(spouseHRS$s_smokeever=="1.Yes" & 
                                     spouseHRS$s_smokecurr=="0.No", "former",
                                   ifelse(spouseHRS$s_smokecurr=="1.Yes", "current", NA)))


#heavy drinker
table(spouseHRS$r_alcohol)
spouseHRS$r_alcohol[spouseHRS$r_alcohol=="0.0 or doesnt drink"] <- 0
spouseHRS$r_alcohol <- as.numeric(spouseHRS$r_alcohol)
spouseHRS$r_heavydrinker <- ifelse(spouseHRS$r_alcohol>=3, 1, 0)
table(spouseHRS$r_heavydrinker, exclude = NULL)

spouseHRS$s_alcohol[spouseHRS$s_alcohol=="0.0 or doesnt drink"] <- 0
spouseHRS$s_alcohol <- as.numeric(spouseHRS$s_alcohol)
spouseHRS$s_heavydrinker <- ifelse(spouseHRS$s_alcohol>=3, 1, 0)
table(spouseHRS$s_heavydrinker, exclude = NULL)


#moderate physical activity
table(spouseHRS$r_moderate_pa)
table(spouseHRS$r_vigorous_pa)
colnames(spouseHRS)
#1.Every day 2.>1 per week  3.1 per week 4.l-3 per mon       5.Never 
spouseHRS[, 47:50] <- lapply(spouseHRS[, 47:50], function(x) ifelse(x=="1.Every day", 7,
                                                                    ifelse(x=="2.>1 per week", 3.5,
                                                                           ifelse(x=="3.1 per week", 1,
                                                                                  ifelse(x=="4.l-3 per mon", 0.5,
                                                                                         ifelse(x=="5.Never", 0, NA))))))

#children
table(spouseHRS$hh_children)

#hhsize
table(spouseHRS$hh_size)

#recode hhincome & #hhwealth into hh_incometertile, hh_wealthquintile
summary(spouseHRS$hh_income)
summary(spouseHRS$hh_wealth)
require(Hmisc)
spouseHRS$hh_income <- cut(spouseHRS$hh_income, breaks=c(wtd.quantile(spouseHRS$hh_income,
                                                                             weights=spouseHRS$hh_weight,
                                                                             probs = c(0,0.33,0.67,1.0))),
                                  right=TRUE,include.lowest=TRUE,
                                  labels=c("Low","Medium","High"))
spouseHRS$hh_wealth <- cut(spouseHRS$hh_wealth, breaks=c(wtd.quantile(spouseHRS$hh_wealth,
                                                                      weights=spouseHRS$hh_weight,
                                                                      probs = seq(0,1,by=0.2))),
                           right=TRUE,include.lowest=TRUE,
                           labels=c("Lowest","Low","Medium","High","Highest"))
#rename them 
colnames(spouseHRS)
names(spouseHRS)[53:54] <- c("hh_incometertile", "hh_wealthquintile")
table(spouseHRS$hh_incometertile)  
table(spouseHRS$hh_wealthquintile)


#residence
table(spouseHRS$residence)
spouseHRS$residence <- ifelse(spouseHRS$residence=="1.rural", "Rural",
                              ifelse(spouseHRS$residence=="0.urban", "Urban", NA))
spouseHRS$residence <- factor(spouseHRS$residence, levels = c("Urban", "Rural"))


#length of current marriage
summary(spouseHRS$hh_lengthmar)
spouseHRS$hh_lengthmar_ge10 <- ifelse(spouseHRS$hh_lengthmar>=10, 1, 0)
table(spouseHRS$hh_lengthmar_ge10)

#if first marriage
table(spouseHRS$hh_firstmar)
spouseHRS$hh_firstmar <- ifelse(spouseHRS$hh_firstmar==1, 1, 0)



#------------------------------------------------------------------------
#reorganize the data using naming w_ and h_
#spilit the data based on respondent's sex
table(spouseHRS$r_gender)
femaledata <- spouseHRS[which(spouseHRS$r_gender=="2.Female"), ]
maledata <- spouseHRS[which(spouseHRS$r_gender=="1.Male"), ]

#rename femaledata
colnames(femaledata)
#we added "hh_htn", so need to update this, too
names(femaledata)[13:70] <- c("w_age", "h_age",
                              "w_education", "h_education",
                              "w_laborforce", "h_laborforce",
                              "w_race", "h_race",
                              "w_hispanic", "h_hispanic",
                              "w_religion", "h_religion",
                              "w_diagnosed_bp", "h_diagnosed_bp",
                              "w_diagnosed_dm", "h_diagnosed_dm",
                              "w_height", "h_height",
                              "w_weight", "h_weight",
                              "w_bmi","h_bmi",
                              "w_waistcircumference", "h_waistcircumference",
                              "w_sbp", "h_sbp",
                              "w_dbp", "h_dbp",
                              "w_smokeever", "h_smokeever",
                              "w_smokecurr", "h_smokecurr",
                              "w_alcohol", "h_alcohol",
                              "w_moderate_pa", "h_moderate_pa",
                              "w_vigorous_pa", "h_vigorous_pa",
                              "hh_children", "hh_size", 
                              "hh_incometertile", "hh_wealthquintile", 
                              "hh_lengthmar", "hh_firstmar",
                              "w_education_h", "h_education_h",
                              "residence",
                              "w_eligible", "h_eligible",
                              "w_raceethnic", "h_raceethnic",
                              "w_htn", "h_htn", "hh_htn",
                              "w_smoke", "h_smoke",
                              "w_heavydrinker", "h_heavydrinker")

#rename maledata
colnames(maledata)
names(maledata)[13:70] <- c("h_age", "w_age",
                            "h_education", "w_education",
                            "h_laborforce", "w_laborforce",
                            "h_race", "w_race",
                            "h_hispanic", "w_hispanic",
                            "h_religion", "w_religion",
                            "h_diagnosed_bp", "w_diagnosed_bp",
                            "h_diagnosed_dm", "w_diagnosed_dm",
                            "h_height", "w_height",
                            "h_weight", "w_weight",
                            "h_bmi","w_bmi",
                            "h_waistcircumference", "w_waistcircumference",
                            "h_sbp", "w_sbp",
                            "h_dbp", "w_dbp",
                            "h_smokeever", "w_smokeever",
                            "h_smokecurr", "w_smokecurr",
                            "h_alcohol", "w_alcohol",
                            "h_moderate_pa", "w_moderate_pa",
                            "h_vigorous_pa", "w_vigorous_pa",
                            "hh_children", "hh_size", 
                            "hh_incometertile", "hh_wealthquintile", 
                            "hh_lengthmar", "hh_firstmar",
                            "h_education_h", "w_education_h",
                            "residence",
                            "h_eligible", "w_eligible",
                            "h_raceethnic", "w_raceethnic",
                            "h_htn", "w_htn", "hh_htn",
                            "h_smoke", "w_smoke",
                            "h_heavydrinker", "w_heavydrinker")

#remerge the femaledata and maledata
coupleHRS <- rbind(femaledata, maledata)
colnames(coupleHRS)

write.csv(coupleHRS, "coupleHRS.csv")


