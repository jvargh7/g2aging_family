setwd("G:\\My Drive\\Crossnation study\\Dataset")
spouseHRS <- read.csv("spouseHRS.csv")
spouseHRS <- spouseHRS[, -1]

library(car)
library(tidyverse)
library(tableone)
library(kableExtra)
library(nnet)
library(psych)

colnames(spouseHRS)

#-----------------------------------------
#check and recode the variables
#age
table(spouseHRS$r_age)
table(spouseHRS$s_age)
#some respondent/spouse are below 45. Should we refine both members to be aged at least 45?

#edu
table(spouseHRS$r_education)
spouseHRS$r_education <- ifelse(spouseHRS$r_education=="0.None", 0,
                                ifelse(spouseHRS$r_education=="17.17+ yrs", 17, spouseHRS$r_education))
spouseHRS$r_education <- as.numeric(spouseHRS$r_education)

spouseHRS$s_education <- ifelse(spouseHRS$s_education=="0.None", 0,
                                ifelse(spouseHRS$s_education=="17.17+ yrs", 17, spouseHRS$s_education))
spouseHRS$s_education <- as.numeric(spouseHRS$s_education)
table(spouseHRS$s_education)

#employment
table(spouseHRS$r_employment)
spouseHRS$r_employment <- ifelse(spouseHRS$r_employment=="1.Working for pay", "Yes", "No")
spouseHRS$r_employment <- factor(spouseHRS$r_employment, levels = c("No", "Yes"))

spouseHRS$s_employment <- ifelse(spouseHRS$s_employment=="1.Working for pay", "Yes", "No")
spouseHRS$s_employment <- factor(spouseHRS$s_employment, levels = c("No", "Yes"))
table(spouseHRS$s_employment)

#retirement
table(spouseHRS$r_retirement)
spouseHRS$r_retirement <- ifelse(spouseHRS$r_retirement=="0.Not retired", "No",
                                 ifelse(spouseHRS$r_retirement=="1.Completely retired"|
                                          spouseHRS$r_retirement=="2.Partly retired", "Yes", NA))
spouseHRS$r_retirement <- factor(spouseHRS$r_retirement, levels = c("No", "Yes"))

spouseHRS$s_retirement <- ifelse(spouseHRS$s_retirement=="0.Not retired", "No",
                                 ifelse(spouseHRS$s_retirement=="1.Completely retired"|
                                          spouseHRS$s_retirement=="2.Partly retired", "Yes", NA))
spouseHRS$s_retirement <- factor(spouseHRS$s_retirement, levels = c("No", "Yes"))
table(spouseHRS$s_retirement)

#sbp
table(spouseHRS$r_sbp)
summary(spouseHRS$r_sbp)
summary(spouseHRS$s_sbp)

#dbp
summary(spouseHRS$r_dbp)
summary(spouseHRS$s_dbp)

#diagnosed bp
table(spouseHRS$r_diagnosed_bp)
spouseHRS$r_diagnosed_bp <- ifelse(spouseHRS$r_diagnosed_bp=="0.No", "No", "Yes")
spouseHRS$r_diagnosed_bp <- factor(spouseHRS$r_diagnosed_bp, levels = c("No", "Yes"))

spouseHRS$s_diagnosed_bp <- ifelse(spouseHRS$s_diagnosed_bp=="0.No", "No", "Yes")
spouseHRS$s_diagnosed_bp <- factor(spouseHRS$s_diagnosed_bp, levels = c("No", "Yes"))
table(spouseHRS$s_diagnosed_bp)

#diagnosed dm
table(spouseHRS$r_diagnosed_dm)
spouseHRS$r_diagnosed_dm <- ifelse(spouseHRS$r_diagnosed_dm=="0.No", "No", "Yes")
spouseHRS$r_diagnosed_dm <- factor(spouseHRS$r_diagnosed_dm, levels = c("No", "Yes"))

spouseHRS$s_diagnosed_dm <- ifelse(spouseHRS$s_diagnosed_dm=="0.No", "No", "Yes")
spouseHRS$s_diagnosed_dm <- factor(spouseHRS$s_diagnosed_dm, levels = c("No", "Yes"))
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

#waistcircumsference
summary(spouseHRS$r_waistcircumference)
hist(spouseHRS$r_waistcircumference)
summary(spouseHRS$s_waistcircumference)

#smoke ever, smoke now, drink ever
table(spouseHRS$r_smokeever)
colnames(spouseHRS)
spouseHRS[, 35:40] <- lapply(spouseHRS[, 35:40], function(x) ifelse(x=="0.No", "No", "Yes"))
spouseHRS[, 35:40] <- lapply(spouseHRS[, 35:40], function(x) factor(x, levels = c("No", "Yes")))

#physical activity
table(spouseHRS$r_physicalactivity)
spouseHRS$r_physicalactivity <- ifelse(spouseHRS$r_physicalactivity=="5.Never"|
                                         spouseHRS$r_physicalactivity=="4.l-3 per mon", "No", "Yes")
spouseHRS$r_physicalactivity <- factor(spouseHRS$r_physicalactivity, levels = c("No", "Yes"))

spouseHRS$s_physicalactivity <- ifelse(spouseHRS$s_physicalactivity=="5.Never"|
                                         spouseHRS$s_physicalactivity=="4.l-3 per mon", "No", "Yes")
spouseHRS$s_physicalactivity <- factor(spouseHRS$s_physicalactivity, levels = c("No", "Yes"))
table(spouseHRS$s_physicalactivity)

#children
table(spouseHRS$hh_children)

#hhsize
table(spouseHRS$hh_size)

#hhincome
summary(spouseHRS$hh_income)

#hhwealth

#length of current marriage
summary(spouseHRS$hh_lengthmar)

#if first marriage
table(spouseHRS$hh_firstmar)
spouseHRS$hh_firstmar <- ifelse(spouseHRS$hh_firstmar==1, "Yes", "No")

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

#------------------------------------------------------------------------
#reorganize the data using naming w_ and h_
#spilit the data based on respondent's sex
table(spouseHRS$r_gender)
femaledata <- spouseHRS[which(spouseHRS$r_gender=="2.Female"), ]
maledata <- spouseHRS[which(spouseHRS$r_gender=="1.Male"), ]

#rename femaledata
colnames(femaledata)
names(femaledata)[11:42] <- c("w_age", "h_age",
                                "w_education", "h_education",
                                "w_employment", "h_employment",
                                "w_retirement", "h_retirement",
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
                                "w_physicalactivity", "h_physicalactivity")

#rename maledata
colnames(maledata)
names(maledata)[11:42] <- c("h_age", "w_age",
                              "h_education", "w_education",
                              "h_employment", "w_employment",
                              "h_retirement", "w_retirement",
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
                              "h_physicalactivity", "w_physicalactivity")

#remerge the femaledata and maledata
coupleHRS <- rbind(femaledata, maledata)
colnames(coupleHRS)

#-------------------------------------------
#descriptive Table 1
table1_US = CreateTableOne(vars = c("w_age", "h_age",
                                    "w_education", "h_education",
                                    "w_employment", "h_employment",
                                    "w_retirement", "h_retirement",
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
                                    "w_physicalactivity", "h_physicalactivity",
                                    "hh_children" , "hh_size", "hh_income",
                                    "hh_wealth", "hh_lengthmar", "hh_firstmar" ), 
                           data=coupleHRS)
print(table1_US, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "Descriptive charateristic (US)",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")
