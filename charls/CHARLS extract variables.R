library(dplyr)
library(ggplot2)
library(foreign)
library(haven)

rm(list = ls())

#Read in dataset
CHARLS <- read_dta("G:\\My Drive\\CHARLS projects\\CHARLS H Data 2021\\H_CHARLS_D_Data\\H_CHARLS_D_Data.dta")

#-----------------------------------------------------------------
#demographic

names(CHARLS)
demog <- subset(CHARLS, select = c(ID, householdID, pn, s3pn, h3coupid, # ID
                                   r3wthha, r3wtresp, s3wtresp,# weights
                                   h3cpl, r3mstat, # if couple household, marital status
                                   r3hukou, s3hukou, # household registeration system
                                   h3rural, s3rural2, # rural vs. urban residence
                                   ragender, s3gender, # gender
                                   r3agey, s3agey, # age
                                   raeduc_c, s3educ_c, # levels/years of education
                                   r3work, s3work, # employment, currently working 
                                   r3fret_c, s3fret_c# retirement, consider self-retired
                                   ))

names(demog) <- c("ID", "hhid", "pid", "spid", "hhcoupid",
                  "hh_weight", "r_indweight", "s_indweight",
                  "ifcouplehh", "mstat",
                  "r_hukou", "s_hukou",
                  "r_rural", "s_rural",
                  "r_gender", "s_gender",
                  "r_age", "s_age",
                  "r_education", "s_education",
                  "r_employment", "s_employment",
                  "r_retirement", "s_retirement")

#health measures
health <- subset(CHARLS, select = c(r3systo, s3systo, # systolic blood pressure
                                    r3diasto, s3diasto, # diastolic blood pressure
                                    r3hibpe, s3hibpe, # ever had high blood pressure
                                    r3diabe, s3diabe, # ever had diabetes
                                    r3mheight, s3mheight, # measured height in meters
                                    r3mweight, s3mheight, # measured weight in kilograms
                                    r3mbmi, s3mbmi, # measured body mass index=kg/m2
                                    r3mwaist, s3mwaist # measured waist circumference
                                    ))

#retained the naming of r_ and s_ for now
names(health) <- c("r_sbp", "s_sbp",
                   "r_dbp", "s_dbp",
                   "r_diagnosed_bp", "s_diagnosed_bp",
                   "r_diagnosed_dm", "s_diagnosed_dm",
                   "r_height", "s_height",
                   "r_weight", "s_weight",
                   "r_bmi", "s_bmi",
                   "r_waistcircumference", "s_waistcircumference")

#health-related covaraites
healthcov <- subset(CHARLS, select = c(r3smokev, s3smokev, # smoke ever
                                        r3smoken, s3smoken, # smokes now
                                        r3drinkev, s3drinkev, # ever drinks any alcohol
                                        r3vgact_c, s3vgact_c # freq vigorous phys activ {finer scale}
                                        ))

names(healthcov) <- c("r_smokeever", "s_smokeever",
                      "r_smokecurr", "s_smokecurr",
                      "r_alcohol", "s_alcohol",
                      "r_physicalactivity", "s_physicalactivity")

# household covariates
hhcov <- subset(CHARLS, select = c(h3child, h3hhres, 
                                   hh3itot, h3atotb 
                                   ))

names(hhcov) <- c("hh_children", "hh_size", 
                  "hh_income","hh_wealth")

#merge all variables
spouseCHARLS_extracted <- cbind(demog, health, healthcov, hhcov)
write.csv(spouseCHARLS_extracted, "G:\\My Drive\\Crossnation study\\Dataset\\spouseCHARLS_extracted.csv")
