library(haven)

rm(list = ls())

#-----------------------------------------------------------------
#CHARLS
CHARLS <- read_dta("G:\\My Drive\\CHARLS projects\\CHARLS H Data 2021\\H_CHARLS_D_Data\\H_CHARLS_D_Data.dta")

#-----------------------------------------------------------------
#demographic
names(CHARLS)
demog <- subset(CHARLS, select = c(ID, householdID, pn, h3coupid, # ID
                                   r3wthha, r3wtresp, s3wtresp,# weights
                                   h3cpl, r3mstat, # if couple household, marital status
                                   ragender, s3gender, # gender
                                   r3agey, s3agey, # age
                                   raeducl, s3educl, # harmonized levels of education
                                   r3lbrf_c, s3lbrf_c, # labor force status
                                   r3hukou, s3hukou, # household registeration system
                                   r3rural2 # rural vs. urban residence
                                   ))

names(demog) <- c("ID", "hhid", "pid", "hhcoupid",
                  "hh_weight", "r_indweight", "s_indweight",
                  "ifcouplehh", "MSTAT",
                  "r_gender", "s_gender",
                  "r_age", "s_age",
                  "r_education", "s_education",
                  "r_laborforce", "s_laborforce",
                  "r_hukou", "s_hukou",
                  "residence")

#health measures
health <- subset(CHARLS, select = c(r3systo, s3systo, # 2015 systolic blood pressure
                                    r3diasto, s3diasto, # 2015 diastolic blood pressure
                                    r3hibpe, s3hibpe, # ever had high blood pressure
                                    r3diabe, s3diabe, # ever had diabetes
                                    r3mheight, s3mheight, # measured height in meters
                                    r3mweight, s3mweight, # measured weight in kilograms
                                    r3mbmi, s3mbmi, # measured body mass index=kg/m2
                                    r3mwaist, s3mwaist # measured waist circumference
                                    ))

#retained the naming of r_ and s_ for now: blood pressure only available 2011, 2013, and 2015
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
                                       r3drinkn_c, s3drinkn_c, # freq drinking by month and week
                                       r3vgactx_c, s3vgactx_c, # freq vigorous phys activity days/week
                                       r3mdactx_c, s3mdactx_c # freq moderate phys activity days/week
                                       ))

names(healthcov) <- c("r_smokeever", "s_smokeever",
                      "r_smokecurr", "s_smokecurr",
                      "r_alcohol", "s_alcohol",
                      "r_vigorous_pa", "s_vigorous_pa",
                      "r_moderate_pa", "s_moderate_pa")

# household covariates
hhcov <- subset(CHARLS, select = c(h3child, h3hhres, 
                                   hh3itot, h3atotb, 
                                   r2mcurln, r2mrct
                                   ))

names(hhcov) <- c("hh_children", "hh_size", 
                  "hh_income","hh_wealth",
                  "hh_lengthmar", "hh_firstmar")

#merge all variables
spouseCHARLS <- cbind(demog, health, healthcov, hhcov)
write.csv(spouseCHARLS, "G:\\My Drive\\Crossnation study\\Dataset\\spouseCHARLS.csv")

