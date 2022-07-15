#-----------------------------------------------------------------
#rand-HRS
library(foreign)
memory.limit(18144)

#my laptop
RANDHRS <- read.spss("E:/HRS data/randhrs1992_2018v1.sav")

#CU laptop
RANDHRS <- read.spss("C:/HRS/randhrs1992_2018v1.sav")

RANDHRS <- as.data.frame(RANDHRS)

#-----------------------------------------------------------------
#demographic
demog <- subset(RANDHRS, select = c(HHIDPN, H13HHID, S13HHIDPN, # ID
                                    R13WTHH, R13WTRESP, S13WTRESP, # weights
                                    H13CPL, R13MSTAT, # if couple household, marital status
                                    RAGENDER, S13GENDER, # gender
                                    R13AGEY_E, S13AGEY_E, # age
                                    RAEDYRS, S13EDYRS, # years of education
                                    R13WORK, S13WORK, # employment, currently working for pay
                                    R13SAYRET, S13SAYRET# retirement, consider self-retired
                                    ))
names(demog) <- c("HHIDPN", "hhid", "S13HHIDPN",
                  "hh_weight", "r_indweight", "s_indweight",
                  "ifcouplehh", "MSTAT",
                  "r_gender", "s_gender",
                  "r_age", "s_age",
                  "r_education", "s_education",
                  "r_employment", "s_employment",
                  "r_retirement", "s_retirement")

#health measures
health <- subset(RANDHRS, select = c(R13BPSYS, S13BPSYS, # systolic blood pressure
                                     R13BPDIA, S13BPDIA, # diastolic blood pressure
                                     R13HIBPE, S13HIBPE, # ever had high blood pressure
                                     R13DIABE, S13DIABE, # ever had diabetes
                                     R13HEIGHT, S13HEIGHT, # Self-reported height in meters
                                     R13WEIGHT, S13WEIGHT, # Self-reported weight in kilograms
                                     R13BMI, S13BMI, # Self-reported body mass index=kg/m2
                                     R13PMWAIST, S13PMWAIST # physical measure of waist
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
healthcov <- subset(RANDHRS, select = c(R13SMOKEV, S13SMOKEV, # smoke ever
                                        R13SMOKEN, S13SMOKEN, # smokes now
                                        R13DRINK, S13DRINK, # ever drinks any alcohol
                                        R13VGACTX, S13VGACTX # Freq vigorous phys activ {finer scale}
                                        ))
names(healthcov) <- c("r_smokeever", "s_smokeever",
                      "r_smokecurr", "s_smokecurr",
                      "r_alcohol", "s_alcohol",
                      "r_physicalactivity", "s_physicalactivity")

# household covariates
hhcov <- subset(RANDHRS, select = c(H13CHILD, H13HHRES, 
                                    H13ITOT, H13ATOTB, 
                                    R13MCURLN, R13MRCT))
names(hhcov) <- c("hh_children", "hh_size", 
                  "hh_income","hh_wealth", 
                  "hh_lengthmar", "hh_firstmar")

#merge all variables
spouseHRS <- cbind(demog, health, healthcov, hhcov)
write.csv(spouseHRS, "E:/files shared by PC & OC/OneDrive/spouse concord China US India/Data/spouseHRS.csv")
