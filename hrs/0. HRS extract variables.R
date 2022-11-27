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
                                    RAESTRAT, RAEHSAMP, #sampling weight
                                    R13WTHH, R13WTRESP, S13WTRESP, # weights
                                    H13CPL, R13MSTAT, # if couple household, marital status
                                    RAGENDER, S13GENDER, # gender
                                    R13AGEY_E, S13AGEY_E, # age
                                    RAEDYRS, S13EDYRS, # years of education
                                    R13LBRF, S13LBRF, # labor force status
                                    RARACEM, S13RACEM,# race
                                    RAHISPAN, S13HISPAN, # if Hispanic
                                    RARELIG, S13RELIG# religion
                                    ))
names(demog) <- c("HHIDPN", "hhid", "S13HHIDPN",
                  "strata_weight", "PSU_weight",
                  "hh_weight", "r_indweight", "s_indweight",
                  "ifcouplehh", "MSTAT",
                  "r_gender", "s_gender",
                  "r_age", "s_age",
                  "r_education", "s_education",
                  "r_laborforce", "s_laborforce",
                  "r_race", "s_race",
                  "r_hispanic", "s_hispanic",
                  "r_religion", "s_religion")

#health measures
health <- subset(RANDHRS, select = c(R12BPSYS, S12BPSYS, # 2016 systolic blood pressure
                                     R12BPDIA, S12BPDIA, # 2016 diastolic blood pressure
                                     R13BPSYS, S13BPSYS, # 2018 systolic blood pressure
                                     R13BPDIA, S13BPDIA, # 2018 diastolic blood pressure
                                     R13HIBPE, S13HIBPE, # ever had high blood pressure
                                     R13DIABE, S13DIABE, # ever had diabetes
                                     R13HEIGHT, S13HEIGHT, # Self-reported height in meters
                                     R13WEIGHT, S13WEIGHT, # Self-reported weight in kilograms
                                     R13BMI, S13BMI, # Self-reported body mass index=kg/m2
                                     R13PMWAIST, S13PMWAIST # physical measure of waist
                                     ))
#because biomakers only do 50% random sample each wave. To get full biomarker, we need to merge data from previous wave, yr 2016
#create new BP measures, if BP in 2018 is missing, use BP in 2016
health$r_sbp <- ifelse(!is.na(health$R13BPSYS), health$R13BPSYS, health$R12BPSYS)
health$s_sbp <- ifelse(!is.na(health$S13BPSYS), health$S13BPSYS, health$S12BPSYS)

health$r_dbp <- ifelse(!is.na(health$R13BPDIA), health$R13BPDIA, health$R12BPDIA)
health$s_dbp <- ifelse(!is.na(health$S13BPDIA), health$S13BPDIA, health$S12BPDIA)

colnames(health)
#drop the first 4 columns
health <- health[, -c(1:8)]

#retained the naming of r_ and s_ for now
names(health) <- c("r_diagnosed_bp", "s_diagnosed_bp",
                   "r_diagnosed_dm", "s_diagnosed_dm",
                   "r_height", "s_height",
                   "r_weight", "s_weight",
                   "r_bmi", "s_bmi",
                   "r_waistcircumference", "s_waistcircumference",
                   "r_sbp", "s_sbp",
                   "r_dbp", "s_dbp")

#health-related covaraites
healthcov <- subset(RANDHRS, select = c(R13SMOKEV, S13SMOKEV, # smoke ever
                                        R13SMOKEN, S13SMOKEN, # smokes now
                                        R13DRINKD, S13DRINKD, # # days/week drinks
                                        R13MDACTX, S13MDACTX, #  Freq moderate phys activ {finer scale}
                                        R13VGACTX, S13VGACTX # Freq vigorous phys activ {finer scale}
                                        ))
names(healthcov) <- c("r_smokeever", "s_smokeever",
                      "r_smokecurr", "s_smokecurr",
                      "r_alcohol", "s_alcohol",
                      "r_moderate_pa", "s_moderate_pa",
                      "r_vigorous_pa", "s_vigorous_pa")

# household covariates
hhcov <- subset(RANDHRS, select = c(H13CHILD, H13HHRES, 
                                    H13ITOT, H13ATOTB, 
                                    R13MCURLN, R13MRCT))
names(hhcov) <- c("hh_children", "hh_size", 
                  "hh_income","hh_wealth", 
                  "hh_lengthmar", "hh_firstmar")


#---------------------------------------------------------------
#harmonized HRS
HHRS <- read.spss("Data/H_HRS_c.sav")
HHRS <- as.data.frame(HHRS)

harmcov <- subset(HHRS, select=c(hhidpn, 
                                 raeducl, s13educl, # harmonized education level
                                 h13rural # w13 lives in rural or urban
                                 ))
names(harmcov) <- c("HHIDPN",
                    "r_education_h", "s_education_h",
                    "residence")


#merge all variables
randcov <- cbind(demog, health, healthcov, hhcov)
spouseHRS <- merge(randcov, harmcov, by="HHIDPN")
write.csv(spouseHRS, "Data/spouseHRS.csv")
