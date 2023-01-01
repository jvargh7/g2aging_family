# HARMONIZED 
wa_covariates = " + w_bmi + w_age + w_education_2 + w_education_3 + w_laborforce + w_smoke + w_heavydrinker + w_moderate_pa + w_vigorous_pa + w_medication_dm + w_medication_bp"
ha_covariates = " + h_bmi + h_age + h_education_2 + h_education_3 + h_laborforce + h_smoke + h_heavydrinker + h_moderate_pa + h_vigorous_pa + h_medication_dm + h_medication_bp"
hha_covariates = "+ hh_low + hh_medium + hh_high + hh_highest + hh_size + residence + hh_lengthmar"



wa1 <- paste0("w_bp_control ~ h_bp_control + country",wa_covariates,hha_covariates) %>% as.formula()
ha1 <- paste0("h_bp_control ~ w_bp_control + country",ha_covariates,hha_covariates) %>% as.formula()

wa2 <- paste0("w_bp_control ~ h_bp_control*country",wa_covariates,hha_covariates) %>% as.formula()
ha2 <- paste0("h_bp_control ~ w_bp_control*country",ha_covariates,hha_covariates) %>% as.formula()

# Lists for models --------

overall_wa1 = list()
overall_ha1 = list()

overall_wa2 = list()
overall_ha2 = list()
