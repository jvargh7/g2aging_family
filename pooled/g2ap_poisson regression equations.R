# # HRS
# w_covariates = "+ w_bmi + w_ge65 + w_education_2 + w_education_3 + w_smoke + w_heavydrinker + w_moderate_pa + w_vigorous_pa + w_religion + w_laborforce + w_raceethnic"
# h_covariates = "+ h_bmi + h_ge65 + h_education_2 + h_education_3 + h_smoke + h_heavydrinker + h_moderate_pa + h_vigorous_pa +h_religion + h_laborforce + h_raceethnic"
# hh_covariates = "+ hh_low + hh_medium + hh_high + hh_highest + hh_size + hh_children + residence + hh_lengthmar"
# 
# 
# # ELSA
# w_covariates = "+ w_bmi + w_age + w_education_2 + w_education_3 + w_laborforce + w_smoke + w_moderate_pa + w_vigorous_pa + w_race + w_religion + w_heavydrinker"
# h_covariates = "+ h_bmi + h_age + h_education_2 + h_education_3 + h_laborforce + h_smoke + h_moderate_pa + h_vigorous_pa + h_race + h_religion + h_heavydrinker"
# hh_covariates = "+ hh_low + hh_medium + hh_high + hh_highest + hh_consumptionquintile + hh_size + hh_lengthmar"
# 
# # CHARLS
# w_covariates = "+ w_bmi + w_ge65 + w_education_2 + w_education_3 + w_smoke + w_heavydrinker + w_moderate_pa + w_vigorous_pa + w_laborforce + w_hukou"
# h_covariates = "+ h_bmi + h_ge65 + h_education_2 + h_education_3 + h_smoke + h_heavydrinker + h_moderate_pa + h_vigorous_pa + h_laborforce + h_hukou"
# hh_covariates = "+ hh_low + hh_medium + hh_high + hh_highest + hh_size + hh_children + residence + hh_lengthmar"
# 
# 
# # LASI
# w_covariates = "+ w_bmi + w_age + w_education_2 + w_education_3 + w_laborforce + w_smoke + w_heavydrinker + w_moderate_pa + w_vigorous_pa"
# h_covariates = "+ h_bmi + h_age + h_education_2 + h_education_3 + h_laborforce + h_smoke + h_heavydrinker + h_moderate_pa + h_vigorous_pa"
# hh_covariates = "+ hh_low + hh_medium + hh_high + hh_highest + hh_consumptionquintile + hh_size + hh_lengthmar + residence + in_religion  + in_caste"


# HARMONIZED 
wp_covariates = "+ w_bmi + w_ge65 + w_education_2 + w_education_3 + w_laborforce + w_smoke + w_heavydrinker + w_moderate_pa + w_vigorous_pa"
hp_covariates = "+ h_bmi + h_ge65 + h_education_2 + h_education_3 + h_laborforce + h_smoke + h_heavydrinker + h_moderate_pa + h_vigorous_pa"
hhp_covariates = "+ hh_low + hh_medium + hh_high + hh_highest + hh_size + residence + hh_lengthmar"



wp1 <- paste0("w_htn ~ h_htn + country",wp_covariates,hhp_covariates) %>% as.formula()
hp1 <- paste0("h_htn ~ w_htn + country",hp_covariates,hhp_covariates) %>% as.formula()

wp2 <- paste0("w_htn ~ h_htn*country",wp_covariates,hhp_covariates) %>% as.formula()
hp2 <- paste0("h_htn ~ w_htn*country",hp_covariates,hhp_covariates) %>% as.formula()

# Lists for models --------

overall_wp1 = list()
overall_hp1 = list()

overall_wp2 = list()
overall_hp2 = list()
