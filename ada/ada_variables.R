# Variables --------
w_selected = c("w_diagnosed_dm","w_medication_bp","w_medication_dm","w_htn","w_bmi","w_sbp","w_dbp","w_age","w_ge65","w_education_2", "w_education_3", "w_laborforce", "w_smoke", "w_heavydrinker", "w_moderate_pa", "w_vigorous_pa")
h_selected = c("h_diagnosed_dm","h_medication_bp","h_medication_dm","h_htn","h_bmi","h_sbp","h_dbp","h_age","h_ge65", "h_education_2", "h_education_3", "h_laborforce", "h_smoke", "h_heavydrinker", "h_moderate_pa", "h_vigorous_pa")

id_selected = c("psu","strata","h_sampleweight","w_sampleweight","hh_sampleweight","r_indweight")
hh_selected = c(
  "hh_low", "hh_medium", "hh_high", "hh_highest", "hh_size", "hh_children", "residence", "hh_lengthmar")
interaction_terms <- c("w_htn_residence","h_htn_residence",
                       "w_htn_h_education_2","w_htn_h_education_3",
                       "h_htn_w_education_2","h_htn_w_education_3",
                       "w_htn_h_ge65","h_htn_w_ge65",
                       "w_htn_hh_low","w_htn_hh_medium","w_htn_hh_high","w_htn_hh_highest",
                       "h_htn_hh_low","h_htn_hh_medium","h_htn_hh_high","h_htn_hh_highest"
)