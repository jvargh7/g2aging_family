
w_covariates = "+ w_bmi + w_age + w_education_2 + w_education_3 + w_smokecurr + w_alcohol + w_children"
h_covariates = "+ h_bmi + h_age + h_education_2 + h_education_3 + h_smokecurr + h_alcohol + h_children"
hh_covariates = "+ hh_low + hh_medium + hh_high + hh_highest + hh_size + in_caste + residence + in_religion"


w1 <- paste0("w_htn ~ h_htn",w_covariates,hh_covariates) %>% as.formula()
h1 <- paste0("h_htn ~ w_htn",h_covariates,hh_covariates) %>% as.formula()


# Lists for models --------

overall_w1 = list()
overall_h1 = list()
