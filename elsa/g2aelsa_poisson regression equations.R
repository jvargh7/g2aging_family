# w_heavydrinker + 
w_covariates = "+ w_bmi + w_age + w_education_2 + w_education_3 + w_laborforce + w_smoke + w_moderate_pa + w_vigorous_pa + w_race + w_religion + w_heavydrinker"
h_covariates = "+ h_bmi + h_age + h_education_2 + h_education_3 + h_laborforce + h_smoke + h_moderate_pa + h_vigorous_pa + h_race + h_religion + h_heavydrinker"
hh_covariates = "+ hh_low + hh_medium + hh_high + hh_highest + hh_consumptionquintile + hh_size + hh_lengthmar"
#   + hh_children 
w0 <- paste0("w_htn ~ h_htn") %>% as.formula()
h0 <- paste0("h_htn ~ w_htn") %>% as.formula()


w1 <- paste0("w_htn ~ h_htn",w_covariates,hh_covariates) %>% as.formula()
h1 <- paste0("h_htn ~ w_htn",h_covariates,hh_covariates) %>% as.formula()

w2 <- paste0("w_htn ~ h_htn*w_ge65",w_covariates,hh_covariates) %>% str_replace(.,"\\+ w_age ","") %>% as.formula()
h2 <- paste0("h_htn ~ w_htn*h_ge65",h_covariates,hh_covariates) %>% str_replace(.,"\\+ h_age ","") %>% as.formula()

w3 <- paste0("w_htn ~ h_htn*w_education_2 + h_htn*w_education_3",w_covariates,hh_covariates) %>% 
  str_replace(.,"\\+ w_education_2 \\+ w_education_3","") %>% as.formula()
h3 <- paste0("h_htn ~ w_htn*h_education_2 + w_htn*h_education_3",h_covariates,hh_covariates) %>% 
  str_replace(.,"\\+ h_education_2 \\+ h_education_3","") %>% as.formula()

w5 <- paste0("w_htn ~ h_htn*hh_low + h_htn*hh_medium + h_htn*hh_high + h_htn*hh_highest",w_covariates,hh_covariates) %>% 
  str_replace(.,"\\+ hh_low \\+ hh_medium \\+ hh_high \\+ hh_highest","") %>% as.formula()
h5 <- paste0("h_htn ~ w_htn*hh_low + w_htn*hh_medium + w_htn*hh_high + w_htn*hh_highest",h_covariates,hh_covariates) %>% 
  str_replace(.,"\\+ hh_low \\+ hh_medium \\+ hh_high \\+ hh_highest","") %>% as.formula()

w6 <- paste0("w_htn ~ h_htn*hh_lengthmar_ge10",w_covariates,hh_covariates) %>% str_replace(.,"\\+ hh_lengthmar ","") %>% as.formula()
h6 <- paste0("h_htn ~ w_htn*hh_lengthmar_ge10",h_covariates,hh_covariates) %>% str_replace(.,"\\+ hh_lengthmar ","") %>% as.formula()

w7 <- paste0("w_htn ~ h_htn",w_covariates,hh_covariates,h_covariates) %>% as.formula()
h7 <- paste0("h_htn ~ w_htn",h_covariates,hh_covariates,w_covariates) %>% as.formula()

w8 <- paste0("w_htn ~ h_htn",w_covariates) %>% as.formula()
h8 <- paste0("h_htn ~ w_htn",h_covariates) %>% as.formula()

# Lists for models --------

overall_w0 = list()
overall_h0 = list()

overall_w1 = list()
overall_h1 = list()

overall_w2 = list()
overall_h2 = list()

overall_w3 = list()
overall_h3 = list()

overall_w5 = list()
overall_h5 = list()

overall_w6 = list()
overall_h6 = list()

overall_w7 = list()
overall_h7 = list()

overall_w8 = list()
overall_h8 = list()