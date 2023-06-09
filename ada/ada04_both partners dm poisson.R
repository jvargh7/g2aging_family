source("ada/ada_variables.R")

hrs_mi <- readRDS(paste0(path_g2a_concordance_folder,"/working/mi_hrs.RDS"))
elsa_mi <- readRDS(paste0(path_g2a_concordance_folder,"/working/mi_elsa.RDS"))
charls_mi <- readRDS(paste0(path_g2a_concordance_folder,"/working/mi_charls.RDS"))
lasi_mi <- readRDS(paste0(path_g2a_concordance_folder,"/working/mi_lasi.RDS"))


source("ada/ada_equations.R")

library(mice)
library(srvyr)
library(survey)

for(i in 1:lasi_mi$m){
  df = bind_rows(
    complete(hrs_mi,action = i) %>% 
      dplyr::filter(w_diagnosed_dm == 1, h_diagnosed_dm == 1) %>% 
      # dplyr::rename(hh_sampleweight = r_indweight) %>% 
      dplyr::select(one_of(id_selected),one_of(w_selected),one_of(h_selected),one_of(hh_selected),h_bp_control,w_bp_control) %>% 
      mutate(country = 1,
             w_sampleweight = hh_sampleweight,
             h_sampleweight = hh_sampleweight),
    complete(elsa_mi,action = i) %>% 
      dplyr::filter(w_diagnosed_dm == 1, h_diagnosed_dm == 1) %>% 
      dplyr::select(one_of(id_selected),one_of(w_selected),one_of(h_selected),one_of(hh_selected),h_bp_control,w_bp_control) %>% 
      mutate(country = 2,
             residence = 0.5),
    complete(charls_mi,action = i) %>% 
      dplyr::filter(w_diagnosed_dm == 1, h_diagnosed_dm == 1) %>% 
      # dplyr::rename(hh_sampleweight = r_indweight) %>% 
      dplyr::select(one_of(id_selected),one_of(w_selected),one_of(h_selected),one_of(hh_selected),h_bp_control,w_bp_control) %>% 
      mutate(country = 3,
             w_sampleweight = hh_sampleweight,
             h_sampleweight = hh_sampleweight) %>% 
      mutate_at(vars(w_htn,h_htn,w_heavydrinker,h_heavydrinker), function(x) case_when(x == 1 ~ 1,
                                                                                       x == 0 ~ 0,
                                                                                       TRUE ~ NA_real_)),
    complete(lasi_mi,action = i) %>% 
      dplyr::filter(w_diagnosed_dm == 1, h_diagnosed_dm == 1) %>% 
      dplyr::select(one_of(id_selected),one_of(w_selected),one_of(h_selected),one_of(hh_selected),h_bp_control,w_bp_control) %>% 
      mutate(country = 4,
             w_sampleweight = h_sampleweight)
  ) %>% 
    mutate(country = factor(country,levels=c(1:4),labels=c("HRS","ELSA","CHARLS","LASI"))) %>% 
    mutate_at(vars(w_smoke,h_smoke),function(x) case_when(x == "former" ~ "Former",
                                                          x == "never" ~ "Never",
                                                          x == "current" ~ "Current",
                                                          TRUE ~ as.character(x))) %>% 
    mutate_at(vars(w_laborforce,h_laborforce), function(x) case_when(x == "None" ~ "Other",
                                                                     x == "other" ~ "Other",
                                                                     x == "retired" ~ "Retired",
                                                                     x == "employed" ~ "Employed",
                                                                     x == "Formal" ~ "Employed",
                                                                     TRUE ~ as.character(x)));
  
  
  svy_des_wives = df  %>% 
    dplyr::filter(w_diagnosed_dm == 1) %>% 
    dplyr::filter(!is.na(w_sampleweight), w_sampleweight > 0) %>% 
    group_by(country) %>% 
    mutate(w_sampleweight = w_sampleweight/sum(w_sampleweight)) %>% 
    mutate(w_sampleweight = w_sampleweight/n()) %>% 
    ungroup() %>% 
    mutate(w_sampleweight = w_sampleweight*n()) %>% 
    
    # Need to impute and correct
    as_survey_design(.data = .,
                     # ids = psu,
                     # strata = strata,
                     weight = w_sampleweight,
                     nest = TRUE,
                     variance = "YG",pps = "brewer");
  
  svy_des_husbands = df  %>% 
    dplyr::filter(h_diagnosed_dm == 1) %>% 
    dplyr::filter(!is.na(h_sampleweight), h_sampleweight > 0) %>% 
    group_by(country) %>% 
    mutate(h_sampleweight = h_sampleweight/sum(h_sampleweight)) %>% 
    mutate(h_sampleweight = h_sampleweight/n()) %>% 
    ungroup() %>% 
    mutate(h_sampleweight = h_sampleweight*n()) %>% 
    # Need to impute and correct
    as_survey_design(.data = .,
                     # ids = psu,
                     # strata = strata,
                     weight = h_sampleweight,
                     nest = TRUE,
                     variance = "YG",pps = "brewer");
  
  overall_wa1[[i]] = svyglm(wa1,design=svy_des_wives,family=quasipoisson());
  overall_ha1[[i]] = svyglm(ha1,design=svy_des_husbands,family=quasipoisson());
  # overall_wa2[[i]] = svyglm(wa2,design=svy_des_wives,family=quasipoisson());
  # overall_ha2[[i]] = svyglm(ha2,design=svy_des_husbands,family=quasipoisson());
  
  
  gc();rm(df);rm(svy_des)
}


# Pooling coefficients ------------
source("C:/code/external/functions/survey/mice_coef_svyglm.R")
# Check https://github.com/jvargh7/functions/blob/main/survey/mice_coef_svyglm.R
# You would also have to download the following:
# a. https://github.com/jvargh7/functions/blob/main/imputation/adjusted_ci.R
# b. https://github.com/jvargh7/functions/tree/main/preprocessing
overall_wa1_out = mice_coef_svyglm(overall_wa1)
overall_ha1_out = mice_coef_svyglm(overall_ha1)
# overall_wa2_out = mice_coef_svyglm(overall_wa2)
# overall_ha2_out = mice_coef_svyglm(overall_ha2)

bind_rows(
  overall_wa1_out %>% mutate(model = "WA1"),
  overall_ha1_out %>% mutate(model = "HA1")
  # overall_wa2_out %>% mutate(model = "WA2"),
  # overall_ha2_out %>% mutate(model = "HA2")
  ) %>% 
  write_csv(.,"ada/ada04_both partners dm poisson regression with multiple imputation.csv")

# source("C:/code/external/functions/survey/mice_contrasts_svyglm.R")
# # Check: https://github.com/jvargh7/functions/blob/main/survey/mice_contrasts_svyglm.R
# # You would also have to download the following:
# # https://github.com/jvargh7/functions/blob/main/preprocessing/prepare_contrasts.R
# # https://github.com/jvargh7/functions/blob/main/survey/contrasts_svyglm.R
# # The coefficients are in column 'RR'. The rest of the columns are for pooling multiple imputated regressions
# 
# contrasts_wa2_out_ELSA = mice_contrasts_svyglm(svymodel_list = overall_wa2,modifier = "countryELSA",exposure = "h_bp_control")
# contrasts_ha2_out_ELSA = mice_contrasts_svyglm(svymodel_list = overall_ha2,modifier="countryELSA",exposure="w_bp_control")
# 
# contrasts_wa2_out_CHARLS = mice_contrasts_svyglm(svymodel_list = overall_wa2,modifier = "countryCHARLS",exposure = "h_bp_control")
# contrasts_ha2_out_CHARLS = mice_contrasts_svyglm(svymodel_list = overall_ha2,modifier="countryCHARLS",exposure="w_bp_control")
# 
# contrasts_wa2_out_LASI = mice_contrasts_svyglm(svymodel_list = overall_wa2,modifier = "countryLASI",exposure = "h_bp_control")
# contrasts_ha2_out_LASI = mice_contrasts_svyglm(svymodel_list = overall_ha2,modifier="countryLASI",exposure="w_bp_control")
# 
# bind_rows(
#   contrasts_wa2_out_ELSA %>% mutate(model = "WA2 ELSA"),
#   contrasts_ha2_out_ELSA %>% mutate(model = "HA2 ELSA"),
#   
#   contrasts_wa2_out_CHARLS %>% mutate(model = "WA2 CHARLS"),
#   contrasts_ha2_out_CHARLS %>% mutate(model = "HA2 CHARLS"),
#   
#   contrasts_wa2_out_LASI %>% mutate(model = "WA2 LASI"),
#   contrasts_ha2_out_LASI %>% mutate(model = "HA2 LASI")
# ) %>% 
#   write_csv(.,"ada/ada04_contrasts for both partners dm poisson regression.csv")
# 
