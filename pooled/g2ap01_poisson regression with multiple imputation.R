hrs_mi <- readRDS(paste0(path_g2a_family_folder,"/working/hrs/G2A HRS Couples mi_dfs.RDS"))
elsa_mi <- readRDS(paste0(path_g2a_family_folder,"/working/elsa/G2A ELSA Couples mi_dfs.RDS"))
charls_mi <- readRDS(paste0(path_g2a_family_folder,"/working/charls/G2A CHARLS Couples mi_dfs.RDS"))
lasi_mi <- readRDS(paste0(path_g2a_family_folder,"/working/lasi/G2A LASI Couples mi_dfs.RDS"))


w_selected = c("w_htn","w_bmi","w_ge65","w_education_2", "w_education_3", "w_laborforce", "w_smoke", "w_heavydrinker", "w_moderate_pa", "w_vigorous_pa")
h_selected = c("h_htn","h_bmi", "h_ge65", "h_education_2", "h_education_3", "h_laborforce", "h_smoke", "h_heavydrinker", "h_moderate_pa", "h_vigorous_pa")
hh_selected = c("psu","strata","h_sampleweight","w_sampleweight","hh_sampleweight","r_indweight",
                "hh_low", "hh_medium", "hh_high", "hh_highest", "hh_size", "hh_children", "residence", "hh_lengthmar")


source("pooled/g2ap_poisson regression equations.R")

require(mice)
require(srvyr)
require(survey)

for(i in 1:lasi_mi$m){
  df = bind_rows(
    complete(hrs_mi,action = i) %>% 
      dplyr::rename(hh_sampleweight = r_indweight) %>% 
      dplyr::select(one_of(w_selected),one_of(h_selected),one_of(hh_selected)) %>% 
      mutate(country = 1,
             w_sampleweight = hh_sampleweight,
             h_sampleweight = hh_sampleweight),
    complete(elsa_mi,action = i) %>% 
      dplyr::select(one_of(w_selected),one_of(h_selected),one_of(hh_selected)) %>% 
      mutate(country = 2,
             residence = 0.5),
    complete(charls_mi,action = i) %>% 
      dplyr::rename(hh_sampleweight = r_indweight) %>% 
      dplyr::select(one_of(w_selected),one_of(h_selected),one_of(hh_selected)) %>% 
      mutate(country = 3,
             w_sampleweight = hh_sampleweight,
             h_sampleweight = hh_sampleweight) %>% 
      mutate_at(vars(w_htn,h_htn,w_heavydrinker,h_heavydrinker), function(x) case_when(x == 1 ~ 1,
                                                         x == 0 ~ 0,
                                                         TRUE ~ NA_real_)),
    complete(lasi_mi,action = i) %>% 
      dplyr::select(one_of(w_selected),one_of(h_selected),one_of(hh_selected)) %>% 
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
    dplyr::filter(!is.na(h_sampleweight), h_sampleweight > 0) %>% 
    group_by(country) %>% 
    mutate(h_sampleweight = h_sampleweight/sum(h_sampleweight)) %>% 
    mutate(h_sampleweight = h_sampleweight/n()) %>% 
    ungroup() %>% 
    mutate(h_sampleweight = h_sampleweight*n())%>% 
    # Need to impute and correct
    as_survey_design(.data = .,
                     # ids = psu,
                     # strata = strata,
                     weight = h_sampleweight,
                     nest = TRUE,
                     variance = "YG",pps = "brewer");
  
  overall_wp1[[i]] = svyglm(wp1,design=svy_des_wives,family=quasipoisson());
  overall_hp1[[i]] = svyglm(hp1,design=svy_des_husbands,family=quasipoisson());
  overall_wp2[[i]] = svyglm(wp2,design=svy_des_wives,family=quasipoisson());
  overall_hp2[[i]] = svyglm(hp2,design=svy_des_husbands,family=quasipoisson());
  
  
  gc();rm(df);rm(svy_des)
}


# Pooling coefficients ------------
source("C:/code/external/functions/survey/mice_coef_svyglm.R")
# Check https://github.com/jvargh7/functions/blob/main/survey/mice_coef_svyglm.R
# You would also have to download the following:
# a. https://github.com/jvargh7/functions/blob/main/imputation/adjusted_ci.R
# b. https://github.com/jvargh7/functions/tree/main/preprocessing
overall_wp1_out = mice_coef_svyglm(overall_wp1)
overall_hp1_out = mice_coef_svyglm(overall_hp1)
overall_wp2_out = mice_coef_svyglm(overall_wp2)
overall_hp2_out = mice_coef_svyglm(overall_hp2)

bind_rows(
  overall_wp1_out %>% mutate(model = "WP1"),
  overall_hp1_out %>% mutate(model = "HP1"),
  overall_wp2_out %>% mutate(model = "WP2"),
  overall_hp2_out %>% mutate(model = "HP2")) %>% 
  write_csv(.,"pooled/g2ap01_poisson regression with multiple imputation.csv")

source("C:/code/external/functions/survey/mice_contrasts_svyglm.R")
# Check: https://github.com/jvargh7/functions/blob/main/survey/mice_contrasts_svyglm.R
# You would also have to download the following:
# https://github.com/jvargh7/functions/blob/main/preprocessing/prepare_contrasts.R
# https://github.com/jvargh7/functions/blob/main/survey/contrasts_svyglm.R
# The coefficients are in column 'RR'. The rest of the columns are for pooling multiple imputated regressions

contrasts_wp2_out_ELSA = mice_contrasts_svyglm(svymodel_list = overall_wp2,modifier = "countryELSA",exposure = "h_htn")
contrasts_hp2_out_ELSA = mice_contrasts_svyglm(svymodel_list = overall_hp2,modifier="countryELSA",exposure="w_htn")

contrasts_wp2_out_CHARLS = mice_contrasts_svyglm(svymodel_list = overall_wp2,modifier = "countryCHARLS",exposure = "h_htn")
contrasts_hp2_out_CHARLS = mice_contrasts_svyglm(svymodel_list = overall_hp2,modifier="countryCHARLS",exposure="w_htn")

contrasts_wp2_out_LASI = mice_contrasts_svyglm(svymodel_list = overall_wp2,modifier = "countryLASI",exposure = "h_htn")
contrasts_hp2_out_LASI = mice_contrasts_svyglm(svymodel_list = overall_hp2,modifier="countryLASI",exposure="w_htn")

bind_rows(
  contrasts_wp2_out_ELSA %>% mutate(model = "WP2 ELSA"),
  contrasts_hp2_out_ELSA %>% mutate(model = "HP2 ELSA"),
  
  contrasts_wp2_out_CHARLS %>% mutate(model = "WP2 CHARLS"),
  contrasts_hp2_out_CHARLS %>% mutate(model = "HP2 CHARLS"),
  
  contrasts_wp2_out_LASI %>% mutate(model = "WP2 LASI"),
  contrasts_hp2_out_LASI %>% mutate(model = "HP2 LASI")
) %>% 
  write_csv(.,"pooled/g2ap01_contrasts for poisson regression with multiple imputation.csv")

