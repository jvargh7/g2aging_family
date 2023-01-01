source("ada/ada_variables.R")

hrs_mi <- readRDS(paste0(path_g2a_concordance_folder,"/working/mi_hrs.RDS"))
elsa_mi <- readRDS(paste0(path_g2a_concordance_folder,"/working/mi_elsa.RDS"))
charls_mi <- readRDS(paste0(path_g2a_concordance_folder,"/working/mi_charls.RDS"))
lasi_mi <- readRDS(paste0(path_g2a_concordance_folder,"/working/mi_lasi.RDS"))



df = bind_rows(
  hrs_mi$data %>% 
    # dplyr::rename(hh_sampleweight = r_indweight) %>% 
    dplyr::select(one_of(id_selected),one_of(w_selected),one_of(h_selected),one_of(hh_selected),h_bp_control,w_bp_control) %>% 
    mutate(country = 1,
           w_sampleweight = hh_sampleweight,
           h_sampleweight = hh_sampleweight),
  elsa_mi$data %>% 
    dplyr::select(one_of(id_selected),one_of(w_selected),one_of(h_selected),one_of(hh_selected),h_bp_control,w_bp_control) %>% 
    mutate(country = 2,
           residence = 0.5),
  charls_mi$data %>% 
    # dplyr::rename(hh_sampleweight = r_indweight) %>% 
    dplyr::select(one_of(id_selected),one_of(w_selected),one_of(h_selected),one_of(hh_selected),h_bp_control,w_bp_control) %>% 
    mutate(country = 3,
           w_sampleweight = hh_sampleweight,
           h_sampleweight = hh_sampleweight) %>% 
    mutate_at(vars(w_htn,h_htn,w_heavydrinker,h_heavydrinker), function(x) case_when(x == 1 ~ 1,
                                                                                     x == 0 ~ 0,
                                                                                     TRUE ~ NA_real_)),
  lasi_mi$data %>% 
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
                                                                   TRUE ~ as.character(x)))

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
                   variance = "YG",pps = "brewer")
svy_des_wives %>% 
  group_by(country) %>% 
  summarize(prop_controlled = survey_mean(w_bp_control,na.rm=TRUE,vartype="ci"))

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
                   variance = "YG",pps = "brewer")

svy_des_husbands %>% 
  group_by(country) %>% 
  summarize(prop_controlled = survey_mean(h_bp_control,na.rm=TRUE,vartype="ci"))
