require(lubridate)
g2aelsa_preprocessing <- function(df){
  df %>% 
    # Blood pressure cleaning -------
  mutate(
    diagnosed_dm = case_when(diagnosed_dm == 1 ~ 1,
                             TRUE ~ 0),
    medication_dm = case_when(medication_dm == 1 ~ 1,
                              TRUE ~ 0),
    
    
    
    diagnosed_bp = case_when(diagnosed_bp == 1 ~ 1,
                             TRUE ~ 0),
    medication_bp = case_when(medication_bp == 1 ~ 1,
                              TRUE ~ 0))  %>% 
    
    mutate(bmi_underweight = case_when(bmi > bmi_max ~ NA_real_,
                                       bmi < bmi_cutoff[1] ~ 1,
                                       bmi >= bmi_cutoff[1] ~ 0,
                                       TRUE ~ NA_real_),
           
           
           bmi_overweight = case_when(bmi > bmi_max ~ NA_real_,
                                      bmi >= bmi_cutoff[2] & bmi < bmi_cutoff[3] ~ 1,
                                      bmi < bmi_cutoff[2] | bmi >= bmi_cutoff[3] ~ 0,
                                      TRUE ~ NA_real_),
           
           
           bmi_obese = case_when(bmi > bmi_max ~ NA_real_,
                                 bmi >= bmi_cutoff[3] ~ 1,
                                 bmi < bmi_cutoff[3] ~ 0,
                                 TRUE ~ NA_real_)) %>% 
    
    mutate(sbp_avg = rowMeans(.[,c("sbp1","sbp2","sbp3")],na.rm=TRUE),
           
           dbp_avg = rowMeans(.[,c("dbp1","dbp2","dbp3")],na.rm=TRUE),
           htn = case_when(diagnosed_bp == 1 ~ 1,
                           is.na(sbp) | is.na(dbp) ~ NA_real_,
                           sbp >= sbp_cutoff ~ 1,
                           dbp >= dbp_cutoff ~ 1,
                           sbp < sbp_cutoff ~ 0,
                           dbp < dbp_cutoff ~ 0,
                           TRUE ~ NA_real_),
           highbp = case_when(
             is.na(sbp) | is.na(dbp) ~ NA_real_,
             sbp >= sbp_cutoff ~ 1,
             dbp >= dbp_cutoff ~ 1,
             sbp < sbp_cutoff ~ 0,
             dbp < dbp_cutoff ~ 0,
             TRUE ~ NA_real_),
           # Among those diagnosed, indicator of hypertension control status
           diaghtn = case_when(
             diagnosed_bp == 0 ~ NA_real_,
             is.na(sbp) | is.na(dbp) ~ NA_real_,
             diagnosed_bp == 1 & age <= agebp_cutoff & sbp > sbp_target[1] ~ 1,
             diagnosed_bp == 1 & age <= agebp_cutoff & dbp > dbp_target[1] ~ 1,
             diagnosed_bp == 1 & age <= agebp_cutoff & sbp <= sbp_target[1] ~ 0,
             diagnosed_bp == 1 & age <= agebp_cutoff & dbp <= dbp_target[1] ~ 0,
             
             diagnosed_bp == 1 & age > agebp_cutoff & sbp > sbp_target[2] ~ 1,
             diagnosed_bp == 1 & age > agebp_cutoff & dbp > dbp_target[2] ~ 1,
             diagnosed_bp == 1 & age > agebp_cutoff & sbp <= sbp_target[2] ~ 0,
             diagnosed_bp == 1 & age > agebp_cutoff & dbp <= dbp_target[2] ~ 0,
             
             TRUE ~ NA_real_)
    ) %>% 
    
    # Hypertension cascade -----
  mutate(htn_sample = case_when(!is.na(sbp)|!is.na(dbp) ~ 1,
                                is.na(sbp) & is.na(dbp) ~ 0,
                                TRUE ~ 1),
         # Diagnosis: No/DK, Blood pressure: in range
         htn_free = case_when(
           is.na(htn) ~ NA_real_,
           htn == 1 ~ 0,
           htn == 0 ~ 1,
           TRUE ~ NA_real_),
         
         htn_undiag_htn = case_when(diagnosed_bp == 1 | is.na(diagnosed_bp) ~ NA_real_,
                                    htn == 1 ~ 1,
                                    htn == 0 ~ 0,
                                    TRUE ~ NA_real_),
         
         # Diagnosis: Yes + Treated: No, Blood pressure: <NA>
         htn_diag_untreat = case_when(diagnosed_bp == 1 & medication_bp == 1 ~ 0,
                                      diagnosed_bp == 1 & medication_bp == 0 ~ 1,
                                      TRUE ~ NA_real_),
         
         # Dignosis: Yes, Treated: Yes, Blood pressure: out of control range
         htn_treat_uncontr = case_when(medication_bp == 0 | is.na(medication_bp)  ~ NA_real_,
                                       medication_bp == 1 & diaghtn == 1 ~ 1,
                                       medication_bp == 1 & diaghtn == 0 ~ 0,
                                       TRUE ~ NA_real_),
         # Dignosis: Yes, Treated: Yes, Blood pressure: in range
         htn_treat_contr = 1 - htn_treat_uncontr,
         
         # Dignosis: Yes, Treated: Yes or No, Blood pressure: out of control range
         htn_diag_uncontr = case_when(diagnosed_bp == 0 | is.na(diagnosed_bp)  ~ NA_real_,
                                      diaghtn == 1 ~ 1,
                                      diaghtn == 0 ~ 0,
                                      TRUE ~ NA_real_),
         # Dignosis: Yes, Treated: Yes, Blood pressure: in range
         htn_diag_contr = 1 - htn_diag_uncontr
         
  ) %>%
    
    # Prediabetes and Prehypertension ------
  mutate(
    prehypertension = case_when(diagnosed_bp == 1 ~ NA_real_,
                                is.na(sbp) | is.na(dbp) ~ NA_real_,
                                htn == 1 ~ 0,
                                sbp >= sbppre_cutoff & sbp < sbp_cutoff ~ 1,
                                dbp >= dbppre_cutoff & dbp < dbp_cutoff~ 1,
                                sbp < sbppre_cutoff ~ 0,
                                dbp < dbppre_cutoff ~ 0,
                                TRUE ~ NA_real_)
  ) %>% 
    mutate(sex = case_when(sex == 1 ~ "Male",
                           sex == 2 ~ "Female"),
           education_h = case_when(education_h %in% c(1:3) ~ education_h,
                                   TRUE ~ NA_real_)
           
           
    ) %>% 
    mutate(laborforce = case_when(retirement == 1 ~ 2,
                                  employment == 1 ~ 1,
                                  employment == 0 ~ 0,
                                  TRUE ~ NA_real_)) %>% 
    mutate(laborforce = factor(laborforce,levels=c(0:2),labels=c("None","Formal","Retired"))) %>% 
    
    mutate_at(vars(insurance,diagnosed_bp,medication_bp,
                   diagnosed_dm,medication_dm), function(x) case_when(x== 2 ~ 0,
                                                                      x == 1 ~ 1,
                                                                      TRUE ~ NA_real_)) %>% 
    mutate(race = case_when(race == 1 ~ 1,
                               race == 4 ~ 0,
                                TRUE ~ as.numeric(race)),
           
           religion = case_when(religion == 1 ~ 11,
                                   religion == 8 ~ 12,
                                   TRUE ~ 13)) %>% 
    # BMI
    mutate_at(vars(bmi),function(x) case_when(x > bmi_max ~ NA_real_,
                                              TRUE ~ as.numeric(x))) %>%
    # Education - harmonized
    mutate_at(vars(education_h),function(x) case_when(x == 1 ~ "Less than lower secondary",
                                                      x == 2 ~ "Upper secondary and vocational training",
                                                      x == 3 ~ "Tertiary",
                                                      TRUE ~ NA_character_)) %>% 
    # Race
    mutate_at(vars(race),function(x) case_when(x == 1 ~ "White",
                                                      x == 0 ~ "Non-White",
                                                      TRUE ~ "Other")) %>%
    # Religion
    mutate_at(vars(religion),function(x) case_when(x == 11 ~ "Christian",
                                                      x == 12 ~ "None",
                                                      TRUE ~ "Other")) %>% 
    # insurance, alcohol
    mutate_at(vars(
      alcohol,insurance), function(x) case_when(x == 0 ~ 0,
                                                x == 1 ~ 1,
                                                TRUE ~ NA_real_)) %>% 
    # Smoking
    mutate_at(vars(smokeever,smokecurr),function(x) case_when(x == 0 ~ 0,
                                                              x == 1 ~ 1,
                                                              TRUE ~ NA_real_)) %>% 
 
    mutate(smoke = case_when(smokecurr == 1 ~ 2,
                             smokeever == 1 ~ 1,
                             smokeever == 0 ~ 0, 
                             smokecurr == 0 ~ 0,
                             TRUE ~ 0)) %>% 
    mutate(smoke = factor(smoke,levels=c(0:2),labels=c("Never","Former","Current"))) %>% 
    mutate(htn_disease = case_when(is.na(htn_free) ~ NA_real_,
                                   htn_free == 1 ~ 0,
                                   htn_undiag_htn == 1 ~ 1,
                                   htn_diag_untreat == 1 ~ 1,
                                   htn_treat_uncontr == 1 ~ 1,
                                   htn_treat_contr == 1 ~ 1,
                                   TRUE ~ 0),
           
           htn_diagnosed = case_when(is.na(htn_free) ~ NA_real_,
                                     htn_free == 1 ~ 0,
                                     htn_undiag_htn == 1 ~ 0,
                                     htn_diag_untreat == 1 ~ 1,
                                     htn_treat_uncontr == 1 ~ 1,
                                     htn_treat_contr == 1 ~ 1,
                                     TRUE ~ 0
           ),
           htn_treated = case_when(is.na(htn_free) ~ NA_real_,
                                   htn_free == 1 ~ 0,
                                   htn_undiag_htn == 1 ~ 0,
                                   htn_diag_untreat == 1 ~ 0,
                                   htn_treat_uncontr == 1 ~ 1,
                                   htn_treat_contr == 1 ~ 1,
                                   TRUE ~ 0
           ),
           htn_controlled = case_when(is.na(htn_free) ~ NA_real_,
                                      htn_free == 1 ~ 0,
                                      htn_undiag_htn == 1 ~ 0,
                                      htn_diag_contr == 1 ~ 1,
                                      htn_diag_untreat == 1 ~ 0,
                                      htn_diag_uncontr == 1 ~ 0,
                                      TRUE ~ 0
           ),
           
           htn_diagnosed_in_dis = case_when(is.na(htn_free) ~ NA_real_,
                                            htn_free == 1 ~ NA_real_,
                                            htn_undiag_htn == 1 ~ 0,
                                            htn_diag_untreat == 1 ~ 1,
                                            htn_treat_uncontr == 1 ~ 1,
                                            htn_treat_contr == 1 ~ 1,
                                            TRUE ~ 0
           ),
           htn_treated_in_dis = case_when(is.na(htn_free) ~ NA_real_,
                                          htn_free == 1 ~ NA_real_,
                                          htn_undiag_htn == 1 ~ 0,
                                          htn_diag_untreat == 1 ~ 0,
                                          htn_treat_uncontr == 1 ~ 1,
                                          htn_treat_contr == 1 ~ 1,
                                          TRUE ~ 0
           ),
           htn_controlled_in_dis = case_when(is.na(htn_free) ~ NA_real_,
                                             htn_free == 1 ~ NA_real_,
                                             htn_undiag_htn == 1 ~ 0,
                                             htn_diag_contr == 1 ~ 1,
                                             htn_diag_untreat == 1 ~ 0,
                                             htn_diag_uncontr == 1 ~ 0,
                                             TRUE ~ 0
           )) %>% 
    
    mutate(bmi_category = case_when(bmi > bmi_max ~ NA_real_,
                                    bmi >= bmi_cutoff[3] ~ 4,
                                    bmi >= bmi_cutoff[2] ~ 3,
                                    bmi >= bmi_cutoff[1] ~ 2,
                                    bmi < bmi_cutoff[1] ~ 1,
                                    TRUE ~ NA_real_),
           
           lengthmar_ge10 = case_when(lengthmar >= 10 ~ 1,
                                      lengthmar < 10 ~ 0,
                                      TRUE ~ NA_real_)
    ) %>% 
    
    mutate(bmi_category = factor(bmi_category,levels=c(1:4),labels=c("Underweight","Normal","Overweight","Obese"))) %>% 
    
    mutate_at(vars(diagnosed_dm,medication_dm,
                   diagnosed_bp,medication_bp),~case_when(is.na(.) ~ 0,
                                                          TRUE ~ .)) %>% 
    
    # ELSA -----
    mutate_at(vars(moderate_pa,vigorous_pa), function(x) case_when(x %in% c(1,2) ~ 1,
                                                                   x %in% c(3,4,5) ~ 0,
                                                                   TRUE ~ NA_real_)) %>% 
    mutate(heavydrinker = case_when(
                                     sex == "Male" & drinksperweek >= 15 ~ 1,
                                     sex == "Female" & drinksperweek >= 8 ~ 1,
                                     
                                     sex == "Male" & drinksperweek < 15 ~ 0,
                                     sex == "Female" & drinksperweek < 8 ~ 0,
                                     TRUE ~ NA_real_)) %>% 
    
    mutate(psu = case_when(psu %in% paste0("E1200000",c(1:9)) ~ str_replace(psu,"E1200000",""),
                           psu == "S99999999" ~ "10",
                           psu == "W99999999" ~ "11")) %>% 
    mutate(psu = as.numeric(psu)) %>% 
    
    return(.)
}

