require(lubridate)
g2alasi_preprocessing <- function(df){
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
             diagnosed_bp == 1 & sbp >= sbp_target ~ 1,
             diagnosed_bp == 1 & dbp >= dbp_target ~ 1,
             diagnosed_bp == 1 & sbp < sbp_target ~ 0,
             diagnosed_bp == 1 & dbp < dbp_target ~ 0,
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
         htn_undiag_uncontr = case_when(diagnosed_bp == 1 | is.na(diagnosed_bp) ~ NA_real_,
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
           education = case_when(education %in% c(1) ~ 10,
                                 education %in% c(2,3) ~ 11,
                                 education %in% c(4) ~ 12,
                                 education %in% c(5:9) ~ 13),
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
    mutate(in_caste = case_when(caste == 5 ~ 4,
                                TRUE ~ as.numeric(caste)),
           in_religion = case_when(religion == 2 ~ 12,
                                   religion == 3 ~ 13,
                                   TRUE ~ 14)) %>% 
    # BMI
    mutate_at(vars(bmi),function(x) case_when(x > bmi_max ~ NA_real_,
                                              TRUE ~ as.numeric(x))) %>%
    # Circumferences
    mutate_at(vars(waistcircumference,hipcircumference),function(x) case_when(x > 240 ~ NA_real_,
                                                                              TRUE ~ as.numeric(x))) %>% 
    # Caste
    mutate_at(vars(in_caste),function(x) case_when(x == 1 ~ "Scheduled Caste",
                                                   x == 2 ~ "Scheduled Tribe",
                                                   x == 3 ~ "OBC",
                                                   x == 4 ~ "General",
                                                   # Imputing missing with General
                                                   TRUE ~ "General")) %>% 
    # Education
    mutate_at(vars(education),function(x) case_when(x == 10 ~ "No education",
                                                    x == 11 ~ "Primary",
                                                    x == 12 ~ "Secondary",
                                                    x == 13 ~ "Higher",
                                                    TRUE ~ NA_character_)) %>% 
    # Education - harmonized
    mutate_at(vars(education_h),function(x) case_when(x == 1 ~ "Less than lower secondary",
                                                    x == 2 ~ "Upper secondary and vocational training",
                                                    x == 3 ~ "Tertiary",
                                                    TRUE ~ NA_character_)) %>% 
    
    # Religion
    mutate_at(vars(in_religion),function(x) case_when(x == 12 ~ "Hindu",
                                                      x == 13 ~ "Muslim",
                                                      TRUE ~ "Other")) %>% 
    # insurance, alcohol
    mutate_at(vars(
      alcohol,insurance), function(x) case_when(x == 0 ~ 0,
                                                x == 1 ~ 1,
                                                TRUE ~ NA_real_)) %>% 
    # Smoking
    mutate_at(vars(smokeever,smokecurr),function(x) case_when(x == 2 ~ 0,
                                                              x == 1 ~ 1,
                                                              TRUE ~ NA_real_)) %>% 
    mutate(smokecount = case_when(smokecount >= 30 ~ 30,
                                  TRUE ~ smokecount)) %>% 
    
    mutate(smoke = case_when(smokecurr == 1 ~ 2,
                             smokeever == 1 ~ 1,
                             smokeever == 0 ~ 0, 
                             smokecurr == 0 ~ 0,
                             TRUE ~ 0)) %>% 
    mutate(smoke = factor(smoke,levels=c(0:2),labels=c("Never","Former","Current"))) %>% 
    mutate(htn_disease = case_when(is.na(htn_free) ~ NA_real_,
                                   htn_free == 1 ~ 0,
                                   htn_undiag_uncontr == 1 ~ 1,
                                   htn_diag_untreat == 1 ~ 1,
                                   htn_treat_uncontr == 1 ~ 1,
                                   htn_treat_contr == 1 ~ 1,
                                   TRUE ~ 0),
           
           htn_diagnosed = case_when(is.na(htn_free) ~ NA_real_,
                                     htn_free == 1 ~ 0,
                                     htn_undiag_uncontr == 1 ~ 0,
                                     htn_diag_untreat == 1 ~ 1,
                                     htn_treat_uncontr == 1 ~ 1,
                                     htn_treat_contr == 1 ~ 1,
                                     TRUE ~ 0
           ),
           htn_treated = case_when(is.na(htn_free) ~ NA_real_,
                                   htn_free == 1 ~ 0,
                                   htn_undiag_uncontr == 1 ~ 0,
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
                                            htn_undiag_uncontr == 1 ~ 0,
                                            htn_diag_untreat == 1 ~ 1,
                                            htn_treat_uncontr == 1 ~ 1,
                                            htn_treat_contr == 1 ~ 1,
                                            TRUE ~ 0
           ),
           htn_treated_in_dis = case_when(is.na(htn_free) ~ NA_real_,
                                          htn_free == 1 ~ NA_real_,
                                          htn_undiag_uncontr == 1 ~ 0,
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
           
           highwc = case_when(sex == "Female" & waistcircumference >= female_wc_cutoff ~ 1,
                              sex == "Female" & waistcircumference < female_wc_cutoff ~ 0,
                              sex == "Male" & waistcircumference >= male_wc_cutoff ~ 1,
                              sex == "Male" & waistcircumference < male_wc_cutoff ~ 0,
                              TRUE ~ NA_real_
           ),
           waist_hip = case_when(!is.na(hipcircumference) ~ waistcircumference/hipcircumference,
                                 TRUE ~ NA_real_),
           
           lengthmar_ge10 = case_when(lengthmar >= 10 ~ 1,
                                      lengthmar < 10 ~ 0,
                                      TRUE ~ NA_real_)
    ) %>% 
    
    mutate(bmi_category = factor(bmi_category,levels=c(1:4),labels=c("Underweight","Normal","Overweight","Obese")),
           highwhr = case_when(sex == "Female" & waist_hip >= female_whr_cutoff ~ 1,
                               sex == "Female" & waist_hip < female_whr_cutoff ~ 0,
                               sex == "Male" & waist_hip >= male_whr_cutoff ~ 1,
                               sex == "Male" & waist_hip < male_whr_cutoff ~ 0,
                               TRUE ~ NA_real_
           )) %>% 
    
    mutate_at(vars(diagnosed_dm,medication_dm,
                   diagnosed_bp,medication_bp),~case_when(is.na(.) ~ 0,
                                                          TRUE ~ .)) %>% 
    return(.)
}

