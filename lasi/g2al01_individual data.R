source("lasi/g2alasi_preprocessing.R")

g2alasi_r_variables <- readxl::read_excel("lasi/G2A LASI Family Variable List.xlsx",sheet="wave1") %>% 
  rename("selected" = harmonized_r) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

g2alasi_s_variables <- readxl::read_excel("lasi/G2A LASI Family Variable List.xlsx",sheet="wave1") %>% 
  rename("selected" = harmonized_s) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

r_male <- haven::read_dta(paste0(path_g2a_data,"/H_LASI_a2.dta"),
                              col_select = na.omit(g2alasi_r_variables$selected)) %>% 
  rename_with(~ g2alasi_r_variables$new_var[which(g2alasi_r_variables$selected == .x)], 
              .cols = g2alasi_r_variables$selected) %>% 
  dplyr::filter(sex == 1)

s_male <- haven::read_dta(paste0(path_g2a_data,"/H_LASI_a2.dta"),
                          col_select = na.omit(g2alasi_s_variables$selected)) %>% 
  rename_with(~ g2alasi_s_variables$new_var[which(g2alasi_s_variables$selected == .x)], 
              .cols = g2alasi_s_variables$selected) %>% 
  dplyr::filter(sex == 1)

r_female <- haven::read_dta(paste0(path_g2a_data,"/H_LASI_a2.dta"),
                          col_select = na.omit(g2alasi_r_variables$selected)) %>% 
  rename_with(~ g2alasi_r_variables$new_var[which(g2alasi_r_variables$selected == .x)], 
              .cols = g2alasi_r_variables$selected) %>% 
  dplyr::filter(sex == 2)

s_female <- haven::read_dta(paste0(path_g2a_data,"/H_LASI_a2.dta"),
                            col_select = na.omit(g2alasi_s_variables$selected)) %>% 
  rename_with(~ g2alasi_s_variables$new_var[which(g2alasi_s_variables$selected == .x)], 
              .cols = g2alasi_s_variables$selected) %>% 
  dplyr::filter(sex == 2)

survey_vars <- c("state","residence","hhweight","hhid")
hh_vars <- c("caste","in_caste","religion","in_religion","hh_wealth","hh_income","hh_consumption","hh_size")

male <- bind_rows(r_male %>% mutate(type = "Respondent"),
          s_male %>% mutate(type = "Spouse"))  %>% 
  g2alasi_preprocessing(.) %>% 
  rename_at(vars(-one_of(c(survey_vars,hh_vars,"coupleid"))),~paste0("h_",.))

female <- bind_rows(r_female %>% mutate(type = "Respondent"),
                  s_female %>% mutate(type = "Spouse"))  %>% 
  g2alasi_preprocessing(.) %>% 
  rename_at(vars(-one_of(c(survey_vars,hh_vars,"coupleid"))),~paste0("w_",.))
  
couples <- left_join(male,
                     female %>% dplyr::select(-one_of(survey_vars,hh_vars)),
                     by="coupleid") %>% 
  distinct(coupleid,.keep_all=TRUE) %>% 
  dplyr::filter(!h_spouseid %in% c(0,NA_real_), !w_spouseid %in% c(0,NA_real_))

require(Hmisc)
hh_quintiles <- couples %>% 
  distinct(hhid,.keep_all=TRUE) %>% 
  dplyr::select(starts_with("hh")) %>% 
  mutate_at(vars(hh_income),
            function(x) cut(x,breaks=c(wtd.quantile(x,weights=.data$hhweight,probs = c(0,0.33,0.67,1.0))),right=TRUE,include.lowest=TRUE,
                            labels=c("Low","Medium","High"))) %>% 
  
  mutate_at(vars(hh_wealth,hh_consumption),
            function(x) cut(x,breaks=c(wtd.quantile(x,weights=.data$hhweight,probs = seq(0,1,by=0.2))),right=TRUE,include.lowest=TRUE,
                            labels=c("Lowest","Low","Medium","High","Highest"))) %>% 
  rename_at(vars(hh_wealth,hh_consumption),~paste0(.,"quintile")) %>% 
  rename(hh_incometertile = hh_income)




couples_hh <- couples %>% 
  left_join(hh_quintiles %>% 
              dplyr::select(hhid,contains("ile")),
            by="hhid") 
saveRDS(couples_hh,paste0(path_g2a_family_folder,"/working/G2A couples_hh.RDS"))  
# couples_hh <- readRDS(paste0(path_g2a_family_folder,"/working/G2A couples_hh.RDS"))  

lasi_couples <- readRDS(paste0(path_g2a_family_folder,"/working/LASI Couples.RDS"))

couples_hh %>% 
  dplyr::select(coupleid,hhid,h_sampleweight,hhweight,
                residence, state, hh_income, hh_consumption,hh_wealth,
                hh_incometertile,hh_consumptionquintile,hh_wealthquintile,
                hh_size, caste, in_caste, religion, in_religion,
                
                w_personid, w_sbp, w_dbp,w_htn,
                w_diagnosed_bp, w_medication_bp, w_diagnosed_dm, w_medication_dm,
                w_weight, w_height, w_bmi, w_waistcircumference, w_hipcircumference,
                w_age,w_sex,w_education,
                w_education_h, w_marital, w_employment, w_retirement, w_smokeever, w_smokecurr,
                w_alcohol, w_moderate_pa, w_vigorous_pa,
                w_children, w_insurance, 
                
                h_personid, h_sbp, h_dbp,h_htn,
                h_diagnosed_bp, h_medication_bp, h_diagnosed_dm, h_medication_dm,
                h_weight, h_height, h_bmi, h_waistcircumference, h_hipcircumference,
                h_age,h_sex,h_education,
                h_education_h, h_marital, h_employment, h_retirement, h_smokeever, h_smokecurr,
                h_alcohol, h_moderate_pa, h_vigorous_pa,
                h_children, h_insurance 
                
                
                ) %>% 
  left_join(lasi_couples %>% 
              dplyr::select(hhid,wife,husband,psu) %>% 
              mutate_at(vars(hhid,wife,husband),~as.character(.)),
            by=c("hhid","w_personid" = "wife","h_personid"="husband")) %>% 
  mutate(psu = zoo::na.locf(psu)) %>% 
  # dplyr::select(hhid,h_personid,w_personid,psu,hhweight) %>% View() %>% 
  distinct(coupleid,.keep_all=TRUE) %>% 
saveRDS(.,paste0(path_g2a_family_folder,"/working/G2A LASI Couples.RDS"))  


