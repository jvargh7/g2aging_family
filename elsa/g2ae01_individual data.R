source("elsa/g2aelsa_preprocessing.R")

h_elsa_g2 <- haven::read_dta(paste0(path_g2a_family_folder,"/working/elsa/h_elsa_g2.dta"))

g2aelsa_r_variables <- readxl::read_excel("elsa/G2A ELSA Family Variable List.xlsx",sheet="wave8") %>% 
  rename("selected" = harmonized_r) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

g2aelsa_s_variables <- readxl::read_excel("elsa/G2A ELSA Family Variable List.xlsx",sheet="wave8") %>% 
  rename("selected" = harmonized_s) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected))

r_male <- haven::read_dta(paste0(path_g2a_family_folder,"/working/elsa/h_elsa_g2.dta"),
                          col_select = na.omit(g2aelsa_r_variables$selected)) %>% 
  rename_with(.cols = g2aelsa_r_variables$selected,
              ~g2aelsa_r_variables$new_var[which(g2aelsa_r_variables$selected == .x)]) %>% 
  dplyr::filter(sex == 1)

s_male <- haven::read_dta(paste0(path_g2a_family_folder,"/working/elsa/h_elsa_g2.dta"),
                          col_select = na.omit(g2aelsa_s_variables$selected)) %>% 
  rename_with(~ g2aelsa_s_variables$new_var[which(g2aelsa_s_variables$selected == .x)], 
              .cols = g2aelsa_s_variables$selected) %>% 
  dplyr::filter(sex == 1)

r_female <- haven::read_dta(paste0(path_g2a_family_folder,"/working/elsa/h_elsa_g2.dta"),
                            col_select = na.omit(g2aelsa_r_variables$selected)) %>% 
  rename_with(~ g2aelsa_r_variables$new_var[which(g2aelsa_r_variables$selected == .x)], 
              .cols = g2aelsa_r_variables$selected) %>% 
  dplyr::filter(sex == 2)

s_female <- haven::read_dta(paste0(path_g2a_family_folder,"/working/elsa/h_elsa_g2.dta"),
                            col_select = na.omit(g2aelsa_s_variables$selected)) %>% 
  rename_with(~ g2aelsa_s_variables$new_var[which(g2aelsa_s_variables$selected == .x)], 
              .cols = g2aelsa_s_variables$selected) %>% 
  dplyr::filter(sex == 2)

survey_vars <- c("strata","psu","hhid")
hh_vars <- c("hh_wealth","hh_income","hh_consumption","hh_size")

male <- bind_rows(r_male %>% mutate(type = "Respondent"),
                  s_male %>% mutate(type = "Spouse"))  %>% 
  g2aelsa_preprocessing(.) %>% 
  rename_at(vars(-one_of(c(survey_vars,hh_vars,"coupleid"))),~paste0("h_",.))

female <- bind_rows(r_female %>% mutate(type = "Respondent"),
                    s_female %>% mutate(type = "Spouse"))  %>% 
  g2aelsa_preprocessing(.) %>% 
  rename_at(vars(-one_of(c(survey_vars,hh_vars,"coupleid"))),~paste0("w_",.))

couples <- left_join(male %>% dplyr::filter(!is.na(coupleid)),
                     female %>% dplyr::filter(!is.na(coupleid)) %>% dplyr::select(-one_of(survey_vars,hh_vars)),
                     by="coupleid") %>% 
  distinct(coupleid,.keep_all=TRUE) %>% 
  dplyr::filter(!h_spouseid %in% c(0,NA_real_), !w_spouseid %in% c(0,NA_real_))

require(Hmisc)
hh_quintiles <- couples %>% 
  distinct(hhid,.keep_all=TRUE) %>% 
  dplyr::select(starts_with("hh"),h_sampleweight) %>% 
  mutate_at(vars(hh_income),
            function(x) cut(x,breaks=c(wtd.quantile(x,weights=.$h_sampleweight,probs = c(0,0.33,0.67,1.0))),right=TRUE,include.lowest=TRUE,
                            labels=c("Low","Medium","High"))) %>% 
  
  mutate_at(vars(hh_wealth,hh_consumption),
            function(x) cut(x,breaks=c(wtd.quantile(x,weights=.$h_sampleweight,probs = seq(0,1,by=0.2))),right=TRUE,include.lowest=TRUE,
                            labels=c("Lowest","Low","Medium","High","Highest"))) %>% 
  rename_at(vars(hh_wealth,hh_consumption),~paste0(.,"quintile")) %>% 
  rename(hh_incometertile = hh_income)




couples_hh <- couples %>% 
  left_join(hh_quintiles %>% 
              dplyr::select(hhid,contains("ile")),
            by="hhid") 
saveRDS(couples_hh,paste0(path_g2a_family_folder,"/working/elsa/G2A couples_hh.RDS"))  
# couples_hh <- readRDS(paste0(path_g2a_family_folder,"/working/elsa/G2A couples_hh.RDS"))  

couples_hh %>% 
  dplyr::select(coupleid,hhid,h_sampleweight,w_sampleweight,
                strata,psu, hh_income, hh_consumption,hh_wealth,
                hh_incometertile,hh_consumptionquintile,hh_wealthquintile,
                hh_size,
                
                w_personid, w_sbp, w_dbp,w_htn,
                w_diagnosed_bp, w_medication_bp, w_diagnosed_dm, w_medication_dm,
                w_weight, w_height, w_bmi, 
                w_age,w_sex,w_lengthmar,w_lengthmar_ge10,
                w_eduyr,w_education_h, w_marital, w_employment, w_retirement,w_laborforce, 
                w_smokeever, w_smokecurr, w_smoke,
                w_alcohol,w_heavydrinker, w_moderate_pa, w_vigorous_pa,
                w_children, w_insurance, w_race,w_religion,
                
                h_personid, h_sbp, h_dbp,h_htn,
                h_diagnosed_bp, h_medication_bp, h_diagnosed_dm, h_medication_dm,
                h_weight, h_height, h_bmi, 
                h_age,h_sex,h_lengthmar,h_lengthmar_ge10,
                h_eduyr, h_education_h, h_marital, h_employment, h_retirement,h_laborforce,
                h_smokeever, h_smokecurr, h_smoke,
                h_alcohol, h_heavydrinker, h_moderate_pa, h_vigorous_pa,
                h_children, h_insurance, h_race, h_religion
                
                
  ) %>% 
  mutate(psu = zoo::na.locf(psu)) %>% 
  # dplyr::select(hhid,h_personid,w_personid,psu,hhweight) %>% View() %>% 
  distinct(coupleid,.keep_all=TRUE) %>% 
  mutate(hh_lengthmar = case_when(!is.na(h_lengthmar) ~ h_lengthmar,
                                  TRUE ~ w_lengthmar),
         hh_lengthmar_ge10 = case_when(!is.na(h_lengthmar_ge10) ~ h_lengthmar_ge10,
                                       TRUE ~ w_lengthmar_ge10)) %>%
  mutate_at(vars(h_height,w_height),function(x) x*100) %>% 
  saveRDS(.,paste0(path_g2a_family_folder,"/working/elsa/G2A ELSA Couples.RDS"))  


