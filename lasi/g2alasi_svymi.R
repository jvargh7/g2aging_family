couples <- readRDS(paste0(path_g2a_family_folder,"/working/G2A LASI Couples.RDS")) %>% 
  mutate_at(vars(w_smokeever,w_smokecurr,w_insurance,
                 h_smokeever,h_smokecurr,h_insurance),function(x) case_when(is.na(x) ~ 0,
                                                                            TRUE ~ x))


continuous_vars <- c(paste0(rep(c("w_","h_"),each=13),
                            c("sbp","dbp","glucose","weight","height",
                              "bmi","waistcircumference","hipcircumference",
                              "age","eduyr","children",
                              "moderate_pa","vigorous_pa")),
                     "hh_size")

proportion_vars <- c(paste0(rep(c("w_","h_"),each=16),
                            c("screened_bp","diagnosed_bp","medication_bp",
                              "screened_dm","diagnosed_dm","medication_dm",
                              
                              "employment","retirement","smokeever","smokecurr","alcohol",
                              "insurance","htn")),"residence")

grouped_vars <- c("w_education_h","h_education_h","in_caste","in_religion","hh_wealthquintile","hh_consumptionquintile","hh_incometertile")


require(survey)
require(mice)

before_imputation <- couples %>% 
  dplyr::select(coupleid,hhid,w_personid,h_personid,h_sampleweight,state,psu,
                one_of(continuous_vars),one_of(proportion_vars),one_of(grouped_vars)) %>% 
  mutate_at(vars(state,w_moderate_pa,w_vigorous_pa,
                 h_moderate_pa,h_vigorous_pa,
                 w_retirement,h_retirement,residence),~as.numeric(.)) %>% 
  # Step 1: One hot encoding
  mutate(
         w_education_2 = case_when(w_education_h == "upper secondary and vocational training" ~ 1,
                                   !is.na(w_education_h) ~ 0,
                                   TRUE ~ NA_real_),
         w_education_3 = case_when(w_education_h == "tertiary" ~ 1,
                                   !is.na(w_education_h) ~ 0,
                                   TRUE ~ NA_real_),
         h_education_2 = case_when(h_education_h == "upper secondary and vocational training" ~ 1,
                                   !is.na(h_education_h) ~ 0,
                                   TRUE ~ NA_real_),
         h_education_3 = case_when(h_education_h == "tertiary" ~ 1,
                                   !is.na(h_education_h) ~ 0,
                                   TRUE ~ NA_real_),
         
         w_ge65 = case_when(w_age >= 65 ~ 1,
                            TRUE ~ 0),
         h_ge65 = case_when(h_age >= 65 ~ 1,
                            TRUE ~ 0),
         
         hh_low = case_when(hh_wealthquintile == "Low" ~ 1,
                            TRUE ~ 0),
         hh_medium = case_when(hh_wealthquintile == "Medium" ~ 1,
                            TRUE ~ 0),
         hh_high = case_when(hh_wealthquintile == "High" ~ 1,
                            TRUE ~ 0),
         hh_highest = case_when(hh_wealthquintile == "Highest" ~ 1,
                            TRUE ~ 0)
         
         
         ) %>% 
  # Step 2: Modeling interactions
  mutate(w_htn_residence = residence*w_htn,
         h_htn_residence = residence*h_htn,
         
         # Since exposure is w_htn and effect modifier is husband's education
         w_htn_h_education_2 = h_education_2*w_htn,
         w_htn_h_education_3 = h_education_3*w_htn,
         
         h_htn_w_education_2 = w_education_2*h_htn,
         h_htn_w_education_3 = w_education_3*h_htn,
         
         # Since exposure is w_htn and effect modifier is husband's age category (<65 vs >=65)
         w_htn_h_ge65 = h_ge65*w_htn,
         h_htn_w_ge65 = w_ge65*h_htn,
         
         # Since exposure is w_htn and effect modifier is household wealth quintile
         w_htn_hh_low = w_htn*hh_low,
         w_htn_hh_medium = w_htn*hh_medium,
         w_htn_hh_high = w_htn*hh_high,
         w_htn_hh_highest = w_htn*hh_highest,
         
         h_htn_hh_low = h_htn*hh_low,
         h_htn_hh_medium = h_htn*hh_medium,
         h_htn_hh_high = h_htn*hh_high,
         h_htn_hh_highest = h_htn*hh_highest
         ) %>% 
  dplyr::select(-w_education_h,-h_education_h,
                -hh_wealthquintile,-hh_wealth,-hh_income,-hh_consumption)


interaction_terms <- c("w_htn_residence","h_htn_residence",
                       "w_htn_h_education_2","w_htn_h_education_3",
                       "h_htn_w_education_2","h_htn_w_education_3",
                       "w_htn_h_ge65","h_htn_w_ge65",
                       "w_htn_hh_low","w_htn_hh_medium","w_htn_hh_high","w_htn_hh_highest",
                       "h_htn_hh_low","h_htn_hh_medium","h_htn_hh_high","h_htn_hh_highest"
                       )

mi_null <- mice(before_imputation,
                maxit = 0)

method = mi_null$method
pred = mi_null$predictorMatrix

pred[c("coupleid","hhid","w_personid","h_personid","h_sampleweight","state","psu"),] <- 0
pred[,c("coupleid","hhid","w_personid","h_personid","h_sampleweight","state","psu")] <- 0

pred[c("w_htn","h_htn"),] <-0
pred[,c("w_htn","h_htn")] <-0

# https://stackoverflow.com/questions/33865161/model-multiple-imputation-with-interaction-terms
# https://thestatsgeek.com/2014/05/10/multiple-imputation-with-interactions-and-non-linear-terms/

for(i_t in interaction_terms){
  print(i_t)
  htn_term = str_extract(i_t,"^(w|h)_htn")
  em_term = str_replace(i_t,pattern=paste0(htn_term,"_"),replacement = "")
  method[i_t] = paste0("~I(",htn_term,"*",em_term,")")
  pred[c(htn_term,em_term),i_t] <- 0
}


# Takes ~30 mins
mi_dfs <- mice(before_imputation,
               method = method,
               pred = pred,
               m=10,maxit=50,seed=500)

saveRDS(mi_dfs, paste0(path_g2a_family_folder,"/working/G2A LASI Couples mi_dfs.RDS"))
