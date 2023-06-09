coupleCHARLS <- read.csv(paste0(path_g2a_family_folder,"/working/charls/coupleCHARLS.csv"))
coupleCHARLS <- coupleCHARLS[, -1]
coupleCHARLS$r_indweight[is.na(coupleCHARLS$r_indweight)] <- 19667



#continuous variable
continuous_vars <- c("h_age", "w_age",
                     "h_sbp", "w_sbp",
                     "h_dbp", "w_dbp",
                     "h_height", "w_height",
                     "h_weight", "w_weight",
                     "h_bmi","w_bmi",
                     "h_waistcircumference", "w_waistcircumference",
                     "h_moderate_pa", "w_moderate_pa",
                     "h_vigorous_pa", "w_vigorous_pa",
                     "hh_children", "hh_size", "hh_lengthmar")

proportion_vars <- c("h_diagnosed_bp", "w_diagnosed_bp",
                     "h_diagnosed_dm", "w_diagnosed_dm",
                     "h_heavydrinker", "w_heavydrinker",
                     "hh_lengthmar_ge10",
                     "w_htn", "h_htn", "hh_htn")

#any factor variables
grouped_vars <- c("h_laborforce", "w_laborforce",
                  "h_hukou", "w_hukou",
                  "residence",
                  "h_smoke", "w_smoke",
                  "h_education_h","w_education_h",
                  "hh_incometertile", "hh_wealthquintile")


#-----------------------------------------------
#multiple imputation
require(survey)
require(mice)

#define factor variables
colnames(coupleCHARLS)
# coupleCHARLS[, c(9:11, 14:20, 25:28, 37:42, 49:50, 52:64)] <- lapply(coupleCHARLS[, c(9:11, 14:20, 25:28, 37:42, 49:50, 52:64)], factor)

before_imputation <- coupleCHARLS %>% 
  dplyr::select(ID, hhid, hhcoupid, hh_weight, r_indweight, s_indweight,
                one_of(continuous_vars), one_of(proportion_vars), one_of(grouped_vars)) %>% 
  mutate_at(vars(w_moderate_pa, w_vigorous_pa,
                 h_moderate_pa, h_vigorous_pa,
                 residence, hh_lengthmar),~as.numeric(.)) %>% 
  # mutate_at(vars(w_htn,h_htn,h_diagnosed_bp,w_diagnosed_bp,
  #                h_diagnosed_dm,w_diagnosed_dm,h_heavydrinker,w_heavydrinker,
  #                hh_htn,h_hukou,w_hukou),function(x) as.numeric(levels(x))[x]) %>% 
  # Step 1: One hot encoding
  mutate(
    w_education_2 = case_when(w_education_h == "2" ~ 1,
                              !is.na(w_education_h) ~ 0,
                              TRUE ~ NA_real_),
    w_education_3 = case_when(w_education_h == "3" ~ 1,
                              !is.na(w_education_h) ~ 0,
                              TRUE ~ NA_real_),
    h_education_2 = case_when(h_education_h == "2" ~ 1,
                              !is.na(h_education_h) ~ 0,
                              TRUE ~ NA_real_),
    h_education_3 = case_when(h_education_h == "3" ~ 1,
                              !is.na(h_education_h) ~ 0,
                              TRUE ~ NA_real_),
    
    hh_lengthmar_ge10 = case_when(hh_lengthmar >= 10 ~ 1,
                                  TRUE ~ 0),
    
    w_ge65 = case_when(w_age >= 65 ~ 1,
                       TRUE ~ 0),
    h_ge65 = case_when(h_age >= 65 ~ 1,
                       TRUE ~ 0),
    
    hh_low = case_when(hh_wealthquintile == "Low" ~ 1,
                       is.na(hh_wealthquintile) ~ NA_real_,
                       TRUE ~ 0),
    hh_medium = case_when(hh_wealthquintile == "Medium" ~ 1,
                          is.na(hh_wealthquintile) ~ NA_real_,
                          TRUE ~ 0),
    hh_high = case_when(hh_wealthquintile == "High" ~ 1,
                        is.na(hh_wealthquintile) ~ NA_real_,
                        TRUE ~ 0),
    hh_highest = case_when(hh_wealthquintile == "Highest" ~ 1,
                           is.na(hh_wealthquintile) ~ NA_real_,
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
         
         # Since exposure is w_htn and effect modifier is length of marriage (<10 vs >=10)
         w_htn_hh_lengthmar_ge10 = hh_lengthmar_ge10*w_htn,
         h_htn_hh_lengthmar_ge10 = hh_lengthmar_ge10*h_htn,
         
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
                -hh_wealthquintile) %>% 
  mutate_at(vars(hh_lengthmar_ge10),~as.numeric(as.character(.)))

interaction_terms <- c("w_htn_residence","h_htn_residence",
                       "w_htn_h_education_2","w_htn_h_education_3",
                       "h_htn_w_education_2","h_htn_w_education_3",
                       "w_htn_h_ge65","h_htn_w_ge65",
                       "w_htn_hh_lengthmar_ge10","h_htn_hh_lengthmar_ge10",
                       "w_htn_hh_low","w_htn_hh_medium","w_htn_hh_high","w_htn_hh_highest",
                       "h_htn_hh_low","h_htn_hh_medium","h_htn_hh_high","h_htn_hh_highest"
)

mi_null <- mice(before_imputation,
                maxit = 0)

method = mi_null$method
pred = mi_null$predictorMatrix

pred[c("ID", "hhid", "hhcoupid", "hh_weight", "r_indweight", "s_indweight"),] <- 0
pred[,c("ID", "hhid", "hhcoupid", "hh_weight", "r_indweight", "s_indweight")] <- 0
method[c("ID", "hhid", "hhcoupid", "hh_weight", "r_indweight", "s_indweight")] <- ""


# Impute via equation and do not use for imputation , --------
method["hh_lengthmar_ge10"] <- "~I((hh_lengthmar>=10)*1)"
pred["hh_lengthmar_ge10",] <- 0
pred[,"hh_lengthmar_ge10"] <- 0

method["w_ge65"] <- "~I((w_age>=65)*1)"
method["h_ge65"] <- "~I((h_age>=65)*1)"
pred[c("w_ge65","h_ge65"),] <- 0
pred[,c("w_ge65","h_ge65")] <- 0

# https://stackoverflow.com/questions/33865161/model-multiple-imputation-with-interaction-terms
# https://thestatsgeek.com/2014/05/10/multiple-imputation-with-interactions-and-non-linear-terms/

for(i_t in interaction_terms){
  print(i_t)
  htn_term = str_extract(i_t,"^(w|h)_htn")
  em_term = str_replace(i_t,pattern=paste0(htn_term,"_"),replacement = "")
  method[i_t] = paste0("~I(",htn_term,"*",em_term,")")
  pred[c(htn_term,em_term),i_t] <- 0
}



# Takes ~1.5 hours
mi_dfs <- mice(before_imputation,
               method = method,
               pred = pred,
               m=10,maxit=50,seed=500)

saveRDS(mi_dfs, paste0(path_g2a_family_folder,"/working/charls/G2A CHARLS Couples mi_dfs JV.RDS"))