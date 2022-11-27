setwd("G:\\My Drive\\Crossnation study\\Dataset")

coupleCHARLS <- read.csv("coupleCHARLS.csv")
coupleCHARLS <- coupleCHARLS[, -1]

library(car)
library(tidyverse)
library(tableone)
library(kableExtra)
library(nnet)
library(psych)

#correlation between hhsize and number of children
cor.test(coupleCHARLS$hh_children, coupleCHARLS$hh_size)

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

#check Kappa coefficient
colnames(coupleCHARLS)
cohen.kappa(coupleCHARLS[, c(57, 58)])

#-------------------------------------------------------------------
#Descriptive Table 1
require(srvyr)
colnames(coupleCHARLS)
source("svysummary.R")

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

#Assign median to weight to run survey function
summary(coupleCHARLS$r_indweight)

coupleCHARLS$r_indweight[is.na(coupleCHARLS$r_indweight)] <- 19667

couples_svy <- coupleCHARLS %>% 
  as_survey_design(.data = .,
                   #ids = PSU_weight, strata = strata_weight,
                   weight = r_indweight,
                   nest = TRUE,
                   variance = "YG", pps = "brewer")

couples_svysummary <- svysummary(couples_svy,
                                 c_vars = continuous_vars,
                                 p_vars = proportion_vars,
                                 g_vars = grouped_vars) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

couples_count <- coupleCHARLS %>% 
  summarize_at(vars(one_of(c(continuous_vars,
                             proportion_vars,
                             grouped_vars))),
               list(n = ~sum(!is.na(.)))) %>% 
  pivot_longer(names_to = "variable", values_to = "n",cols = everything()) %>% 
  mutate(variable = str_replace(variable,"_n$",""))

left_join(couples_svysummary,
          couples_count,
          by = "variable") %>% 
  write_csv(.,"CHARLS_table1.csv")

#-----------------------------------------------
#multiple imputation
require(survey)
require(mice)

#define factor variables
colnames(coupleCHARLS)
coupleCHARLS[, c(9:11, 14:20, 25:28, 37:42, 49:50, 52:64)] <- lapply(coupleCHARLS[, c(9:11, 14:20, 25:28, 37:42, 49:50, 52:64)], factor)

before_imputation <- coupleCHARLS %>% 
  dplyr::select(ID, hhid, hhcoupid, hh_weight, r_indweight, s_indweight,
                one_of(continuous_vars), one_of(proportion_vars), one_of(grouped_vars)) %>% 
  mutate_at(vars(w_moderate_pa, w_vigorous_pa,
                 h_moderate_pa, h_vigorous_pa,
                 residence, hh_lengthmar),~as.numeric(.)) %>% 
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
    
    hh_lengthmar10 = case_when(hh_lengthmar >= 10 ~ 1,
                               TRUE ~ 0),
    
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
                -hh_wealthquintile)

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

# Do not impute and do not use for imputation ------
pred[c("w_htn","h_htn"),] <-0
pred[,c("w_htn","h_htn")] <-0

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

for(i_t in interaction_terms){
  print(i_t)
  htn_term = str_extract(i_t,"^(w|h)_htn")
  em_term = str_replace(i_t,pattern=paste0(htn_term,"_"),replacement = "")
  method[i_t] = paste0("~I(",htn_term,"*",em_term,")")
  
  # Do not use interaction terms for imputation of the source variables
  pred[c(htn_term,em_term),i_t] <- 0
}

# Takes ~1.5 hours
mi_dfs <- mice(before_imputation,
               method = method,
               pred = pred,
               m=10,maxit=50,seed=500)

saveRDS(mi_dfs, "G2A CHARLS Couples mi_dfs.RDS")

#----------------------------------------------
#Poisson with MI data
mi_dfs <- readRDS("G2A CHARLS Couples mi_dfs.RDS")

require(mice)
require(srvyr)
require(survey)

source("g2ahrs_poisson regression equations_CHARLS.R")

#check if I have the same variables in this function
colnames(mi_dfs$data)

#make sure h_htn and w_htn variables as 1/0 to facilitate model estimation
table(mi_dfs$data$h_htn)
table(mi_dfs$data$w_htn)

table(mi_dfs$data$hh_lengthmar_ge10)

mi_dfs$data$h_htn <- ifelse(mi_dfs$data$h_htn == "1", 1, 0)
mi_dfs$data$w_htn <- ifelse(mi_dfs$data$w_htn == "1", 1, 0)
table(mi_dfs$data$h_htn)

# Run Poisson Regression ------------
for(i in 1:mi_dfs$m){
  df = complete(mi_dfs,action = i) %>% 
    dplyr::filter(!is.na(residence));
  
  svy_des = df  %>% 
    
    as_survey_design(.data = .,
                     #ids = PSU_weight, strata = strata_weight,
                     weight = r_indweight,
                     nest = TRUE,
                     variance = "YG",pps = "brewer");
  overall_w1[[i]] = svyglm(w1,design=svy_des,family=quasipoisson());
  overall_h1[[i]] = svyglm(h1,design=svy_des,family=quasipoisson());
  overall_w2[[i]] = svyglm(w2,design=svy_des,family=quasipoisson());
  overall_h2[[i]] = svyglm(h2,design=svy_des,family=quasipoisson());
  overall_w3[[i]] = svyglm(w3,design=svy_des,family=quasipoisson());
  overall_h3[[i]] = svyglm(h3,design=svy_des,family=quasipoisson());
  overall_w4[[i]] = svyglm(w4,design=svy_des,family=quasipoisson());
  overall_h4[[i]] = svyglm(h4,design=svy_des,family=quasipoisson());
  overall_w5[[i]] = svyglm(w5,design=svy_des,family=quasipoisson());
  overall_h5[[i]] = svyglm(h5,design=svy_des,family=quasipoisson());
  overall_w6[[i]] = svyglm(w6,design=svy_des,family=quasipoisson());
  overall_h6[[i]] = svyglm(h6,design=svy_des,family=quasipoisson());
  
  gc();rm(df);rm(svy_des)
}

# Pooling coefficients ------------
source("mice_coef_svyglm.R")
# Check https://github.com/jvargh7/functions/blob/main/survey/mice_coef_svyglm.R
# You would also have to download the following:
# a. https://github.com/jvargh7/functions/blob/main/imputation/adjusted_ci.R
# b. https://github.com/jvargh7/functions/tree/main/preprocessing

overall_w1_out = mice_coef_svyglm(overall_w1)
overall_h1_out = mice_coef_svyglm(overall_h1)
overall_w2_out = mice_coef_svyglm(overall_w2)
overall_h2_out = mice_coef_svyglm(overall_h2)
overall_w3_out = mice_coef_svyglm(overall_w3)
overall_h3_out = mice_coef_svyglm(overall_h3)
overall_w4_out = mice_coef_svyglm(overall_w4)
overall_h4_out = mice_coef_svyglm(overall_h4)
overall_w5_out = mice_coef_svyglm(overall_w5)
overall_h5_out = mice_coef_svyglm(overall_h5)
overall_w6_out = mice_coef_svyglm(overall_w6)
overall_h6_out = mice_coef_svyglm(overall_h6)

bind_rows(
  overall_w1_out %>% mutate(model = "W1"),
  overall_h1_out %>% mutate(model = "H1"),
  overall_w2_out %>% mutate(model = "W2"),
  overall_h2_out %>% mutate(model = "H2"),
  overall_w3_out %>% mutate(model = "W3"),
  overall_h3_out %>% mutate(model = "H3"),
  overall_w4_out %>% mutate(model = "W4"),
  overall_h4_out %>% mutate(model = "H4"),
  overall_w5_out %>% mutate(model = "W4"),
  overall_h5_out %>% mutate(model = "H4"),
  overall_w6_out %>% mutate(model = "W6"),
  overall_h6_out %>% mutate(model = "H6")
  
) %>% 
  write_csv(.,"g2al03_poisson regression with multiple imputation.csv")

source("mice_contrasts_svyglm.R")
# Check: https://github.com/jvargh7/functions/blob/main/survey/mice_contrasts_svyglm.R
# You would also have to download the following:
# https://github.com/jvargh7/functions/blob/main/preprocessing/prepare_contrasts.R
# https://github.com/jvargh7/functions/blob/main/survey/contrasts_svyglm.R
# The coefficients are in column 'RR'. The rest of the columns are for pooling multiple imputated regressions

contrasts_w2_out = mice_contrasts_svyglm(svymodel_list = overall_w2,modifier = "w_ge65",exposure = "h_htn")
contrasts_h2_out = mice_contrasts_svyglm(svymodel_list = overall_h2,modifier = "h_ge65",exposure = "w_htn")

contrasts_w3_out_ed2 = mice_contrasts_svyglm(svymodel_list = overall_w3,modifier = "w_education_2",exposure = "h_htn")
contrasts_h3_out_ed2 = mice_contrasts_svyglm(svymodel_list = overall_h3,modifier = "h_education_2",exposure = "w_htn")

contrasts_w3_out_ed3 = mice_contrasts_svyglm(svymodel_list = overall_w3,modifier = "w_education_3",exposure = "h_htn")
contrasts_h3_out_ed3 = mice_contrasts_svyglm(svymodel_list = overall_h3,modifier= "h_education_3",exposure = "w_htn")

contrasts_w4_out = mice_contrasts_svyglm(svymodel_list = overall_w4,modifier = "residence",exposure = "h_htn")
contrasts_h4_out = mice_contrasts_svyglm(svymodel_list = overall_h4,modifier = "residence",exposure = "w_htn")

contrasts_w5_out_wlt2 = mice_contrasts_svyglm(svymodel_list = overall_w5,modifier = "hh_low",exposure = "h_htn")
contrasts_h5_out_wlt2 = mice_contrasts_svyglm(svymodel_list = overall_h5,modifier = "hh_low",exposure = "w_htn")

contrasts_w5_out_wlt3 = mice_contrasts_svyglm(svymodel_list = overall_w5,modifier = "hh_medium",exposure = "h_htn")
contrasts_h5_out_wlt3 = mice_contrasts_svyglm(svymodel_list = overall_h5,modifier = "hh_medium",exposure = "w_htn")

contrasts_w5_out_wlt4 = mice_contrasts_svyglm(svymodel_list = overall_w5,modifier = "hh_high",exposure = "h_htn")
contrasts_h5_out_wlt4 = mice_contrasts_svyglm(svymodel_list = overall_h5,modifier = "hh_high",exposure = "w_htn")

contrasts_w5_out_wlt5 = mice_contrasts_svyglm(svymodel_list = overall_w5,modifier = "hh_highest",exposure = "h_htn")
contrasts_h5_out_wlt5 = mice_contrasts_svyglm(svymodel_list = overall_h5,modifier = "hh_highest",exposure = "w_htn")

contrasts_w6_out = mice_contrasts_svyglm(svymodel_list = overall_w6,modifier = "hh_lengthmar_ge10",exposure = "h_htn")
contrasts_h6_out = mice_contrasts_svyglm(svymodel_list = overall_h6,modifier = "hh_lengthmar_ge10",exposure = "w_htn")

bind_rows(
  contrasts_w2_out %>% mutate(model = "W2"),
  contrasts_h2_out %>% mutate(model = "H2"),
  contrasts_w3_out_ed2 %>% mutate(model = "W3 E2"),
  contrasts_h3_out_ed2 %>% mutate(model = "H3 E2"),
  contrasts_w3_out_ed3 %>% mutate(model = "W3 E3"),
  contrasts_h3_out_ed3 %>% mutate(model = "H3 E3"),
  
  contrasts_w4_out %>% mutate(model = "W4"),
  contrasts_h4_out %>% mutate(model = "H4"),
  
  contrasts_w5_out_wlt2 %>% mutate(model = "W5 Low"),
  contrasts_h5_out_wlt2 %>% mutate(model = "H5 Low"),
  
  contrasts_w5_out_wlt3 %>% mutate(model = "W5 Medium"),
  contrasts_h5_out_wlt3 %>% mutate(model = "H5 Medium"),
  
  contrasts_w5_out_wlt4 %>% mutate(model = "W5 High"),
  contrasts_h5_out_wlt4 %>% mutate(model = "H5 High"),
  
  contrasts_w5_out_wlt5 %>% mutate(model = "W5 Highest"),
  contrasts_h5_out_wlt5 %>% mutate(model = "H5 Highest"),
  
  
  contrasts_w6_out %>% mutate(model = "W6"),
  contrasts_h6_out %>% mutate(model = "H6")
  
) %>% 
  write_csv(.,"g2al03_contrasts for poisson regression with multiple imputation.csv")

#----------------------------------
#table main analysis
htn <- read_csv("g2al03_poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("W1","H1")) %>% 
  mutate(outcome = "Hypertension")

table_main <- htn %>% 
  dplyr::filter(!str_detect(iv,"^factor\\(state\\)")) %>% 
  mutate(iv = str_replace(iv,"^(w|h)_","")) %>% 
  dplyr::select(model,outcome,iv,RR) %>% 
  pivot_wider(names_from = c("model","outcome"),values_from="RR")

write_csv(table_main,"table_main analysis results.csv")

#-----------------------------------------
#figure emm analysis
htn <- read_csv("g2al03_contrasts for poisson regression with multiple imputation.csv") %>% 
  mutate(outcome = "Hypertension")

htn_main <- read_csv("g2al03_poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("W1","H1"),iv %in% c("w_htn","h_htn")) %>% 
  mutate(outcome = "Hypertension",
         label = "Overall")

contrast_map <- readxl::read_excel("LASI Contrast Map.xlsx") %>% 
  dplyr::filter(!is.na(label))


tab_stratum <- bind_rows(htn_main,
                         bind_rows(htn) %>% 
                           left_join(contrast_map,
                                     by=c("term","model")) %>% 
                           dplyr::filter(!is.na(label))) %>% 
  dplyr::select(label,outcome,model,RR,theta_D,lci,uci) %>% 
  mutate(est = exp(theta_D),
         sex_self = case_when(str_detect(model,"W") ~ "Wives",
                              TRUE ~ "Husbands")) %>% 
  mutate(label = factor(label,levels=c("Overall",
                                       "Age < 65",
                                       "Age >= 65",
                                       "Education: Less than lower secondary",
                                       "Education: Upper secondary and vocational training",
                                       "Education: Tertiary",
                                       "Urban",
                                       "Rural",
                                       "Wealth: Lowest",
                                       "Wealth: Low",
                                       "Wealth: Medium",
                                       "Wealth: High",
                                       "Wealth: Highest",
                                       "Length of marriage < 10",
                                       "Length of marriage >= 10"), ordered=TRUE))


tab_stratum %>% 
  dplyr::select(label,sex_self,RR) %>% 
  pivot_wider(names_from="sex_self",values_from="RR") %>% 
  
  write_csv(.,"table_emm analysis results.csv")

figA <- tab_stratum %>% dplyr::filter(outcome == "Hypertension") %>% 
  ggplot(data=.,
         aes(x=est,y=label,
             xmin=lci,xmax =uci,
             color=sex_self)) +
  geom_point(position=position_dodge(width=0.9)) +
  geom_errorbar(position=position_dodge(width=0.9),width=0.1) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits=c(0,3),breaks=seq(0,3,by=1)) +
  theme_bw() +
  xlab("Prevalence Ratio (95% CI)") +
  ylab("") +
  geom_vline(xintercept=1.0,color="black",linetype=2) +
  scale_color_manual(name = "Prevalence Ratio in ",values=c("darkblue","red")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)) 

figA
