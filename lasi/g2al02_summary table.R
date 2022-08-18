require(srvyr)
source("C:/code/external/functions/survey/svysummary.R")

couples <- readRDS(paste0(path_g2a_family_folder,"/working/G2A LASI Couples.RDS")) %>% 
  mutate_at(vars(w_smokeever,w_smokecurr,w_insurance,
                 h_smokeever,h_smokecurr,h_insurance),function(x) case_when(is.na(x) ~ 0,
                                                                            TRUE ~ x))

continuous_vars <- c(paste0(rep(c("w_","h_"),each=14),
                          c("sbp","dbp","glucose","weight","height",
                            "bmi","waistcircumference","hipcircumference",
                            "age","eduyr","children","lengthmar",
                            "moderate_pa","vigorous_pa")),
                     "hh_size","hh_wealth","hh_income","hh_consumption","hh_lengthmar")

proportion_vars <- c(paste0(rep(c("w_","h_"),each=17),
                          c("screened_bp","diagnosed_bp","medication_bp",
                            "fasting","screened_dm","diagnosed_dm","medication_dm",
                            
                            "pregnant","employment","retirement","smokeever","smokecurr","alcohol","lengthmar_ge10",
                            "insurance","dm","htn")),"residence","hh_lengthmar_ge10")

grouped_vars <- c("w_education","w_education_h","h_education","h_education_h","in_caste","in_religion",
                  "hh_wealthquintile","hh_consumptionquintile","hh_incometertile")


couples_svy <- couples  %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = h_sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

couples_svysummary <- svysummary(couples_svy,
                                 c_vars = continuous_vars,
                                 p_vars = proportion_vars,
                                 g_vars = grouped_vars) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

couples_count <- couples %>% 
  summarize_at(vars(one_of(c(continuous_vars,
                             proportion_vars,
                             grouped_vars))),
               list(n = ~sum(!is.na(.)))) %>% 
  pivot_longer(names_to="variable",values_to="n",cols=everything()) %>% 
  mutate(variable = str_replace(variable,"_n$",""))


left_join(couples_svysummary,
          couples_count,
          by="variable") %>% 
  write_csv(.,"lasi/summary table.csv")

