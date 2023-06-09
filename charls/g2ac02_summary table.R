
coupleCHARLS <- read.csv(paste0(path_g2a_family_folder,"/working/charls/coupleCHARLS.csv"))
coupleCHARLS <- coupleCHARLS[, -1]
mean(coupleCHARLS$h_age < 45 | coupleCHARLS$w_age < 45)

require(srvyr)
colnames(coupleCHARLS)
if(Sys.info()["user"] == "JVARGH7"){
  source("C:/code/external/functions/survey/svysummary.R")
  
} else{
  source("svysummary.R")
  
}

summary(coupleCHARLS$h_age - coupleCHARLS$w_age)

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

full_join(couples_svysummary,
          couples_count,
          by = "variable") %>% 
  write_csv(.,"charls/CHARLS_table1.csv")
