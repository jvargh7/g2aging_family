mi_dfs <- readRDS(paste0(path_g2a_family_folder,"/working/charls/G2A CHARLS Couples mi_dfs JV.RDS"))

library(mice)
library(lme4)
library(merDeriv)


g_covariates = "+ bmi + ge65 + education_2 + education_3 + laborforce + smoke + heavydrinker + moderate_pa + vigorous_pa + hukou"
hhg_covariates = "+ hh_low + hh_medium + hh_high + hh_highest + hh_children + hh_size + residence + hh_lengthmar"


## IMPORTANT - Check if coupleid is nested within PSU
## Before proceeding, check variance partition between 'psu' and 'coupleid'

charls_create_pooled <- function(mice_df){
  
  w_selected = c("w_personid","w_htn","h_htn","w_bmi","w_ge65","w_education_2", "w_education_3", "w_laborforce", "w_smoke", "w_heavydrinker", "w_moderate_pa", "w_vigorous_pa","w_hukou")
  h_selected = c("h_personid","h_htn","w_htn","h_bmi", "h_ge65", "h_education_2", "h_education_3", "h_laborforce", "h_smoke", "h_heavydrinker", "h_moderate_pa", "h_vigorous_pa","h_hukou")
  hh_selected = c(
                  "hhid", # Level 2
                  "hh_low", "hh_medium", "hh_high", "hh_highest", "hh_size",  "residence", "hh_lengthmar",
                  "hh_children",
                  # Add survey weight variable here ------
                  "r_indweight")
  
  
  # "hh_children": Cannot be used for LASI 
  
  wives = mice_df %>% 
    dplyr::select(one_of(w_selected),one_of(hh_selected)) %>% 
    rename_at(vars(one_of(w_selected)),~str_replace(.,"^w_","")) %>% 
    rename(partner_htn = h_htn)
  
  husbands = mice_df %>% 
    dplyr::select(one_of(h_selected),one_of(hh_selected)) %>% 
    rename_at(vars(one_of(h_selected)),~str_replace(.,"^h_","")) %>% 
    rename(partner_htn = w_htn)
  
  pooled <- bind_rows(wives %>% 
                        mutate(husband = 0) ,
                      
                      husbands %>% 
                        mutate(husband = 1) )
  
  return(pooled)
  
}


# The levels may vary for different cohorts
# In CHARLS: Household ~ Couple?
mi_dfs$data %>% group_by(hhid) %>% tally() %>% dplyr::filter(n > 1) %>% summarize(n_multiple = sum(n))

# g0 <- paste0("htn ~ (1|hhid/coupleid)") %>% as.formula()

# We can use glmer if we find a package to compute robust SE - commenting it out for now
# g1a <- paste0("htn ~ partner_htn + (1|hhid/coupleid) + ",g_covariates,hhg_covariates) %>% as.formula()
# g1b <- paste0("htn ~ partner_htn + (1|hhid) + ",g_covariates,hhg_covariates) %>% as.formula()

g1b_gee <- paste0("htn ~ partner_htn + husband",g_covariates,hhg_covariates) %>% as.formula()
g2b_gee <- paste0("htn ~ partner_htn*husband",g_covariates,hhg_covariates) %>% as.formula()

# Check if boundary is singular - if yes, use lme4 with only higher level
# Singular if random effects explain near-zero variance
df_1 = complete(mi_dfs,action = 1)
pooled_1 <- charls_create_pooled(df_1)
# vpc_1 = glmer(formula = g0,data=pooled_1,family = poisson())

# If singular, use gee with 1 level since glmer robust SE are not available based on initial search
require(geepack)
g1b_list <- list()
g2b_list <- list()

for(i in 1:mi_dfs$m){
  df = complete(mi_dfs,i)
  
  print(i)
  pooled = charls_create_pooled(df)
  
  # Each regression might take time since generalized linear mixed models are slow to fit 
  
  # if(isSingular(vpc_1)){
  #   
  #   g1b_list[[i]] = glmer(formula = g1b,data=pooled_1,family = poisson())
  #   
  #   
  # } else {
  #   g1b_list[[i]] = glmer(formula = g1a,data=pooled_1,family = poisson())
  # }
  
  g1b_list[[i]] = geeglm(formula = g1b_gee,
                         corstr = "exchangeable",
                         family=poisson(),
                         data = pooled,
                         weights = r_indweight,
                         id = hhid)
  g2b_list[[i]] = geeglm(formula = g2b_gee,
                         corstr = "exchangeable",
                         family=poisson(),
                         data = pooled,
                         weights = r_indweight,
                         id = hhid)
  
}

# Please download the latest version ----

# https://github.com/jvargh7/functions/blob/main/imputation/adjusted_ci.R
# download.file("https://github.com/jvargh7/functions/blob/main/imputation/adjusted_ci.R",destfile = "")
source("C:/code/external/functions/imputation/adjusted_ci.R")
# https://github.com/jvargh7/functions/blob/main/imputation/clean_mi_conditionalregression.R
source("C:/code/external/functions/imputation/clean_mi_conditionalregression.R")

overall_g1_out = clean_mi_conditionalregression(g1b_list,link="geeglm log")
overall_g2_out = clean_mi_conditionalregression(g2b_list,link="geeglm log")

bind_rows(
  overall_g1_out %>% mutate(model = "G1"),
  overall_g2_out %>% mutate(model = "G2")) %>% 
  write_csv(.,"charls/g2ac04_poisson regression for gendered differences.csv")
