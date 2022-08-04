mi_dfs <- readRDS(paste0(path_g2a_family_folder,"/working/G2A LASI Couples mi_dfs.RDS"))

require(mice)
require(srvyr)
require(survey)

source("lasi/g2alasi_poisson regression equations.R")

# Run Poisson Regression ------------

for(i in 1:mi_dfs$m){
  df = complete(mi_dfs,action = i);
  
  svy_des = df %>% 
    as_survey_design(.data = .,
                     ids = psu,strata = state,
                     weight = h_sampleweight,
                     nest = TRUE,
                     variance = "YG",pps = "brewer");
  
  overall_w1[[i]] = svyglm(w1,design=svy_des,family=quasipoisson());
  overall_h1[[i]] = svyglm(h1,design=svy_des,family=quasipoisson());
  
  
  gc();rm(df);rm(svy_des)
}

# Pooling coefficients ------------
source("C:/code/external/functions/survey/mice_coef_svyglm.R")
# Check https://github.com/jvargh7/functions/blob/main/survey/mice_coef_svyglm.R
# You would also have to download the following:
# a. https://github.com/jvargh7/functions/blob/main/imputation/adjusted_ci.R
# b. https://github.com/jvargh7/functions/tree/main/preprocessing

overall_w1_out = mice_coef_svyglm(overall_w1)
overall_h1_out = mice_coef_svyglm(overall_h1)



