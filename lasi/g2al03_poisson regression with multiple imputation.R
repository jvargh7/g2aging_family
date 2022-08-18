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
  overall_w2[[i]] = svyglm(w2,design=svy_des,family=quasipoisson());
  overall_h2[[i]] = svyglm(h2,design=svy_des,family=quasipoisson());
  overall_w3[[i]] = svyglm(w3,design=svy_des,family=quasipoisson());
  overall_h3[[i]] = svyglm(h3,design=svy_des,family=quasipoisson());
  
  
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
overall_w2_out = mice_coef_svyglm(overall_w2)
overall_h2_out = mice_coef_svyglm(overall_h2)
overall_w3_out = mice_coef_svyglm(overall_w3)
overall_h3_out = mice_coef_svyglm(overall_h3)



source("C:/code/external/functions/survey/mice_contrasts_svyglm.R")
# You would also have to download the following:
# source("C:/code/external/functions/preprocessing/prepare_contrasts.R")
# source("C:/code/external/functions/survey/contrasts_svyglm.R")
# The coefficients are in column 'RR'. The rest of the columns are for pooling multiple imputated regressions

contrasts_w2_out = mice_contrasts_svyglm(svymodel_list = overall_w2,modifier = "w_ge65",exposure = "h_htn")
contrasts_h2_out = mice_contrasts_svyglm(svymodel_list = overall_h2,modifier="h_ge65",exposure="w_htn")

contrasts_w3_out_ed2 = mice_contrasts_svyglm(svymodel_list = overall_w3,modifier = "w_education_2",exposure = "h_htn")
contrasts_h3_out_ed2 = mice_contrasts_svyglm(svymodel_list = overall_h3,modifier="h_education_2",exposure="w_htn")

contrasts_w3_out_ed3 = mice_contrasts_svyglm(svymodel_list = overall_w3,modifier = "w_education_3",exposure = "h_htn")
contrasts_h3_out_ed3 = mice_contrasts_svyglm(svymodel_list = overall_h3,modifier="h_education_3",exposure="w_htn")



