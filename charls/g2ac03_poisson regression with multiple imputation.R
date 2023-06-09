

if(Sys.info()["user"] == "JVARGH7"){
  coupleCHARLS <- read.csv(paste0(path_g2a_family_folder,"/working/charls/coupleCHARLS.csv"))
  coupleCHARLS <- coupleCHARLS[, -1]
  
  
} else{
  setwd("G:\\My Drive\\Crossnation study\\Dataset")
  coupleCHARLS <- read.csv("coupleCHARLS.csv")
  coupleCHARLS <- coupleCHARLS[, -1]
  #correlation between hhsize and number of children
  cor.test(coupleCHARLS$hh_children, coupleCHARLS$hh_size)
  
  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")
  
  #check Kappa coefficient
  colnames(coupleCHARLS)
  cohen.kappa(coupleCHARLS[, c(57, 58)])
}




#Poisson with MI data
if(Sys.info()["user"] == "JVARGH7"){
  mi_dfs <- readRDS(paste0(path_g2a_family_folder,"/working/charls/G2A CHARLS Couples mi_dfs JV.RDS"))
  source("charls/g2ahrs_poisson regression equations_CHARLS.R")
  source("C:/code/external/functions/survey/mice_coef_svyglm.R")
  
  source("C:/code/external/functions/survey/mice_contrasts_svyglm.R")
  
  
} else{
  mi_dfs <- readRDS("G2A CHARLS Couples mi_dfs.RDS")
source("g2ahrs_poisson regression equations_CHARLS.R")
  source("mice_coef_svyglm.R")
  source("mice_contrasts_svyglm.R")
}

require(mice)
require(srvyr)
require(survey)


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
  overall_w0[[i]] = svyglm(w0,design=svy_des,family=quasipoisson());
  overall_h0[[i]] = svyglm(h0,design=svy_des,family=quasipoisson());
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
  overall_w7[[i]] = svyglm(w7,design=svy_des,family=quasipoisson());
  overall_h7[[i]] = svyglm(h7,design=svy_des,family=quasipoisson());
  overall_w8[[i]] = svyglm(w8,design=svy_des,family=quasipoisson());
  overall_h8[[i]] = svyglm(h8,design=svy_des,family=quasipoisson());
  
  
  gc();rm(df);rm(svy_des)
}

# Pooling coefficients ------------

# Check https://github.com/jvargh7/functions/blob/main/survey/mice_coef_svyglm.R
# You would also have to download the following:
# a. https://github.com/jvargh7/functions/blob/main/imputation/adjusted_ci.R
# b. https://github.com/jvargh7/functions/tree/main/preprocessing
overall_w0_out = mice_coef_svyglm(overall_w0)
overall_h0_out = mice_coef_svyglm(overall_h0)
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
overall_w7_out = mice_coef_svyglm(overall_w7)
overall_h7_out = mice_coef_svyglm(overall_h7)
overall_w8_out = mice_coef_svyglm(overall_w8)
overall_h8_out = mice_coef_svyglm(overall_h8)

bind_rows(
  overall_w0_out %>% mutate(model = "W0"),
  overall_h0_out %>% mutate(model = "H0"),
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
  overall_h6_out %>% mutate(model = "H6"),
  overall_w7_out %>% mutate(model = "W7"),
  overall_h7_out %>% mutate(model = "H7"),
  overall_w8_out %>% mutate(model = "W8"),
  overall_h8_out %>% mutate(model = "H8")
  
) %>% 
  write_csv(.,"charls/g2ac03_poisson regression with multiple imputation.csv")


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

contrasts_w6_out = mice_contrasts_svyglm(svymodel_list = overall_w6,modifier = "hh_lengthmar_ge101",exposure = "h_htn")
contrasts_h6_out = mice_contrasts_svyglm(svymodel_list = overall_h6,modifier = "hh_lengthmar_ge101",exposure = "w_htn")

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
  write_csv(.,"charls/g2ac03_contrasts for poisson regression with multiple imputation.csv")

