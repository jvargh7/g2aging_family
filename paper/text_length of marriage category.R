require(srvyr)
source("C:/code/external/functions/survey/svysummary.R")

source("lasi/g2alasi_analytic sample.R")

continuous_vars = c("w_age","h_age")

couples_svy <- couples  %>% 
  as_survey_design(.data = .,
                   ids = psu,strata = state,
                   weight = h_sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

couples_svysummary <- svysummary(couples_svy,
                                 c_vars = continuous_vars,
                                 # p_vars = proportion_vars,
                                 # g_vars = grouped_vars
                                id_vars = "hh_lengthmar_ge10"
                                 ) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))
