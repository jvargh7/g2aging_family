require(srvyr)
source("C:/code/external/functions/survey/svysummary.R")

source("elsa/g2aelsa_analytic sample.R")

continuous_vars <- c(paste0(rep(c("w_","h_"),each=11),
                            c("sbp","dbp","weight","height",
                              "bmi",
                              "age","eduyr","children","lengthmar",
                              "moderate_pa","vigorous_pa")),
                     "hh_size","hh_wealth","hh_income","hh_consumption","hh_lengthmar")

proportion_vars <- c(paste0(rep(c("w_","h_"),each=18),
                            c("screened_bp","diagnosed_bp","medication_bp",
                              "fasting","screened_dm","diagnosed_dm","medication_dm",
                              "heavydrinker",
                              "pregnant","employment","retirement","smokeever","smokecurr","alcohol","lengthmar_ge10",
                              "insurance","dm","htn")),"residence","hh_lengthmar_ge10","hh_htn")

grouped_vars <- c("w_education_h","h_education_h",
                  "h_race","h_religion",
                  "w_race","w_religion",
                  "hh_wealthquintile","hh_consumptionquintile","hh_incometertile",
                  "w_laborforce","w_smoke","h_laborforce","h_smoke"
)

# Husbands summary ----------
husbands_svy <- couples  %>% 
  dplyr::filter(!is.na(h_sampleweight), h_sampleweight > 0) %>% 
  as_survey_design(.data = .,
                   ids = psu,
                   # strata = strata,
                   weight = h_sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")


husbands_svysummary <- svysummary(husbands_svy,
                                 c_vars = continuous_vars[regexpr("^(h_|hh_)",continuous_vars)>0],
                                 p_vars = proportion_vars[regexpr("^(h_|hh_)",proportion_vars)>0],
                                 g_vars = grouped_vars[regexpr("^(h_|hh_)",grouped_vars)>0]
                                 ) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

husbands_count <- couples  %>% 
  dplyr::filter(!is.na(h_sampleweight), h_sampleweight > 0) %>% 
  summarize_at(vars(one_of(c(continuous_vars[regexpr("^(h_|hh_)",continuous_vars)>0],
                             proportion_vars[regexpr("^(h_|hh_)",proportion_vars)>0],
                             grouped_vars[regexpr("^(h_|hh_)",grouped_vars)>0]))),
               list(n = ~sum(!is.na(.)))) %>% 
  pivot_longer(names_to="variable",values_to="n",cols=everything()) %>% 
  mutate(variable = str_replace(variable,"_n$",""))


# Wives summary ----------
wives_svy <- couples  %>% 
  dplyr::filter(!is.na(w_sampleweight), w_sampleweight > 0) %>% 
  as_survey_design(.data = .,
                   ids = psu,
                   # strata = strata,
                   weight = h_sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

wives_svysummary <- svysummary(wives_svy,
                                  c_vars = continuous_vars[regexpr("^(w_|hh_)",continuous_vars)>0],
                                  p_vars = proportion_vars[regexpr("^(w_|hh_)",proportion_vars)>0],
                                  g_vars = grouped_vars[regexpr("^(w_|hh_)",grouped_vars)>0]
) %>% 
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>% 
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

wives_count <- couples  %>% 
  dplyr::filter(!is.na(w_sampleweight), w_sampleweight > 0) %>% 
  summarize_at(vars(one_of(c(continuous_vars[regexpr("^(w_|hh_)",continuous_vars)>0],
                             proportion_vars[regexpr("^(w_|hh_)",proportion_vars)>0],
                             grouped_vars[regexpr("^(w_|hh_)",grouped_vars)>0]))),
               list(n = ~sum(!is.na(.)))) %>% 
  pivot_longer(names_to="variable",values_to="n",cols=everything()) %>% 
  mutate(variable = str_replace(variable,"_n$",""))





left_join(bind_rows(husbands_svysummary %>% mutate(spouse = "Husband"),
                    wives_svysummary %>% mutate(spouse = "Wife")),
          
    bind_rows(husbands_count %>% mutate(spouse = "Husband"),
            wives_count %>% mutate(spouse = "Wife")),
          by=c("variable","spouse")) %>% 
  write_csv(.,"elsa/summary table.csv")

