library(binom)
library(srvyr)

source("elsa/g2aelsa_analytic sample.R")
household_svy <- couples  %>% 
  dplyr::filter(!is.na(h_sampleweight)|!is.na(w_sampleweight)) %>% 
  mutate(hh_sampleweight = case_when(is.na(h_sampleweight) ~ w_sampleweight,
                                     TRUE ~ h_sampleweight)) %>% 
  as_survey_design(.data = .,
                   ids = psu,
                   # strata = strata,
                   weight = hh_sampleweight,
                   nest = TRUE,
                   variance = "YG",pps = "brewer")

household_svy %>% 
  group_by(hh_lengthmar_ge10) %>% 
  summarize(t = survey_total())

binom.confint(679,680,conf.level = 0.95)
