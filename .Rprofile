require(tidyverse)
require(haven)

if(Sys.info()["user"]=="JVARGH7"){
  path_g2a_data <- "C:/Cloud/OneDrive - Emory University/data/G2Aging"
  path_g2a_family_folder <- "C:/Cloud/OneDrive - Emory University/Papers/Crossnational Family Clustering"
}

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

bmi_max <- 60.00
bmi_cutoff <- c(18.50, 25.00, 30.00)
# Non-Asians
female_wc_cutoff = 80 
female_whr_cutoff = 0.80
male_wc_cutoff = 94 
male_whr_cutoff = 0.95


# Asians
# female_wc_cutoff = 80 
# female_whr_cutoff = 0.85
# male_wc_cutoff = 90 
# male_whr_cutoff = 0.9
fpg_cutoff <- 126
rpg_cutoff <- 200
sbp_cutoff <- 140
dbp_cutoff <- 90

fpgpre_cutoff <- 100
rpgpre_cutoff <- 140
sbppre_cutoff <- 130
dbppre_cutoff <- 85
