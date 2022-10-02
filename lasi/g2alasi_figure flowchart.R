couples <- readRDS(paste0(path_g2a_family_folder,"/working/G2A LASI Couples.RDS")) 


legal_age <- couples %>%
  dplyr::filter(w_age >= 18, h_age >= 21)


self_htn_yes <- legal_age %>% 
  dplyr::filter(w_diagnosed_bp == 1, h_diagnosed_bp == 1)

self_htn_other <- legal_age %>% 
  dplyr::filter(!(w_diagnosed_bp == 1 & h_diagnosed_bp == 1)) 


sbp_dbp <- self_htn_other %>% 
  mutate(w_eligible = case_when(w_diagnosed_bp == 1 ~ 1,
                                !is.na(w_sbp) & !is.na(w_dbp) ~ 1,
                                TRUE ~ 0),
         h_eligible = case_when(h_diagnosed_bp == 1 ~ 1,
                                !is.na(h_sbp) & !is.na(h_dbp) ~ 1,
                                TRUE ~ 0)) %>% 
  dplyr::filter(w_eligible == 1,h_eligible == 1)
