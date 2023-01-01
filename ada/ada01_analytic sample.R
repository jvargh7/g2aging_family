source("ada/ada_variables.R")

# Datasets -----
hrs <- readRDS(paste0(path_g2a_family_folder,"/working/hrs/G2A HRS Couples mi_dfs.RDS"))$data %>% 
  left_join(read.csv(paste0(path_g2a_concordance_folder,"/working/coupleHRS_dm.csv")) %>% 
              dplyr::select(hhid,contains("medication")) %>% 
              rename(w_medication_bp = w_bp_medication,
                     h_medication_bp = h_bp_medication,
                     w_medication_dm = w_dm_medication,
                     h_medication_dm = h_dm_medication),
            by = "hhid") %>% 
  dplyr::filter(w_diagnosed_dm == 1 | h_diagnosed_dm == 1) %>% 
  dplyr::mutate(w_eligible = case_when(!is.na(w_sbp) & !is.na(w_dbp) ~ 1,
                                        TRUE ~ 0),
                h_eligible = case_when(!is.na(h_sbp) & !is.na(h_dbp) ~ 1,
                                       TRUE ~ 0)) %>% 
  dplyr::filter(w_eligible == 1, h_eligible == 1) %>% 
  dplyr::rename(hh_sampleweight = r_indweight) %>% 
  mutate(residence = case_when(
                               residence == 2 ~ 0,
                               residence == 1 ~ 1)) %>% 
  dplyr::select(one_of(id_selected),one_of(w_selected),one_of(h_selected),one_of(hh_selected),one_of(interaction_terms)) %>% 
  mutate_at(vars(w_diagnosed_dm,h_diagnosed_dm,contains("medication")),function(x) case_when(is.na(x) ~ 0,
                                                           TRUE ~ as.numeric(x))) %>% 
  mutate(w_bp_control = case_when(w_sbp < 140 & w_dbp < 90 ~ 1,
                                  TRUE ~ 0),
         h_bp_control = case_when(h_sbp < 140 & h_dbp < 90 ~ 1,
                                  TRUE ~ 0)
  ) 
saveRDS(hrs,paste0(path_g2a_concordance_folder,"/working/hrs.RDS"))


elsa <- readRDS(paste0(path_g2a_family_folder,"/working/elsa/G2A ELSA Couples mi_dfs.RDS"))$data %>% 
  dplyr::filter(w_diagnosed_dm == 1 | h_diagnosed_dm == 1)  %>% 
  dplyr::mutate(w_eligible = case_when(!is.na(w_sbp) & !is.na(w_dbp) ~ 1,
                                       TRUE ~ 0),
                h_eligible = case_when(!is.na(h_sbp) & !is.na(h_dbp) ~ 1,
                                       TRUE ~ 0)) %>% 
  dplyr::filter(w_eligible == 1, h_eligible == 1) %>% 
  dplyr::select(one_of(id_selected),one_of(w_selected),one_of(h_selected),one_of(hh_selected),one_of(interaction_terms)) %>% 
  mutate_at(vars(w_diagnosed_dm,h_diagnosed_dm,contains("medication")),~case_when(is.na(.) ~ 0,
                                                           TRUE ~ .)) %>% 
  mutate(w_bp_control = case_when(w_sbp < 140 & w_dbp < 90 ~ 1,
                                  TRUE ~ 0),
         h_bp_control = case_when(h_sbp < 140 & h_dbp < 90 ~ 1,
                                  TRUE ~ 0)
  ) 
saveRDS(elsa,paste0(path_g2a_concordance_folder,"/working/elsa.RDS"))


charls <- readRDS(paste0(path_g2a_family_folder,"/working/charls/G2A CHARLS Couples mi_dfs JV.RDS"))$data  %>% 
  left_join(read.csv(paste0(path_g2a_concordance_folder,"/working/coupleCHARLS20230101.csv")) %>% 
              dplyr::select(hhid,contains("medication")) %>% 
              rename(w_medication_bp = w_bp_medication,
                     h_medication_bp = h_bp_medication,
                     w_medication_dm = w_dm_medication,
                     h_medication_dm = h_dm_medication),
            by = "hhid") %>% 
  dplyr::filter(w_diagnosed_dm == 1 | h_diagnosed_dm == 1)  %>% 
  dplyr::rename(hh_sampleweight = r_indweight) %>%
  mutate(residence = case_when(
    residence == 2 ~ 0,
    residence == 1 ~ 1)) %>% 
  dplyr::select(one_of(id_selected),one_of(w_selected),one_of(h_selected),one_of(hh_selected),one_of(interaction_terms)) %>% 
  dplyr::mutate(w_eligible = case_when(!is.na(w_sbp) & !is.na(w_dbp) ~ 1,
                                       TRUE ~ 0),
                h_eligible = case_when(!is.na(h_sbp) & !is.na(h_dbp) ~ 1,
                                       TRUE ~ 0)) %>% 
  dplyr::filter(w_eligible == 1, h_eligible == 1) %>% 
  mutate_at(vars(w_diagnosed_dm,h_diagnosed_dm,contains("medication")),~case_when(is.na(.) ~ 0,
                                                           TRUE ~ as.numeric(.))) %>% 
  mutate(w_bp_control = case_when(w_sbp < 140 & w_dbp < 90 ~ 1,
                                  TRUE ~ 0),
         h_bp_control = case_when(h_sbp < 140 & h_dbp < 90 ~ 1,
                                  TRUE ~ 0)
  ) 
saveRDS(charls,paste0(path_g2a_concordance_folder,"/working/charls.RDS"))


lasi <- readRDS(paste0(path_g2a_family_folder,"/working/lasi/G2A LASI Couples mi_dfs.RDS"))$data %>% 
  dplyr::filter(w_diagnosed_dm == 1 | h_diagnosed_dm == 1)  %>% 
  dplyr::mutate(w_eligible = case_when(!is.na(w_sbp) & !is.na(w_dbp) ~ 1,
                                       TRUE ~ 0),
                h_eligible = case_when(!is.na(h_sbp) & !is.na(h_dbp) ~ 1,
                                       TRUE ~ 0)) %>% 
  dplyr::filter(w_eligible == 1, h_eligible == 1) %>% 
  dplyr::select(one_of(id_selected),one_of(w_selected),one_of(h_selected),one_of(hh_selected),one_of(interaction_terms)) %>% 
  mutate_at(vars(w_diagnosed_dm,h_diagnosed_dm,contains("medication")),~case_when(is.na(.) ~ 0,
                                                           TRUE ~ .)) %>% 
  mutate(w_bp_control = case_when(w_sbp < 140 & w_dbp < 90 ~ 1,
                                  TRUE ~ 0),
         h_bp_control = case_when(h_sbp < 140 & h_dbp < 90 ~ 1,
                                  TRUE ~ 0)
  ) 
saveRDS(lasi,paste0(path_g2a_concordance_folder,"/working/lasi.RDS"))


