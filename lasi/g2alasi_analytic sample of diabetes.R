diabetes <- readRDS(paste0(path_g2a_family_folder,"/working/lasi/G2A LASI Couples.RDS")) %>% 
  mutate_at(vars(w_smokeever,w_smokecurr,w_insurance,
                 h_smokeever,h_smokecurr,h_insurance),function(x) case_when(is.na(x) ~ 0,
                                                                            TRUE ~ x))  %>% 
  mutate(hh_children = apply(.[,c("h_children","w_children")],1,max,na.rm=TRUE) %>% as.numeric(.)) %>% 
  mutate(w_eligible = case_when(# w_diagnosed_bp == 1 ~ 1,
                                !is.na(w_sbp) & !is.na(w_dbp) & w_diagnosed_dm == 1 ~ 1,
                                TRUE ~ 0),
         h_eligible = case_when(# h_diagnosed_bp == 1 ~ 1,
                                !is.na(h_sbp) & !is.na(h_dbp) & h_diagnosed_dm == 1 ~ 1,
                                TRUE ~ 0)) %>% 
  dplyr::filter(w_eligible == 1, h_eligible == 1)

