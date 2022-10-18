htn <- read_csv("elsa/g2ae03_poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("W1","H1")) %>% 
  mutate(outcome = "Hypertension")

table_main <- htn %>% 
  dplyr::filter(!str_detect(iv,"^factor\\(state\\)")) %>% 
  mutate(iv = str_replace(iv,"^(w|h)_","")) %>% 
  dplyr::select(model,outcome,iv,RR) %>% 
  pivot_wider(names_from = c("model","outcome"),values_from="RR")

write_csv(table_main,"elsa/table_main analysis results.csv")
