hrs_mi <- readRDS(paste0(path_g2a_family_folder,"/working/hrs/G2A HRS Couples mi_dfs.RDS"))
elsa_mi <- readRDS(paste0(path_g2a_family_folder,"/working/elsa/G2A ELSA Couples mi_dfs.RDS"))
charls_mi <- readRDS(paste0(path_g2a_family_folder,"/working/charls/G2A CHARLS Couples mi_dfs JV.RDS"))
lasi_mi <- readRDS(paste0(path_g2a_family_folder,"/working/lasi/G2A LASI Couples mi_dfs.RDS"))



df = bind_rows(
    hrs_mi$where %>% data.frame() %>% 
      
      mutate(country = 1),
    elsa_mi$where %>% data.frame() %>% 
      mutate(country = 2),
    charls_mi$where %>% data.frame() %>% 
      mutate(country = 3),
    lasi_mi$where %>% data.frame() %>% 
      mutate(country = 4)
  ) %>% 
    mutate(country = factor(country,levels=c(1:4),labels=c("HRS","ELSA","CHARLS","LASI")))

rownames(df) <- NULL


couples_count <- df %>% 
  group_by(country) %>% 
  summarize_at(vars(-one_of("country")),list(n = ~sum(.*1))) %>% 
  pivot_longer(names_to="variable",values_to="n",cols=-one_of("country")) %>% 
  mutate(variable = str_replace(variable,"_n$","")) %>% 
  pivot_wider(names_from="country",values_from="n")

couples_count %>% 
  write_csv(.,"pooled/g2ap02_missingness in key covariates.csv")
