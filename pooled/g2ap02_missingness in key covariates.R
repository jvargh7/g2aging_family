hrs_table1 <- read_csv("hrs/HRS_table1.csv")
elsa_table1 <- read_csv("elsa/summary table.csv")
charls_table1 <- read_csv("charls/CHARLS_table1.csv")
lasi_table1 <- read_csv("lasi/summary table.csv")



df = bind_rows(
  hrs_table1 %>% 
      distinct(variable,n) %>% 
      mutate(country = 1,
             spouse = NA,
             prop = n/max(hrs_table1$n)),
    elsa_table1 %>% 
    distinct(variable,spouse,n) %>% 
      mutate(country = 2,
             prop = n/max(elsa_table1$n)),
    charls_table1 %>% 
    distinct(variable,n) %>% 
      mutate(country = 3,
             spouse = NA,
             prop = n/max(charls_table1$n)),
    lasi_table1 %>% 
    distinct(variable,n) %>% 
      mutate(country = 4,
             spouse = NA,
             prop = n/max(lasi_table1$n))
  ) %>% 
    mutate(country = factor(country,levels=c(1:4),labels=c("HRS","ELSA","CHARLS","LASI")))

rownames(df) <- NULL


couples_count <- df %>% 
  dplyr::select(-prop) %>% 
  pivot_wider(names_from=c("country","spouse"),values_from="n")

couples_count %>% 
  write_csv(.,"pooled/g2ap02_missingness in key covariates.csv")
