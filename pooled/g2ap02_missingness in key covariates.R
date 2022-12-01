hrs_table1 <- read_csv("hrs/HRS_table1.csv")
elsa_table1 <- read_csv("elsa/summary table.csv")
charls_table1 <- read_csv("charls/CHARLS_table1.csv")
lasi_table1 <- read_csv("lasi/summary table.csv")



df = bind_rows(
  hrs_table1 %>% 
      distinct(variable,n) %>% 
      mutate(country = 1,
             spouse = NA,
             missing = max(n,na.rm=TRUE) - n,
             prop = n/max(n,na.rm=TRUE)),
    elsa_table1 %>% 
    distinct(variable,spouse,n) %>% 
    group_by(spouse) %>% 
      mutate(country = 2,
             missing = max(n,na.rm=TRUE) - n,
             prop = n/max(n,na.rm=TRUE)) %>% 
    ungroup(),
    charls_table1 %>% 
    distinct(variable,n) %>% 
      mutate(country = 3,
             spouse = NA,
             missing = max(n,na.rm=TRUE) - n,
             prop = n/max(n,na.rm=TRUE)),
    lasi_table1 %>% 
    distinct(variable,n) %>% 
      mutate(country = 4,
             spouse = NA,
             missing = max(n,na.rm=TRUE) - n,
             prop = n/max(n,na.rm=TRUE))
  ) %>% 
    mutate(country = factor(country,levels=c(1:4),labels=c("HRS","ELSA","CHARLS","LASI")))

rownames(df) <- NULL


couples_count <- df %>% 
  dplyr::select(-prop,-n) %>% 
  pivot_wider(names_from=c("country","spouse"),values_from="missing") %>% 
  dplyr::select(variable,HRS_NA,ELSA_Wife,ELSA_Husband,CHARLS_NA,LASI_NA)



  

couples_count %>% 
  write_csv(.,"pooled/g2ap02_missingness in key covariates.csv")
