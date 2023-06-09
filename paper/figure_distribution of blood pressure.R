hrs_mi <- readRDS(paste0(path_g2a_family_folder,"/working/hrs/G2A HRS Couples mi_dfs.RDS"))
elsa_mi <- readRDS(paste0(path_g2a_family_folder,"/working/elsa/G2A ELSA Couples mi_dfs.RDS"))
charls_mi <- readRDS(paste0(path_g2a_family_folder,"/working/charls/G2A CHARLS Couples mi_dfs JV.RDS"))
lasi_mi <- readRDS(paste0(path_g2a_family_folder,"/working/lasi/G2A LASI Couples mi_dfs.RDS"))


pooled <- bind_rows(
  hrs_mi$data %>% 
    dplyr::rename(hh_sampleweight = r_indweight,
                  coupleid = hhid) %>% 
    dplyr::select(coupleid,h_sbp,h_dbp,w_sbp,w_dbp,hh_sampleweight) %>% 
    mutate(cohort = 1,
           w_sampleweight = hh_sampleweight,
           h_sampleweight = hh_sampleweight),
  elsa_mi$data %>% 
    dplyr::select(coupleid,h_sbp,h_dbp,w_sbp,w_dbp,w_sampleweight,h_sampleweight) %>% 
    mutate(cohort = 2),
  charls_mi$data %>% 
    dplyr::rename(hh_sampleweight = r_indweight,
                  coupleid = hhcoupid) %>% 
    dplyr::select(coupleid,h_sbp,h_dbp,w_sbp,w_dbp,hh_sampleweight) %>% 
    mutate(cohort = 3,
           w_sampleweight = hh_sampleweight,
           h_sampleweight = hh_sampleweight),
  lasi_mi$data %>% 
    dplyr::select(coupleid,h_sbp,h_dbp,w_sbp,w_dbp,h_sampleweight) %>% 
    mutate(cohort = 4,
           w_sampleweight = h_sampleweight))  %>% 
  mutate(cohort = factor(cohort,levels=c(1:4),labels=c("USA","England","China","India")))

# Common parameters ------------

ylabel = "Weighted Frequency"
fill_values = c("red","darkblue","orange","darkgreen")

figA <- pooled  %>% 
  dplyr::filter(!is.na(w_sampleweight), w_sampleweight > 0) %>% 
  group_by(cohort) %>% 
  mutate(w_sampleweight = w_sampleweight/sum(w_sampleweight)) %>% 
  mutate(w_sampleweight = w_sampleweight/n()) %>% 
  ungroup() %>% 
  mutate(w_sampleweight = w_sampleweight*n()) %>% 
  
  ggplot(data=.,aes(weight=w_sampleweight,fill=cohort,x=w_sbp)) +
  geom_density(alpha=0.2) +
  theme_bw() +
  xlab("Systolic Blood Pressure") +
  ylab(ylabel) +
  scale_fill_manual(name = "",values=fill_values) +
  geom_vline(xintercept=140,col="red",linetype=2) +
  scale_x_continuous(limits=c(50,250),breaks=seq(50,250,by=50))

figC <- pooled  %>% 
  dplyr::filter(!is.na(w_sampleweight), w_sampleweight > 0) %>% 
  group_by(cohort) %>% 
  mutate(w_sampleweight = w_sampleweight/sum(w_sampleweight)) %>% 
  mutate(w_sampleweight = w_sampleweight/n()) %>% 
  ungroup() %>% 
  mutate(w_sampleweight = w_sampleweight*n()) %>% 
  
  ggplot(data=.,aes(weight=w_sampleweight,fill=cohort,x=w_dbp)) +
  geom_density(alpha=0.2) +
  theme_bw() +
  xlab("Diastolic Blood Pressure") +
  ylab(ylabel) +
  scale_fill_manual(name = "",values=fill_values) +
  geom_vline(xintercept=90,col="red",linetype=2)  +
  scale_x_continuous(limits=c(50,250),breaks=seq(50,250,by=50))


figB <- pooled  %>% 
  dplyr::filter(!is.na(h_sampleweight), h_sampleweight > 0) %>% 
  group_by(cohort) %>% 
  mutate(h_sampleweight = h_sampleweight/sum(h_sampleweight)) %>% 
  mutate(h_sampleweight = h_sampleweight/n()) %>% 
  ungroup() %>% 
  mutate(h_sampleweight = h_sampleweight*n()) %>% 
  
  ggplot(data=.,aes(weight=h_sampleweight,fill=cohort,x=h_sbp)) +
  geom_density(alpha=0.2) +
  theme_bw() +
  xlab("Systolic Blood Pressure") +
  ylab(ylabel) +
  scale_fill_manual(name = "",values=fill_values) +
  geom_vline(xintercept=140,col="red",linetype=2)  +
  scale_x_continuous(limits=c(50,250),breaks=seq(50,250,by=50))

figD <- pooled  %>% 
  dplyr::filter(!is.na(h_sampleweight), h_sampleweight > 0) %>% 
  group_by(cohort) %>% 
  mutate(h_sampleweight = h_sampleweight/sum(h_sampleweight)) %>% 
  mutate(h_sampleweight = h_sampleweight/n()) %>% 
  ungroup() %>% 
  mutate(h_sampleweight = h_sampleweight*n()) %>% 
  
  ggplot(data=.,aes(weight=h_sampleweight,fill=cohort,x=h_dbp)) +
  geom_density(alpha=0.2) +
  theme_bw() +
  xlab("Diastolic Blood Pressure") +
  ylab(ylabel) +
  scale_fill_manual(name = "",values=fill_values) +
  geom_vline(xintercept=90,col="red",linetype=2) +
  scale_x_continuous(limits=c(50,250),breaks=seq(50,250,by=50))


library(ggpubr)
ggarrange(figA,figB,figC,figD,
          nrow=2,ncol=2,
          labels = LETTERS[1:4],
          common.legend= TRUE,
          legend = "bottom") %>% 
  ggsave(.,filename=paste0(path_g2a_family_folder,"/figures/distribution of blood pressure.png"),width=8,height=8)
