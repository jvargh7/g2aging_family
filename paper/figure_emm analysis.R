

hrs <- read_csv("hrs/g2al03_contrasts for poisson regression with multiple imputation.csv") %>% 
  mutate(outcome = "Hypertension",
         survey = "HRS")

hrs_main <- read_csv("hrs/g2al03_poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("W1","H1"),iv %in% c("w_htn","h_htn")) %>% 
  mutate(outcome = "Hypertension",
         label = "Overall",
         survey = "HRS")


charls <- read_csv("charls/g2al03_contrasts for poisson regression with multiple imputation.csv") %>% 
  mutate(outcome = "Hypertension",
         survey = "CHARLS")

charls_main <- read_csv("charls/g2al03_poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("W1","H1"),iv %in% c("w_htn","h_htn")) %>% 
  mutate(outcome = "Hypertension",
         label = "Overall",
         survey = "CHARLS")


lasi <- read_csv("lasi/g2al03_contrasts for poisson regression with multiple imputation.csv") %>% 
  mutate(outcome = "Hypertension",
         survey = "LASI")

lasi_main <- read_csv("lasi/g2al03_poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("W1","H1"),iv %in% c("w_htn","h_htn")) %>% 
  mutate(outcome = "Hypertension",
         label = "Overall",
         survey = "LASI")


contrast_map <- readxl::read_excel("lasi/LASI Contrast Map.xlsx") %>% 
  dplyr::filter(!is.na(label))


tab_stratum <- bind_rows(hrs_main,
                         charls_main,
                         lasi_main,
                         bind_rows(hrs,
                                   charls,
                                   lasi
                                   ) %>% 
                           left_join(contrast_map,
                                     by=c("term","model")) %>% 
                           dplyr::filter(!is.na(label))) %>% 
  dplyr::select(label,outcome,survey, model,RR,theta_D,lci,uci) %>% 
  mutate(est = exp(theta_D),
         sex_self = case_when(str_detect(model,"W") ~ "Wives",
                              TRUE ~ "Husbands")) %>% 
  mutate(label = factor(label,levels=c("Overall",
                                       "Age < 65",
                                       "Age >= 65",
                                       "Education: Less than lower secondary",
                                       "Education: Upper secondary and vocational training",
                                       "Education: Tertiary",
                                       "Urban",
                                       "Rural",
                                       "Wealth: Lowest",
                                       "Wealth: Low",
                                       "Wealth: Medium",
                                       "Wealth: High",
                                       "Wealth: Highest",
                                       "Length of marriage < 10",
                                       "Length of marriage >= 10"),ordered=TRUE))


tab_stratum %>% 
  dplyr::select(label,survey,sex_self,RR) %>% 
  pivot_wider(names_from="sex_self",values_from="RR") %>% 
  
  write_csv(.,"paper/table_emm analysis results.csv")

figA <- tab_stratum %>% dplyr::filter(outcome == "Hypertension",survey == "HRS") %>% 
  ggplot(data=.,
         aes(x=est,y=label,
             xmin=lci,xmax =uci,
             color=sex_self)) +
  geom_point(position=position_dodge(width=0.9)) +
  geom_errorbar(position=position_dodge(width=0.9),width=0.1) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits=c(0,3),breaks=seq(0,2,by=0.5)) +
  theme_bw() +
  xlab("Prevalence Ratio (95% CI)") +
  ylab("") +
  geom_vline(xintercept=1.0,color="black",linetype=2) +
  scale_color_manual(name = "Prevalence Ratio in ",values=c("darkblue","red")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)) 

figB <- tab_stratum %>% dplyr::filter(outcome == "Hypertension",survey == "CHARLS") %>% 
  ggplot(data=.,
         aes(x=est,y=label,
             xmin=lci,xmax =uci,
             color=sex_self)) +
  geom_point(position=position_dodge(width=0.9)) +
  geom_errorbar(position=position_dodge(width=0.9),width=0.1) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits=c(0,3),breaks=seq(0,2,by=0.5)) +
  theme_bw() +
  xlab("Prevalence Ratio (95% CI)") +
  ylab("") +
  geom_vline(xintercept=1.0,color="black",linetype=2) +
  scale_color_manual(name = "Prevalence Ratio in ",values=c("darkblue","red")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(), # This removes y axis labels for CHARLS
        axis.title = element_text(size = 10)) 

figC <- tab_stratum %>% dplyr::filter(outcome == "Hypertension",survey == "LASI") %>% 
  ggplot(data=.,
         aes(x=est,y=label,
             xmin=lci,xmax =uci,
             color=sex_self)) +
  geom_point(position=position_dodge(width=0.9)) +
  geom_errorbar(position=position_dodge(width=0.9),width=0.1) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits=c(0,3),breaks=seq(0,2,by=0.5)) +
  theme_bw() +
  xlab("Prevalence Ratio (95% CI)") +
  ylab("") +
  geom_vline(xintercept=1.0,color="black",linetype=2) +
  scale_color_manual(name = "Prevalence Ratio in ",values=c("darkblue","red")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(), # This removes y axis labels for LASI
        axis.title = element_text(size = 10)) 


require(ggpubr)

ggarrange(figA,
          figB,
          figC,
          common.legend=TRUE,
          labels=c("A","B","C"),nrow=1,ncol=3,widths = c(2.5,1.5,1.5)) %>% 
  # Enter folder path to save here...
  ggsave(.,"/enter/folder/path/here.png",width = 12,height=6)
