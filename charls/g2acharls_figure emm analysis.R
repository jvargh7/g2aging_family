#----------------------------------
#table main analysis
htn <- read_csv("charls/g2al03_poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("W1","H1")) %>% 
  mutate(outcome = "Hypertension")

table_main <- htn %>% 
  dplyr::filter(!str_detect(iv,"^factor\\(state\\)")) %>% 
  mutate(iv = str_replace(iv,"^(w|h)_","")) %>% 
  dplyr::select(model,outcome,iv,RR) %>% 
  pivot_wider(names_from = c("model","outcome"),values_from="RR")

write_csv(table_main,"charls/table_main analysis results.csv")

#-----------------------------------------
#figure emm analysis
htn <- read_csv("charls/g2al03_contrasts for poisson regression with multiple imputation.csv") %>% 
  mutate(outcome = "Hypertension")

htn_main <- read_csv("charls/g2al03_poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("W1","H1"),iv %in% c("w_htn","h_htn")) %>% 
  mutate(outcome = "Hypertension",
         label = "Overall")

contrast_map <- readxl::read_excel("lasi/LASI Contrast Map.xlsx") %>% 
  dplyr::filter(!is.na(label))


tab_stratum <- bind_rows(htn_main,
                         bind_rows(htn) %>% 
                           left_join(contrast_map,
                                     by=c("term","model")) %>% 
                           dplyr::filter(!is.na(label))) %>% 
  dplyr::select(label,outcome,model,RR,theta_D,lci,uci) %>% 
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
                                       "Length of marriage >= 10"), ordered=TRUE))


tab_stratum %>% 
  dplyr::select(label,sex_self,RR) %>% 
  pivot_wider(names_from="sex_self",values_from="RR") %>% 
  
  write_csv(.,"charls/table_emm analysis results.csv")

figA <- tab_stratum %>% dplyr::filter(outcome == "Hypertension") %>% 
  ggplot(data=.,
         aes(x=est,y=label,
             xmin=lci,xmax =uci,
             color=sex_self)) +
  geom_point(position=position_dodge(width=0.9)) +
  geom_errorbar(position=position_dodge(width=0.9),width=0.1) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits=c(0,3),breaks=seq(0,3,by=1)) +
  theme_bw() +
  xlab("Prevalence Ratio (95% CI)") +
  ylab("") +
  geom_vline(xintercept=1.0,color="black",linetype=2) +
  scale_color_manual(name = "Prevalence Ratio in ",values=c("darkblue","red")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)) 

figA
