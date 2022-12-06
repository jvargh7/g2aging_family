# setwd("E:/OneDrive/spouse concord China US India/Data")
library(dplyr)
library(tidyverse)
library(readr)

hrs <- read_csv("hrs/hrs_contrasts for poisson regression with multiple imputation.csv") %>% 
  mutate(outcome = "Hypertension",
         survey = "HRS")

hrs_main <- read_csv("hrs/hrs_poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("W1","H1"),iv %in% c("w_htn","h_htn")) %>% 
  mutate(outcome = "Hypertension",
         label = "Overall",
         survey = "HRS")


charls <- read_csv("charls/g2ac03_contrasts for poisson regression with multiple imputation.csv") %>% 
  mutate(outcome = "Hypertension",
         survey = "CHARLS")

charls_main <- read_csv("charls/g2ac03_poisson regression with multiple imputation.csv") %>% 
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

elsa <- read_csv("elsa/g2ae03_contrasts for poisson regression with multiple imputation.csv") %>% 
  mutate(outcome = "Hypertension",
         survey = "ELSA") %>% 
  bind_rows(hrs %>% 
              dplyr::filter(str_detect(term,"lengthmar")) %>% 
              mutate(survey = "ELSA",
                     RR = NA_character_) %>% 
              mutate_if(is.numeric, ~NA_real_))

elsa_main <- read_csv("elsa/g2ae03_poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("W1","H1"),iv %in% c("w_htn","h_htn")) %>% 
  mutate(outcome = "Hypertension",
         label = "Overall",
         survey = "ELSA")

contrast_map <- readxl::read_excel("lasi/LASI Contrast Map.xlsx") %>% 
  dplyr::filter(!is.na(label))


tab_stratum <- bind_rows(hrs_main,
                         charls_main,
                         lasi_main,
                         elsa_main,
                         bind_rows(hrs,
                                   charls,
                                   lasi,
                                   elsa
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
  
  write_csv(.,"table_emm analysis results.csv")

figA <- tab_stratum %>% dplyr::filter(outcome == "Hypertension",survey == "HRS") %>% 
  ggplot(data=.,
         aes(x=est,y=label,
             xmin=lci,xmax =uci,
             color=sex_self)) +
  geom_point(position=position_dodge(width=0.9)) +
  geom_errorbar(position=position_dodge(width=0.9),width=0.1) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits=c(0,3),breaks=seq(0,2.5,by=0.5)) +
  theme_bw() +
 # xlab("Prevalence Ratio (95% CI)") +
  ylab("") +
  geom_vline(xintercept=1.0,color="black",linetype=2) +
  scale_color_manual(name = "Prevalence Ratio in ",values=c("darkblue","red")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)) +
  theme(legend.position="left")+
  labs(title="HRS")

figB <- tab_stratum %>% dplyr::filter(outcome == "Hypertension",survey == "CHARLS") %>% 
  ggplot(data=.,
         aes(x=est,y=label,
             xmin=lci,xmax =uci,
             color=sex_self)) +
  geom_point(position=position_dodge(width=0.9)) +
  geom_errorbar(position=position_dodge(width=0.9),width=0.1) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits=c(0,3),breaks=seq(0,2.5,by=0.5)) +
  theme_bw() +
 # xlab("Prevalence Ratio (95% CI)") +
  ylab("") +
  geom_vline(xintercept=1.0,color="black",linetype=2) +
  scale_color_manual(name = "Prevalence Ratio in ",values=c("darkblue","red")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(), # This removes y axis labels for CHARLS
        axis.title = element_text(size = 10))  +
  theme(legend.position="left")+
  labs(title="CHARLS")

figC <- tab_stratum %>% dplyr::filter(outcome == "Hypertension",survey == "LASI") %>% 
  ggplot(data=.,
         aes(x=est,y=label,
             xmin=lci,xmax =uci,
             color=sex_self)) +
  geom_point(position=position_dodge(width=0.9)) +
  geom_errorbar(position=position_dodge(width=0.9),width=0.1) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits=c(0,3),breaks=seq(0,2.5,by=0.5)) +
  theme_bw() +
#  xlab("Prevalence Ratio (95% CI)") +
  ylab("") +
  geom_vline(xintercept=1.0,color="black",linetype=2) +
  scale_color_manual(name = "Prevalence Ratio in ",values=c("darkblue","red")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(), # This removes y axis labels for LASI
        axis.title = element_text(size = 10))  +
  theme(legend.position="left")+
  labs(title="LASI")


figD <- tab_stratum %>% dplyr::filter(outcome == "Hypertension",survey == "ELSA") %>% 
  ggplot(data=.,
         aes(x=est,y=label,
             xmin=lci,xmax =uci,
             color=sex_self)) +
  geom_point(position=position_dodge(width=0.9)) +
  geom_errorbar(position=position_dodge(width=0.9),width=0.1) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits=c(0,3),breaks=seq(0,2.5,by=0.5)) +
  theme_bw() +
#  xlab("Prevalence Ratio (95% CI)") +
  ylab("") +
  geom_vline(xintercept=1.0,color="black",linetype=2) +
  scale_color_manual(name = "Prevalence Ratio in ",values=c("darkblue","red")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_blank(), # This removes y axis labels for LASI
        axis.title = element_text(size = 10))  +
  theme(legend.position="left")+
  labs(title="ELSA")

require(ggpubr)
# jpeg("Supplemental Figure 2. combined CI.jpeg", width = 10, height = 6, units = 'in', res = 600)
ggarrange(figA,
          figD,
          figB,
          figC,
          common.legend=TRUE,nrow=1,ncol=4,widths = c(4.2,1.5,1.5, 1.5)) %>% 
  ggsave(.,filename = paste0(path_g2a_family_folder,"/figures/Supplemental Figure 2_combined CI.jpeg"),
         width = 10, height=6,units='in',dpi = 600)
