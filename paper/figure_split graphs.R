

hrs_main <- read_csv("hrs/hrs_poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("W1","H1"),iv %in% c("w_htn","h_htn")) %>% 
  mutate(outcome = "Hypertension",
         label = "Overall",
         survey = "USA")

elsa_main <- read_csv("elsa/g2ae03_poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("W1","H1"),iv %in% c("w_htn","h_htn")) %>% 
  mutate(outcome = "Hypertension",
         label = "Overall",
         survey = "England")

charls_main <- read_csv("charls/g2ac03_poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("W1","H1"),iv %in% c("w_htn","h_htn")) %>% 
  mutate(outcome = "Hypertension",
         label = "Overall",
         survey = "China")

lasi_main <- read_csv("lasi/g2al03_poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(model %in% c("W1","H1"),iv %in% c("w_htn","h_htn")) %>% 
  mutate(outcome = "Hypertension",
         label = "Overall",
         survey = "India")






figA_df <- bind_rows(hrs_main,
                     elsa_main,
                     charls_main,
                     lasi_main) %>% 
  mutate(sex_self = case_when(model == "W1" ~ "Wife",
                              model == "H1" ~ "Husband"),
         est = exp(theta_D)) %>% 
  mutate(survey = factor(survey,levels=c("USA","England","China","India")))



# Figure B -------------




hrs_couplediff <- read_csv("hrs/g2ah04_poisson regression for gendered differences.csv") %>% 
  dplyr::filter(model %in% c("G2"),iv %in% c("partner_htn:husband")) %>% 
  mutate(outcome = "Hypertension",
         label = "Overall",
         survey = "USA")

elsa_couplediff <- read_csv("elsa/g2ae04_poisson regression for gendered differences.csv") %>% 
  dplyr::filter(model %in% c("G2"),iv %in% c("partner_htn:husband")) %>% 
  mutate(outcome = "Hypertension",
         label = "Overall",
         survey = "England")

charls_couplediff <- read_csv("charls/g2ac04_poisson regression for gendered differences.csv") %>% 
  dplyr::filter(model %in% c("G2"),iv %in% c("partner_htn:husband")) %>% 
  mutate(outcome = "Hypertension",
         label = "Overall",
         survey = "China")

lasi_couplediff <- read_csv("lasi/g2al04_poisson regression for gendered differences.csv") %>% 
  dplyr::filter(model %in% c("G2"),iv %in% c("partner_htn:husband")) %>% 
  mutate(outcome = "Hypertension",
         label = "Overall",
         survey = "India")

figB_df <- bind_rows(hrs_couplediff,
                     elsa_couplediff,
                     charls_couplediff,
                     lasi_couplediff) %>% 
  mutate(sex_self = "Husband:Wife",
         est = exp(theta_D)) %>% 
  mutate(survey = factor(survey,levels=c("USA","England","China","India")))


# Figure C -----------

figC_df <- read_csv("pooled/g2ap01_contrasts for poisson regression with multiple imputation.csv") %>%
  dplyr::filter(term %in% c("Interaction of h_htn:countryELSA",
                            "Interaction of h_htn:countryCHARLS",
                            "Interaction of h_htn:countryLASI")) %>%
  mutate(survey = c("England","China","India")) %>%
  bind_rows(data.frame(survey = "USA",
                       theta_D = 0)) %>% 
  mutate(sex_self = "Wife",
         est = exp(theta_D)) %>%
  mutate(survey = factor(survey,levels=c("USA","England","China","India")))

# Figure D ---------
figD_df <- read_csv("pooled/g2ap01_contrasts for poisson regression with multiple imputation.csv") %>%
  dplyr::filter(term %in% c("Interaction of w_htn:countryELSA",
                            "Interaction of w_htn:countryCHARLS",
                            "Interaction of w_htn:countryLASI")) %>%
  mutate(survey = c("England","China","India")) %>%
  bind_rows(data.frame(survey = "USA",
                       theta_D = 0)) %>% 
  mutate(sex_self = "Husband",
         est = exp(theta_D)) %>%
  mutate(survey = factor(survey,levels=c("USA","England","China","India")))




figA <- figA_df %>% 
  ggplot(data=.,
         aes(x=est,y=survey,
             xmin=lci,xmax =uci,
             color=sex_self)) +
  geom_point(position=position_dodge(width=0.9)) +
  geom_errorbar(position=position_dodge(width=0.9),width=0.1) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits=c(0.75,1.5),breaks=seq(0.75,1.5,by=0.25)) +
  theme_bw() +
  xlab("Prevalence Ratio (95% CI)") +
  ylab("") +
  geom_vline(xintercept=1.0,color="black",linetype=2) +
  scale_color_manual(name = "Prevalence Ratio in ",values=c("Husband"="darkblue","Wife"="red","Husband:Wife"="black")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)) +
  theme(legend.position="left")

figB <- figB_df %>% 
  ggplot(data=.,
         aes(x=est,y=survey,
             xmin=lci,xmax =uci,
             color=sex_self)) +
  geom_point(position=position_dodge(width=0.9)) +
  geom_errorbar(position=position_dodge(width=0.9),width=0.1) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits=c(0.75,1.5),breaks=seq(0.75,1.5,by=0.25)) +
  theme_bw() +
  xlab("Prevalence Ratio (95% CI)") +
  ylab("") +
  geom_vline(xintercept=1.0,color="black",linetype=2) +
  scale_color_manual(name = "Prevalence Ratio in ",values=c("Husband"="darkblue","Wife"="red","Husband:Wife"="black")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)) +
  theme(legend.position="left")

figC <- figC_df %>% 
  ggplot(data=.,
         aes(x=est,y=survey,
             xmin=lci,xmax =uci,
             color=sex_self)) +
  geom_point(position=position_dodge(width=0.9)) +
  geom_errorbar(position=position_dodge(width=0.9),width=0.1) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits=c(0.75,1.5),breaks=seq(0.75,1.5,by=0.25)) +
  theme_bw() +
  xlab("Prevalence Ratio (95% CI)") +
  ylab("") +
  geom_vline(xintercept=1.0,color="black",linetype=2) +
  scale_color_manual(name = "Prevalence Ratio in ",values=c("Husband"="darkblue","Wife"="red","Husband:Wife"="black")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)) +
  theme(legend.position="left")


figD <- figD_df %>% 
  ggplot(data=.,
         aes(x=est,y=survey,
             xmin=lci,xmax =uci,
             color=sex_self)) +
  geom_point(position=position_dodge(width=0.9)) +
  geom_errorbar(position=position_dodge(width=0.9),width=0.1) + 
  scale_y_discrete(limits = rev) +
  scale_x_continuous(limits=c(0.75,1.5),breaks=seq(0.75,1.5,by=0.25)) +
  theme_bw() +
  xlab("Prevalence Ratio (95% CI)") +
  ylab("") +
  geom_vline(xintercept=1.0,color="black",linetype=2) +
  scale_color_manual(name = "Prevalence Ratio in ",values=c("Husband"="darkblue","Wife"="red","Husband:Wife"="black")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)) +
  theme(legend.position="left")


require(ggpubr)
ggarrange(figA,figB,
          labels=LETTERS[1:2],
          common.legend = TRUE,
          legend = "bottom") %>% 
  ggsave(.,filename=paste0(path_g2a_family_folder,"/figures/figure_pooled and gender differences interaction.png"),
         width=8,height=3)



ggarrange(figC,figD,
          labels=LETTERS[1:2],
          common.legend = TRUE,
          legend = "bottom") %>% 
  ggsave(.,filename=paste0(path_g2a_family_folder,"/figures/figure_pooled country differences interaction.png"),
         width=8,height=3)
