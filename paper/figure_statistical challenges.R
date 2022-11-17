
pW = seq(5,100,by=5)
pH = seq(5,100,by=5)

prop_WpHp = seq(0,1,by=0.05)

PR_df <- expand.grid(pW,pH,prop_WpHp) %>% 
  data.frame() %>%
  rename(pW = Var1,
         pH = Var2,
         prop_WpHp = Var3) %>% 
  dplyr::mutate(pWpHp = case_when((pW*prop_WpHp <= pH) & (pH*prop_WpHp <= pW) ~ pW*prop_WpHp,
                           TRUE ~ NA_real_)
         ) %>% 
  dplyr::filter(!is.na(pWpHp)) %>% 
  mutate(pWpHn = pW - pWpHp,
         pWnHp = pH - pWpHp) %>% 
  mutate(PR_wife = (pWpHp/pH)/(pWpHn/(100-pH)),
         PR_husband = (pWpHp/pW)/(pWnHp/(100-pW)),
         ) %>% 
  dplyr::filter(pWnHp > 0, pWpHn > 0) %>% 
  mutate(PR_category_wife = case_when(PR_wife < 1 ~ 1,
                                      PR_wife >= 1 & PR_wife <= 1.1 ~ 2,
                                      TRUE ~ 3),
         PR_category_husband = case_when(PR_husband < 1 ~ 1,
                                      PR_husband >= 1 & PR_husband <= 1.1 ~ 2,
                                      TRUE ~ 3)
         ) %>% 
  mutate_at(vars(PR_category_wife,PR_category_husband),function(x) factor(x,levels=c(1:3),labels=c("<1","1.00-1.10",">1.10")))

figW <- ggplot(data=PR_df,aes(x = pW,y = pH,fill = PR_category_wife)) +
  geom_tile() +
  scale_fill_manual("Prevalence Ratio",values = c("darkgreen","yellow","red")) +
  theme_bw() +
  xlab("Wife's marginal prevalence (%)") +
  ylab("Husband's marginal prevalence (%)")
  
figH <- ggplot(data=PR_df,aes(x = pW,y = pH,fill = PR_category_husband)) +
  geom_tile() +
  scale_fill_manual("Prevalence Ratio",values = c("darkgreen","yellow","red")) +
  theme_bw() +
  xlab("Wife's marginal prevalence (%)") +
  ylab("Husband's marginal prevalence (%)")


require(ggpubr)
ggarrange(figW,figH,nrow=1,ncol=2,
          labels = c("Wife","Husband"),
          common.legend = TRUE,legend="bottom") %>% 
  ggsave(.,filename = paste0(path_g2a_family_folder,"/figures/figure_statistical challenge.png"),width=15,height=8)
