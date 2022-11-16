
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
  dplyr::filter(pWnHp > 0, pWpHn > 0) 

figW <- ggplot(data=PR_df,aes(x = pW,y = pH,fill = PR_wife)) +
  geom_tile() +
  scale_fill_gradient2("Prevalence Ratio",low="darkgreen",mid="yellow",high="red",midpoint = 1,limits=c(0,100)) +
  theme_bw() +
  xlab("Wife's marginal prevalence (%)") +
  ylab("Husband's marginal prevalence (%)")
  
figH <- ggplot(data=PR_df,aes(x = pW,y = pH,fill = PR_husband)) +
  geom_tile() +
  scale_fill_gradient2("Prevalence Ratio",low="darkgreen",mid="yellow",high="red",midpoint = 1,limits=c(0,100)) +
  theme_bw() +
  xlab("Wife's marginal prevalence (%)") +
  ylab("Husband's marginal prevalence (%)")


require(ggpubr)
ggarrange(figW,figH,nrow=1,ncol=2,
          labels = c("Wife","Husband"),
          common.legend = TRUE,legend="bottom",
          widths = c(2.2,1.5)) %>% 
  ggsave(.,filename = paste0(path_g2a_family_folder,"/figures/figure_statistical challenge.png"),width=15,height=8)
