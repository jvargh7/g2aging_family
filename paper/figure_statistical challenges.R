# The figure attempts to prove 2 points
# 1. As marginal prevalence increases, minimum joint prevalence for large magnitude associations increases?
# 2. For similar marginal prevalences, PR is symmetric for wives and husbands --> This doesn't need proving as such

pW = seq(0.05,1,by=0.05)
pH = seq(0.05,1,by=0.05)

PR = seq(1,1.5,by=0.1)

# PR_{wife} = pWpHp/pH / pWpHn/(1-pH)
# prob_WpHp = pWpHp/pH

prob_WpHp = seq(0,1,by=0.05)

PR_wife = expand.grid(pW,pH,prob_WpHp) %>% 
  data.frame() %>% 
  rename(pW = Var1,
         pH = Var2,
         prob_WpHp = Var3) %>% 
  mutate(pWpHp = pmin(pW,pH*prob_WpHp),
         eWpHp = pH*pW,
         pWpHn = pW - pWpHp,
         p_minus_e = pWpHp - eWpHp) %>%
  mutate(PR = round((pWpHp/pH)/(pWpHn/(1-pH)),1)) %>% 
  dplyr::filter(p_minus_e > 0,PR < 2)


# For a given pH, what should be the value of pW for PR = 1, 1.1, 1.2,...,1.5
require(plotly)
# https://plotly.com/r/3d-mesh/
fig <- PR_wife %>% 
  mutate_at(vars(pW,pH,pWpHp,p_minus_e),~.*100) %>% 
  plot_ly(data=.,x = ~pW,y=~pH,z=~p_minus_e,type="mesh3d",
               intensity = ~PR,
               colorscale = list(c(0,'grey'),
                                 c(0.25,'lightgreen'),
                                 c(0.50,'yellow'),
                                 c(0.75, 'orange'),
                                 c(1, 'red'))
) %>%
  layout(scene = list(xaxis = list(title = "Wife's prevalence (%)"), 
                      yaxis = list(title = "Husband's prevalence (%)"),
                      zaxis = list(title = "Joint - Expected Prevalence: Couple (%)")
                      
  )) %>% 
  layout(legend=list(title=list(text='<b> Trend </b>')))

fig




