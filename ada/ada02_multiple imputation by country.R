
source("ada/ada_variables.R")


hrs <- readRDS(paste0(path_g2a_concordance_folder,"/working/hrs.RDS")) %>% 
  mutate(w_htn_residence = residence*w_htn,
         h_htn_residence = residence*h_htn)
elsa <- readRDS(paste0(path_g2a_concordance_folder,"/working/elsa.RDS"))
charls <- readRDS(paste0(path_g2a_concordance_folder,"/working/charls.RDS")) %>% 
  mutate(w_htn_residence = residence*w_htn,
         h_htn_residence = residence*h_htn)
lasi <- readRDS(paste0(path_g2a_concordance_folder,"/working/lasi.RDS"))

dfs <- list(hrs,elsa,charls,lasi)
labels <- c("hrs","elsa","charls","lasi")
library(mice)
for (d in 1:length(dfs)){
  df = dfs[d][[1]]
  
  mi_null <- mice(df,
                  maxit = 0)
  
  method = mi_null$method
  pred = mi_null$predictorMatrix
  
  pred[names(d) %in% id_selected,] <- 0
  pred[,names(d) %in% id_selected] <- 0
  
  
  # Impute via equation and do not use for imputation , -------
  
  method["w_ge65"] <- "~I((w_age>=65)*1)"
  method["h_ge65"] <- "~I((h_age>=65)*1)"
  pred[c("w_ge65","h_ge65"),] <- 0
  pred[,c("w_ge65","h_ge65")] <- 0
  
  # https://stackoverflow.com/questions/33865161/model-multiple-imputation-with-interaction-terms
  # https://thestatsgeek.com/2014/05/10/multiple-imputation-with-interactions-and-non-linear-terms/
  
  for(i_t in interaction_terms){
    print(i_t)
    if(i_t %in% names(d)){
      htn_term = str_extract(i_t,"^(w|h)_htn")
      em_term = str_replace(i_t,pattern=paste0(htn_term,"_"),replacement = "")
      method[i_t] = paste0("~I(",htn_term,"*",em_term,")")
      
      # Do not use interaction terms for imputation of the source variables
      pred[c(htn_term,em_term),i_t] <- 0
    }
    
  }
  
  
  mi_dfs <- mice(df,
                 method = method,
                 pred = pred,
                 m=10,maxit=50,seed=500)
  
  saveRDS(mi_dfs,paste0(path_g2a_concordance_folder,"/working/mi_",labels[d],".RDS"))
  rm(mi_dfs)
}
