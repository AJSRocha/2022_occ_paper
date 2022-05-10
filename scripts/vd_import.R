library(dplyr)
source('C:/repos/path.R')

for(i in c(2011:2021)){
  load(paste0(dados,'vendas-dia/vd_', i, '.RData'))
  assign(paste0('vd_', i),
         mget(ls(pattern = paste0('vd_*', i)))[[1]] %>%
           filter(EESPECIE %in% c('OCC', 'OCT')))
   
}

vd = do.call("rbind", mget(ls(pattern = "^vd_*")))
rm(list = ls(pattern = 'vd_'))

vd_sum = vd  %>% 
  group_by(zona, ETAMANHO) %>% 
  summarise(QVENDA = sum(QVENDA))


