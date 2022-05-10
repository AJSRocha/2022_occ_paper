portos_slv = read.csv(paste0(dados, "portos_slv/codigos_portos.csv"))
portos_slv[nrow(portos_slv)+1,] =  c(40950, "CAIS DO BICO", "csbic", "NW","AVEIRO", "PTCDB", "CDB",NA,NA)

df_lota[,c('lota', 'codporto')] %>% unique
unique(lota16$PORTO_NOME)

portos_slv = portos_slv %>% left_join(df_lota[,c('lota', 'codporto')] %>% 
                                    unique %>%
                                    mutate(codporto = as.character(codporto)), by = c("codporto")) %>% 
  mutate(lota = case_when(nome == 'LISBOA' ~ 'Lisboa',
                          nome == 'FUZETA' ~ 'Fuzeta',
                          nome == 'ARMACAO DE PERA' ~ 'Armação de Pêra',
                          nome == 'QUARTEIRA' ~ 'Quarteira',
                          T ~ lota))

