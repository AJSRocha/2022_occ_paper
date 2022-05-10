#importa e processa os landings:

land = read.csv('C://dados_pnab//desembarques//ceph_gux.txt',
                sep = ",",
                dec = ".",
                header = T)

land = 
  land %>% 
  filter(COD_FAO %in% c("OCC", "OCT")) %>%
  # filter(PESQUEIRO == "P") %>%
  filter(ARTE_EU %in%c('DTRAWL', 'POLYVALENT')) %>% 
  transmute(ANO = factor(ANO),
            MES = factor(MES),
            REGIAO = factor(REGIAO),
            PORTO_PNAB = factor(PORTO_PNAB),
            PORTO_SLV = factor(PORTO_SLV),
            PORTO_NOME = factor(PORTO_NOME),
            ARTE_EU = factor(ARTE_EU),
            EGRUPART = factor(case_when(ARTE_EU %in% c('PSEINERS', 'SP_PSEINERS') ~ "PS",
                                       ARTE_EU %in% c('DTRAWL', 'SP_DTRAWL') ~ "OTB",
                                       T ~ "MIS")),
            COD_FAO = factor('OCC'),
            land_kg = DESEMBARQUE)


            
land %>% group_by(PORTO_NOME, EGRUPART,
                  COD_FAO, ANO, MES, REGIAO) %>%
  summarise(land_kg = sum(land_kg, na.rm = T))

land = land %>% left_join(portos_slv[,c('codporto', 'lota')],
                          by = c('PORTO_SLV' = 'codporto'))
