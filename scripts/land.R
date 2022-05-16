#importa e processa os landings:

land = read.csv(paste0(dados,'desembarques//ceph_gux.txt'),
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
            REGIAO =case_when(REGIAO == "S" ~ "27.9.a.s.a",
                              REGIAO == "C" ~ "27.9.a.c.s",
                              REGIAO == "N" ~ "27.9.a.c.n",
                                T ~ 'vaipaputaquetepariuilheudocaralho'),
            PORTO_PNAB = factor(PORTO_PNAB),
            PORTO_SLV = factor(PORTO_SLV),
            PORTO_NOME = factor(PORTO_NOME),
            ARTE_EU = factor(ARTE_EU),
            EGRUPART = factor(case_when(ARTE_EU %in% c('PSEINERS', 'SP_PSEINERS') ~ "PS",
                                       ARTE_EU %in% c('DTRAWL', 'SP_DTRAWL') ~ "OTB",
                                       T ~ "MIS")),
            COD_FAO = factor('OCC'),
            land_kg = DESEMBARQUE) %>% 
  filter(REGIAO != 'vaipaputaquetepariuilheudocaralho')


            
land %>% group_by(PORTO_NOME, EGRUPART,
                  COD_FAO, ANO, MES, REGIAO) %>%
  summarise(land_kg = sum(land_kg, na.rm = T))

land = land %>% left_join(portos_slv[,c('codporto', 'lota')],
                          by = c('PORTO_SLV' = 'codporto'))
