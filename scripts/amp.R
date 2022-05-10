amp = 
  naut_cmp %>%
  merge(.,
        (land %>% select(lota, EGRUPART, ANO, MES, land_kg)),
        by.x = c('PORTO_NOME', 'GEAR','ANO', 'MES'),
        by.y = c('lota', 'EGRUPART', 'ANO', 'MES'),
        all.x = T, all.y = F) %>%
  mutate(n_amplia = 
           land_kg.x / peso_amostrado_dom *
           peso_total_caixa / peso_am_caixa *
           peso_total_spp / peso_am_spp * 
           n_nao_observados,
         n_final = n_amplia * land_kg.y/land_kg.x) %>%
  group_by(REGIAO, PORTO_NOME, ANO, MES, GEAR, classe_comp) %>%
  summarise(freq = sum(n_final, na.rm = T),
            desemb = unique(land_kg.y))

amp_land = 
amp %>% select(ANO, MES, REGIAO, GEAR, PORTO_NOME, desemb) %>% 
  unique %>%
  mutate(zona = case_when(REGIAO == 'S' ~ 'S',
                          T ~'W')) %>% 
  group_by(ANO, MES, zona, GEAR) %>% 
  summarise(desemb = sum(desemb, na.rm = T))


amp_cmp = amp %>% 
  mutate(zona = case_when(REGIAO == 'S' ~ 'S',
                          T ~'W')) %>% 
  group_by(ANO, MES, zona, GEAR, classe_comp) %>% 
  summarise(freq = sum(freq, na.rm = T)) %>% 
  left_join(amp_land, by = c('ANO', 'MES', 'zona', 'GEAR'))

