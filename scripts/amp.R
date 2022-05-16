
# PROPOSTA DE LIMPEZAS:

check_peso_medio =
naut_peso %>% 
  mutate(peso_referencia = case_when(ANO %in% c(2009:2016) ~ peso_am_caixa,
                                     T ~ peso_amostrado_dom)) %>%
  group_by(id_viagem, cat_com) %>% 
  summarise(peso_referencia = mean(peso_referencia, na.rm = T),
            n_cat = sum(n_nao_observados)) %>% 
  group_by(id_viagem) %>% 
  summarise(peso_referencia = sum(peso_referencia, na.rm = T),
            n_viagem = sum(n_cat),
            check = peso_referencia/n_viagem) %>% View
  filter(check > 7.5 | check < 0.3) %>% 
  select(id_viagem) %>% unique


check_classes = 
  naut_cmp %>% 
  filter(classe_comp < 6) %>% 
  select(id_viagem) %>% unique




land_agg = land %>% 
  group_by(ANO, REGIAO, EGRUPART) %>% 
  summarise(desemb_kg = sum(land_kg, na.rm = T))

land_amo = naut_peso %>% 
  # primeiro agregamos a categoria
  group_by(id_viagem, ANO, REGIAO, GEAR, cat_com) %>%
  summarise(amp16 = mean(peso_total_caixa, na.rm = T),
            amp19 = mean(land_kg, na.rm = T)) %>% 
  mutate(peso_desemb_amo = case_when(ANO %in% c(2009:2016) ~ amp16 ,
                                        T ~ amp19)) %>% 
  # agregamos a viagem 
  group_by(id_viagem, ANO, REGIAO, GEAR) %>% 
  summarise(peso_desemb_amo = sum(peso_desemb_amo, na.rm = T)) %>% 
  # agregamos à unidade
  group_by(ANO, REGIAO, GEAR) %>% 
  summarise(peso_desemb_amo = sum(peso_desemb_amo, na.rm = T)) %>% 
  left_join(., land_agg, by = c("ANO" = "ANO", "GEAR" = "EGRUPART", "REGIAO" = "REGIAO")) %>% 
  mutate(check = peso_desemb_amo/desemb_kg) %>%
  filter(check > 0.0001)
# calcula totais amostrados para ampliar


amp_peso =
  naut_peso %>% 
  filter(GEAR != 'PS') %>% 
  # aplica filtros
  filter(!id_viagem %in% check_peso_medio) %>%
  filter(!id_viagem %in% check_classes) %>% 
  mutate(n_amplia_viagem = case_when(ANO %in% c(2009:2016) ~ n_nao_observados * peso_total_caixa / peso_am_caixa,
                                     T ~ n_nao_observados * peso_total_spp / peso_am_spp * land_kg / peso_amostrado_dom)) %>% 
  # agrega
  group_by(ANO, REGIAO, GEAR, classe_peso) %>%  
  summarise(n_amplia_amostrado = sum(n_amplia_viagem, na.rm = T)) %>%
  left_join(land_amo, by = c('ANO', 'GEAR', 'REGIAO')) %>% 
  mutate(n_ampliados = n_amplia_amostrado * desemb_kg/peso_desemb_amo,
         peso_estimado = n_ampliados * classe_peso / 1000) %>%
  # agrega N e C
  mutate(zona = case_when(REGIAO == '27.9.a.s.a' ~ 'S',
                          T ~'W')) %>%
  group_by(ANO, zona, GEAR, classe_peso) %>%
  summarise(n_ampliados = sum(n_ampliados, na.rm = T),
            peso_estimado = sum(peso_estimado, na.rm = T))


  
           
           
           
           
           
 