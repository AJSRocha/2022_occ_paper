#outputs por ano, zona, arte e T

output_cmp = 
naut_cmp %>% 
  filter(cat_com %in% c('T1', 'T2', 'T3', 'T4')) %>% 
  group_by(ANO, REGIAO, GEAR, cat_com, classe_comp) %>% 
  summarise(n = sum(n_nao_observados),
            n_sampled = n) %>% 
  group_by(ANO, REGIAO, GEAR, cat_com) %>% 
  filter(n == max(n, na.rm =T)) %>% 
  group_by(ANO, REGIAO, GEAR, cat_com, n) %>%
  summarise(classe_modal = paste(classe_comp, collapse = ','))

range_cmp = 
naut_cmp %>% 
  filter(cat_com %in% c('T1', 'T2', 'T3', 'T4')) %>% 
  group_by(ANO, REGIAO, GEAR, cat_com) %>% 
  summarise(min = min(classe_comp),
            max = max(classe_comp),
            n_sampled_cat = sum(n_nao_observados)) 

output_cmp = output_cmp %>% 
  left_join(range_cmp) %>% 
  select(-n)

output_peso = 
  naut_peso %>% 
  filter(cat_com %in% c('T1', 'T2', 'T3', 'T4')) %>% 
  group_by(ANO, REGIAO, GEAR, cat_com, classe_peso) %>% 
  summarise(n = sum(n_nao_observados)) %>%
  group_by(ANO, REGIAO, GEAR, cat_com) %>% 
  filter(n == max(n, na.rm =T)) %>% 
  group_by(ANO, REGIAO, GEAR, cat_com, n) %>%
  summarise(classe_modal = paste(classe_peso, collapse = ','))

range_peso = 
  naut_peso %>% 
  filter(cat_com %in% c('T1', 'T2', 'T3', 'T4')) %>% 
  group_by(ANO, REGIAO, GEAR, cat_com) %>% 
  summarise(min = min(classe_peso),
            max = max(classe_peso),
            n_sampled_cat = sum(n_nao_observados))

output_peso = output_peso %>% 
  left_join(range_peso) %>% 
  select(-n)

write.csv(output_cmp, file = 'outputs/classes_cmp.csv')
write.csv(output_peso, file = 'outputs/classes_peso.csv')


#outputs por ano, zona, arte e T

output_cmp_sem_t =
  naut_cmp %>% 
  # filter(cat_com %in% c('T1', 'T2', 'T3', 'T4')) %>% 
  group_by(ANO, REGIAO, GEAR, classe_comp) %>% 
  summarise(n = sum(n_nao_observados)) %>%
  group_by(ANO, REGIAO, GEAR) %>% 
  filter(n == max(n, na.rm =T)) %>% View
  group_by(ANO, REGIAO, GEAR, n) %>%
  summarise(classe_modal = paste(classe_comp, collapse = ','))


range_cmp_sem_t = 
  naut_cmp %>% 
  # filter(cat_com %in% c('T1', 'T2', 'T3', 'T4')) %>% 
  group_by(ANO, REGIAO, GEAR) %>% 
  summarise(min = min(classe_comp),
            max = max(classe_comp),
            n_sampled = sum(n_nao_observados))

output_cmp_sem_t = output_cmp_sem_t %>% 
  left_join(range_cmp_sem_t) %>% 
  select(-n)

## plot auxiliar
naut_cmp %>% 
  # filter(cat_com %in% c('T1', 'T2', 'T3', 'T4')) %>% 
  filter(GEAR != 'PS') %>% 
  group_by(ANO, REGIAO, GEAR, classe_comp) %>% 
  summarise(n = sum(n_nao_observados)) %>% 
  ggplot +
  geom_bar(aes(
    x = classe_comp,
    y = n,
    fill = GEAR),
    stat = 'identity') + 
  facet_grid(ANO ~ REGIAO) +
  theme_light() + 
  labs(title = 'Classes de comprimento modais OCC') + 
  theme(legend.position = 'bottom')


output_peso_sem_t = 
  naut_peso %>% 
  # filter(cat_com %in% c('T1', 'T2', 'T3', 'T4')) %>% 
  group_by(ANO, REGIAO, GEAR, classe_peso) %>% 
  summarise(n = sum(n_nao_observados)) %>%
  group_by(ANO, REGIAO, GEAR) %>% 
  filter(n == max(n, na.rm =T)) %>% 
  group_by(ANO, REGIAO, GEAR, n) %>%
  summarise(classe_modal = paste(classe_peso, collapse = ','))

range_peso_sem_t = 
  naut_peso %>% 
  # filter(cat_com %in% c('T1', 'T2', 'T3', 'T4')) %>% 
  group_by(ANO, REGIAO, GEAR) %>% 
  summarise(min = min(classe_peso),
            max = max(classe_peso),
            n_sampled = sum(n_nao_observados))

output_peso_sem_t = output_peso_sem_t %>% 
  left_join(range_peso_sem_t) %>% 
  select(-n)

## plot auxiliar
naut_peso %>% 
  # filter(cat_com %in% c('T1', 'T2', 'T3', 'T4')) %>% 
  filter(GEAR != 'PS') %>% 
  group_by(ANO, REGIAO, GEAR, classe_peso) %>% 
  summarise(n = sum(n_nao_observados, na.rm = T)) %>% 
  ggplot +
  geom_bar(aes(
    x = classe_peso,
    y = n,
    fill = GEAR),
    stat = 'identity') + 
  facet_grid(ANO ~ REGIAO) +
  theme_light() + 
  labs(title = 'Classes de peso modais OCC') + 
  theme(legend.position = 'bottom')

write.csv(output_cmp_sem_t, file = 'outputs/classes_cmp_sem_t.csv')
write.csv(output_peso_sem_t, file = 'outputs/classes_peso_sem_t.csv')
