# Check LW:

land_2_zonas =
  land_amo %>% 
  mutate(zona = case_when(REGIAO == '27.9.a.s.a' ~ 'S',
                          T ~'W')) %>% 
  group_by(ANO, GEAR, zona) %>% 
  summarise(desemb_kg = sum(desemb_kg, na.rm = T))


amp_peso %>% 
  group_by(ANO, zona, GEAR) %>% 
  summarise(peso_estimado = sum(peso_estimado, na.rm = T)) %>% 
  left_join(land_zonas,
            by = c('ANO', 'GEAR', 'zona')) %>%
  mutate(check = peso_estimado/desemb_kg) %>% View


amp_peso %>% 
  filter(GEAR != 'PS') %>% 
  ggplot() + 
  geom_bar(aes(x = classe_peso, y = n_ampliados, fill = GEAR),
           stat = 'identity', position = 'dodge',
           col = 'black') +
  theme_light() + 
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  ) +
  facet_grid(zona ~ ANO, scales = 'free_y') + 
  scale_fill_manual(values = wes_palette('FantasticFox1', n = 3)) +
  labs(x = 'Year', y = ' landings (tonnes)', fill = "")

# 1 - Desembarques agregados
write.csv(land_2_zonas, file = 'desemb.csv')


# 2 - Classes de comprimento
write.csv(amp_peso, file = 'classes_peso.csv')

# 3 - Classes modais
classe_modal_peso = amp_peso %>% 
  group_by(ANO, GEAR, zona) %>% 
  summarise(classe_modal = classe_peso[which.max(n_ampliados)],
            freq = max(n_ampliados))

recrutas_peso = 
amp_peso %>% 
  left_join(classe_modal_peso, by = c('ANO', 'GEAR', 'zona')) %>% 
  group_by(ANO, GEAR, zona) %>% 
  summarise(recrutas = sum(n_ampliados[classe_peso <= classe_modal], na.rm = T),
            total = sum(n_ampliados, na.rm = T),
            ratio = recrutas/total)

write.csv(recrutas_peso, file = 'recrutamento_peso.csv')



