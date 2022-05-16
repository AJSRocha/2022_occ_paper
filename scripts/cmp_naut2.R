OTB =
  unique(df_lota$arte_eu)[grepl('OTB',unique(df_lota$arte_eu)) |
                            grepl('TRAWL',unique(df_lota$arte_eu))]
PS =
  unique(df_lota$arte_eu)[grepl('PS_',unique(df_lota$arte_eu)) ]

df_bio =
df_bio %>% 
  left_join(meties2[,c('id', 'arte_eu')],
            by = c('id_viagem' = 'id'))

lota_naut_2 =
  df_bio %>%
  #possivel outlier?
  filter(estrategia_amostragem == "Concurrent Sampling") %>%
  filter(Sampling_type != 'OnBoard') %>% 
  filter(peso_total < 10000) %>% 
  filter(ano <= 2021) %>% 
  transmute(id_viagem = factor(id_viagem),
            id_denominacao = factor(id_denominacao),
            id_caixa = factor(id_caixa),
            id_spp = factor(id_spp),
            id_indiv = factor(id_indiv),
            REGIAO = factor( 
              case_when(regiao == "Sul" ~ "27.9.a.s.a",
                        regiao == "SW" ~ "27.9.a.c.s",
                        regiao == "NW" ~ "27.9.a.c.n")),
            DATA = as.POSIXct(data_venda, format = '%Y-%m-%d'),
            MES = gsub('(?<=\\b|-)0',
                       '',
                       format(DATA, format = '%m'),
                       perl=TRUE), 
            trim = factor(trim),
            ANO = factor(ano),
            PORTO = factor(lota),
            codporto = factor(iporto),
            GEAR = factor(
              case_when(arte_eu %in% OTB ~ 'OTB',
                        arte_eu %in% PS ~ 'PS',
                        TRUE ~ 'MIS')),
            cat_com = factor(cat_com),
            especie_am = factor("OCC"),
            land_kg = peso_total_dom,
            peso_amostrado_dom = peso_amostrado_dom,
            peso_total_caixa = peso_total_caixa,
            peso_am_caixa = peso_am_caixa,
            peso_total_spp = peso_total_spp,
            peso_am_spp = peso_am_spp,
            n_total_caixa = n_total_caixa,
            n_amostrados_caixa = n_amostrados_caixa,
            n_total_spp = n_total_spp,
            n_amostrado_comprimentos = n_amostrado_comprimentos,
            n_nao_observados_tot = n_nao_observados_tot,
            comp_manto = comp_manto,
            peso_total = peso_total) %>%
  # correcção aos barcos de peniche para os quais nao foi possivel obter info dos ts e, consequentemente extrapolar à viagem
  mutate(land_kg = case_when(is.na(peso_total_spp) & PORTO == 'Peniche' ~ peso_am_spp,
                             T ~ land_kg),
         peso_total_spp = case_when(is.na(peso_total_spp) & PORTO == 'Peniche' ~ peso_am_spp,
                                    T ~ peso_total_spp))
            
            
lota_naut_2 =
  merge(lota_naut_2, portos_slv[,c("codporto","nome")],
        by.x = 'codporto',
        by.y = 'codporto',
        all.x = T,
        all.y = F)


