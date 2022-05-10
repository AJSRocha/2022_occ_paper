# lotas guardadas no nautilus em que dados foram registados com comprimentos

OTB =
  unique(df_lota$arte_eu)[grepl('OTB',unique(df_lota$arte_eu)) |
                                           grepl('TRAWL',unique(df_lota$arte_eu))]
PS =
  unique(df_lota$arte_eu)[grepl('PS_',unique(df_lota$arte_eu)) ]


lota_naut_1 =
df_lota %>% 
    # filter(estrategia_amostragem == "Concurrent Sampling") %>%
    # filter(Sampling_type != 'OnBoard') %>% 
    transmute(id_viagem = factor(id_viagem),
              id_denominacao = factor(id_denominacao),
              id_caixa = factor(id_caixa),
              id_spp = factor(id_spp),
              REGIAO = factor( 
                case_when(zona == "Sul" ~ "27.9.a.s.a",
                          zona == "SW" ~ "27.9.a.c.s",
                          zona == "NW" ~ "27.9.a.c.n")),
              DATA = as.POSIXct(data_venda, format = '%Y-%m-%d'),
              ANO = factor(ano),
              MES = gsub('(?<=\\b|-)0',
                         '',
                         format(DATA, format = '%m'),
                         perl=TRUE),
              codporto = factor(codporto),
              PORTO = factor(lota),
              GEAR = factor(
                case_when(arte_eu %in% OTB ~ 'OTB',
                          arte_eu %in% PS ~ 'PS',
                          TRUE ~ 'MIS')),
              
              cat_com = factor(cat_com),
              especie_am = factor("OCC"),
              land_kg = peso_total_dom,
              # cat_com = factor(cat_com),
              peso_amostrado_dom = peso_amostrado_dom,
              peso_total_caixa = peso_total_caixa,
              peso_am_caixa = peso_am_caixa,
              peso_total_spp = peso_total_spp,
              peso_am_spp = peso_am_spp,
              n_total_caixa = n_total_caixa,
              n_amostrados_caixa = n_amostrados_caixa,
              n_total_spp = n_total_spp,
              n_amostrado_comprimentos = n_amostrado_comprimentos,
              n_nao_observados = n_nao_observados,
              n_nao_observados_tot = n_nao_observados_tot,
              classe_comp = classe_comp)

lota_naut_1 =
  merge(lota_naut_1, portos_slv[,c("codporto","nome")],
        by.x = 'codporto',
        by.y = 'codporto',
        all.x = T,
        all.y = F)


