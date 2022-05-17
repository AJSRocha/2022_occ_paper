#' Query usada
#' 
# select p1.id_viagem,p1.id_amostra,to_char(p1.data,'DD-MM-YYYY') data_am,p2.porto_nome,p2.regiao,
# p1.arte_eu,p1.cod_fao,p1.matricula,p1.navio,p1.desembarque,p1.peso_am,
# p3.cat,p3.peso_a_c,p3.peso_d_c,p3.n_ind,p4.c_classe, coalesce(p4.indif,0) + coalesce(p4.MACHOS, 0) + coalesce(p4.FEMEAS, 0) indif
# from dbm.tbl_amostra p1, dbm.tbl_porto_old p2, dbm.tbl_cat_amostra p3, dbm.tbl_classe_comp p4
# where p2.porto_slv=p1.porto_slv and p3.id_amostra=p1.id_amostra and p4.id_cat_amostra=p3.id_cat_amostra
# and to_char(p1.data,'YYYY') between 2009 and 2016
# and p1.tipo_amostra='C'
# and p1.cod_fao in ('OCT','OCC')
# order by data_am,porto_nome,matricula,cod_fao,cat,c_classe

# 
# lota16 = read.csv(paste0(dados,
#                          'sic_naut/allspp_19972016_cmp.csv'), sep = ";", dec = ",")


lota16 = read.csv(paste0(dados,
                         'sic_naut/occ_20092016_cmp.csv'), sep = ",", dec = ".") %>% 
  mutate(ANO = substr(DATA_AM, 7,10))

OTB =
  unique(lota16$ARTE_EU)[grepl('OTB',unique(lota16$ARTE_EU)) |
                           grepl('TRAWL',unique(lota16$ARTE_EU))]
PS =
  unique(lota16$ARTE_EU)[grepl('PS_',unique(lota16$ARTE_EU)) ]

# polvos medidos até à ponta do tentaculo (??!!)
viag_sines = c(106841,106857,106876,106931,106897,106930,106915,106835,106921,107703,115753,114784,115756)



lota16 =
  lota16 %>% 
  filter(COD_FAO == 'OCC') %>%
  filter(C_CLASSE < 200) %>% 
  filter(!ID_VIAGEM %in% viag_sines) %>% 
  transmute(ID_VIAGEM = factor(ID_VIAGEM),
            ID_AMOSTRA = factor(ID_AMOSTRA),
            #DATA = as.POSIXct(DATA, format = '%y.%m.%d'), #valido para ficheiro alspp,
            DATA = as.POSIXct(DATA_AM, format = '%d-%m-%Y'),
            
            MES =  gsub('(?<=\\b|-)0',
                       '',
                       format(DATA, format = '%m'),
                       perl=TRUE),
            ANO = factor(ANO),
            PORTO_NOME = factor(case_when(PORTO_NOME == "VILA REAL S. ANTONIO" ~ "VRSA",
                                          TRUE ~ PORTO_NOME)),
            REGIAO = case_when(REGIAO == "N" ~ "27.9.a.c.n",
                               REGIAO == "C" ~ "27.9.a.c.s",
                               TRUE ~ "27.9.a.s.a"),
            ARTE_EU = factor(ARTE_EU),
            GEAR = factor(case_when(ARTE_EU %in% OTB ~ "OTB",
                                    ARTE_EU %in% PS ~ "PS",
                                    TRUE ~ "MIS")),
            COD_FAO = factor('OCC'),
            land_kg = DESEMBARQUE,
            PESO_AM = PESO_AM,
            cat = factor(
              case_when(
                CAT %in% c("0", "T0", "~0", "T0.", "TO", "T", "TUD", "MIS") ~ "T0",
                CAT %in% c("1", "T1", "T1*", "T1+") ~ "T1",
                CAT %in% c("T2", "2", "T2*") ~ "T2",
                CAT %in% c("3", "T3"," T3","T3R") ~ "T3",
                CAT %in% c("T4", "4") ~ "T4",
                CAT %in% c("T5", "5", "T54") ~ "T5",
                CAT %in% c("T6", "6") ~ "T6",
                CAT %in% c("P", "P-", "P.") ~ "P",
                CAT %in% c("MP") ~ "MP",
                CAT %in% c("M") ~ "M",
                CAT %in% c("GM") ~ "GM",
                CAT %in% c("G", "G+") ~ "G",
                CAT %in% c("999") ~ "tatudouiui")),
            PESO_A_C = PESO_A_C,
            PESO_D_C = PESO_D_C,
            N_IND = N_IND,
            C_CLASSE = C_CLASSE,
            INDIF = INDIF 
  ) %>% 
  mutate(PORTO_NOME = case_when(PORTO_NOME == 'LISBOA' ~ 'Lisboa',
                                PORTO_NOME == 'MATOSINHOS' ~ 'Matosinhos',
                                PORTO_NOME == 'PORTIMAO' ~ 'Portimão',
                                PORTO_NOME == 'LAGOS' ~ 'Lagos',
                                PORTO_NOME == 'PENICHE' ~ 'Peniche',
                                PORTO_NOME == 'AVEIRO' ~ 'Aveiro',
                                PORTO_NOME == 'FIGUEIRA DA FOZ' ~ 'Figueira da Foz',
                                PORTO_NOME == 'POVOA DO VARZIM' ~ 'Póvoa de Varzim',
                                PORTO_NOME == 'ARMACAO DE PERA' ~ 'Armação de Pêra',
                                PORTO_NOME == 'OLHAO' ~ 'Olhão',
                                PORTO_NOME == 'SINES' ~ 'Sines',
                                PORTO_NOME == 'SETUBAL' ~ 'Setúbal',
                                PORTO_NOME == 'SESIMBRA' ~ 'Sesimbra',
                                PORTO_NOME == 'VRSA' ~ 'Vila Real de Santo António',
                                PORTO_NOME == 'FUZETA' ~ 'Fuzeta',
                                PORTO_NOME == 'SAGRES' ~ 'Sagres',
                                PORTO_NOME == 'COSTA DA CAPARICA' ~ 'Costa da Caparica',
                                PORTO_NOME == 'QUARTEIRA' ~ 'Quarteira',
                                PORTO_NOME == 'VIANA DO CASTELO' ~ 'Viana do Castelo',
                                PORTO_NOME == 'NAZARE' ~ 'Nazaré',
                                PORTO_NOME == 'SANTA LUZIA' ~ 'Santa Luzia'))


# Correccoes:
# lota16[lota16$ID_VIAGEM == '90161' &
#          lota16$cat  == 'T3' &
#          lota16$C_CLASSE == 35,]$C_CLASSE = 15

lota16 = lota16 %>% 
  filter(ID_VIAGEM != '108797')

# Esta venda é de PIL, não é de OCC
lota16 = lota16 %>% 
  filter(ID_VIAGEM != '110458')



# viagens filhas da puta em sines, medidas ate ao tentaculo:
# lota16 %>% filter(PORTO_NOME == "SINES" & ANO == 2009) %>%
#   select(ID_VIAGEM,C_CLASSE) %>%
#   group_by(ID_VIAGEM) %>%
#   summarise(ref = min(C_CLASSE))


# lota16 %>%
#   filter(!C_CLASSE %in% c(998,999)) %>%
#   filter(!ID_VIAGEM %in% viag_sines) %>% 
#   filter(C_CLASSE < 50) %>%
#   ggplot + 
#   geom_point(aes(y = PESO_AM, x = C_CLASSE, col = PORTO_NOME)) + 
#   theme_light() + 
#   # geom_hline(aes(yintercept = 40), col = 'red') + 
#   facet_wrap(REGIAO ~ ., scales = "free") + 
#   theme(legend.position = 'bottom')
