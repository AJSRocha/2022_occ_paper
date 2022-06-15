library(sizeMat)
library(tidyr)
library(pals)
# pal.bands(alphabet, alphabet2, cols25, glasbey, kelly, polychrome, 
#           stepped, tol, watlington,
#           show.names=FALSE)

ogivador = function(data){
  
  # parametros de morfometria
  temp = 
  classify_mature(data,
                  varNames = c('peso', 'comp_manto'),
                  varSex = 'sexo',
                  selectSex = NULL,
                  method = 'ld')
  
  # l50 morfometria
  temp = morph_mature(temp,
                      method = 'bayes',
                      niter = 1000)
  
  # l50 gonadas
  temp2 = data %>% 
    mutate(mat.a = case_when(mat == '1' ~'I',
                             mat == '2' ~'II',
                             mat == '2/3' ~'II',
                             mat == '3' ~'III',
                             mat == '4' ~'IV',
                             mat == '5' ~'V',
                             T ~ as.character(NA))) %>% 
    filter(!is.na(mat.a))
  
  temp2 =  gonad_mature(temp2,
                        varNames = c("peso", "mat.a"),
                        inmName =unique(temp2$mat.a)[unique(temp2$mat.a) %in% c('I', 'II')],
                        matName = unique(temp2$mat.a)[unique(temp2$mat.a) %in% c('III', 'IV','V')],
                        method = "fq", niter = 999)
  
  output = data.frame(l50_morph = temp$L50_boot[1000],
                      l50_gonad= temp2$L50_boot[999],
                      l50_x = temp2$out$x,
                      l50_y = temp2$out$fitted,
                      l50_lower = temp2$out$CIlower,
                      l50_upper = temp2$out$CIupper)
  
  return(output) 
}

temp_2 = bio_tmp  %>% 
  mutate(mat.a = case_when(mat == '1' ~'I',
                           mat == '2' ~'II',
                           mat == '2/3' ~'II',
                           mat == '3' ~'III',
                           mat == '4' ~'IV',
                           mat == '5' ~'V',
                           T ~ as.character(NA))) %>% 
  filter(!is.na(mat.a))



bitcho = 
gonad_mature(temp_2,
             varNames = c("peso", "mat.a"),
             inmName =unique(temp_2$mat.a)[unique(temp_2$mat.a) %in% c('I', 'II')],
             matName = unique(temp_2$mat.a)[unique(temp_2$mat.a) %in% c('III', 'IV','V')],
             method = "fq", niter = 999)

a = Sys.time()
teste = 
bio_tmp %>%
  mutate(regiao = case_when(regiao == '27.9.a.s.a' ~ 'S',
                            T ~'W')) %>%
  # filter para testes
  # filter(regiao == 'S' & sexo == 'M' & ano %in% c('2009','2010'))  %>%
  group_by(regiao, sexo, ano) %>% 
  do(l50_gonad = ogivador(.)$l50_gonad,
     l50_x = ogivador(.)$l50_x,
     l50_y =  ogivador(.)$l50_y,
     l50_lower =  ogivador(.)$l50_lower,
     l50_upper =  ogivador(.)$l50_upper)
b = Sys.time(); b-a

teste_u = teste %>% unnest(., c(l50_gonad, l50_x,
                           l50_y, l50_lower, l50_upper)) 

teste_u %>% 
  ggplot() + 
  geom_line(aes(x = l50_x,
                y = l50_y,
                group = ano,
                col = ano),
            size = 0.5) +
  # geom_ribbon(aes(x = l50_x, ymin = l50_lower, ymax = l50_upper,
  #                 group = ano, fill = ano), alpha = 0.1) + 
  theme_light() + 
  theme(legend.position = 'right') +
  facet_grid(sexo ~ regiao) + 
  scale_color_manual(values = kelly(14)[2:14]) + 
  labs(x = 'weight (g)',
       y = 'Prop. matures')

#'só prosseguir para acrescentar contabilidade do numero de individuos por amostra;
#'necessario actualizar objecto teste



teste = 
teste %>% left_join(
(bio_tmp %>%
  mutate(regiao = case_when(regiao == '27.9.a.s.a' ~ 'S',
                            T ~'W')) %>% 
  group_by(regiao, sexo, ano) %>%
   summarise(n = length(mat),
             n_imat = length(mat[mat%in%c(1,2)]),
             n_mat = length(mat[mat%in%c(3,4,5)]))),
by = c('regiao', 'sexo', 'ano'))

(bio_tmp %>%
    mutate(regiao = case_when(regiao == '27.9.a.s.a' ~ 'S',
                              T ~'W')) %>% 
    group_by(regiao, sexo, ano) %>%
    summarise(n = length(mat),
              n_imat = length(mat[mat%in%c(1,2)]),
              n_mat = length(mat[mat%in%c(3,4,5)])))



names(teste)[6] = 'L50_gonad_mm'

write.csv(teste, file = 'C:/repos/l50.csv')

