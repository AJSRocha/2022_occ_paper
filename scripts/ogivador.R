library(sizeMat)

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
                      l50_gonad_= temp2$L50_boot[999])
  
  return(output) 
}


ogivador_cm = function(data){
  
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
                        varNames = c("comp_manto", "mat.a"),
                        inmName =unique(temp2$mat.a)[unique(temp2$mat.a) %in% c('I', 'II')],
                        matName = unique(temp2$mat.a)[unique(temp2$mat.a) %in% c('III', 'IV','V')],
                        method = "fq", niter = 999)
  
  output = data.frame(l50_morph = temp$L50_boot[1000],
                      l50_gonad_= temp2$L50_boot[999])
  
  return(output) 
}




a = Sys.time()
teste = 
bio_tmp %>%
  mutate(regiao = case_when(regiao == '27.9.a.s.a' ~ 'S',
                            T ~'W')) %>% 
  # filter(regiao == 'S' & sexo == 'M' & ano %in% c('2013','2014'))  %>%
  group_by(regiao, sexo, ano) %>% 
  do(L50_morph = ogivador(.)$l50_morph,
     L50_gonad = ogivador(.)$l50_gonad,
     L50_gonad_cm = ogivador_cm(.)$l50_gonad)
b = Sys.time(); b-a

teste = teste %>% 
  mutate(L50_morph = unlist(L50_morph),
         L50_gonad = unlist(L50_gonad),
         L50_gonad_cm = unlist(L50_gonad_cm))

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

