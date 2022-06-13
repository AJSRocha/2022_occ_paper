library(dplyr)
library(ggplot2)
library(tidyr)
library(CatDyn)

#TODO -Confirmar se nao devemos passar captura a numero de individuos;
#' ver parametro nmult

for(i in c(2009:2021)){
  load(paste0('Z:/PNAB/vendas-dia/vd_', i,'.Rdata'))
}

vd = rbind(vd_2009,vd_2010,vd_2011,vd_2012,vd_2013,vd_2014,vd_2015,vd_2016,vd_2017,vd_2018,vd_2019,
           vd_2020,vd_2021)


viagens_occ = vd %>% 
  filter(EESPECIE == 'OCC') %>% 
  select(id_venda)

passo_1 = vd %>%
  mutate(zona = case_when(zona == '27.9.a.s.a' ~ 'S',
                            T ~'W'),
         EGRUPART = case_when(EGRUPART == 'MIS_MIS' ~ 'MIS',
                              T ~ 'OTB')) %>%
  
  filter(EGRUPART != 'PS') %>% 
  filter(id_venda %in% viagens_occ$id_venda) %>% 
  group_by(year_sale, month_sale, EGRUPART, zona) %>% 
    summarise(effort = length(unique(id_venda)),
              catch = sum(QVENDA[EESPECIE == 'OCC']))


passo_2 = 
amp_peso %>%
  mutate(MES = case_when(!MES %in% c('10', '11', '12') ~ paste0(0, MES),
                         T ~ MES)) %>% 
  group_by(ANO, MES, zona, GEAR) %>% 
  summarise(peso_medio = weighted.mean(classe_peso, n_ampliados))

passo_3 = passo_1 %>% left_join(passo_2, by = c('zona', 'EGRUPART' = 'GEAR', 'year_sale' = 'ANO',
                                                'month_sale' = 'MES'))


lgahi <- as.CatDynData(x=lolgahi,
                       step="day",
                       fleet.name="Fk",
                       coleff=2,
                       colcat=1,
                       colmbw=3,
                       unitseff="nboats",
                       unitscat="kg",
                       unitsmbw="kg",
                       nmult="bill",
                       season.dates=c(as.Date("1990-01-31"),
                                      as.Date("1990-05-30")))



# Create dataframe
cobaia = 
passo_3 %>% 
  # filter(zona == 'W' & EGRUPART == 'MIS') %>% 
  pivot_wider(id_cols = c('year_sale', 'month_sale'),
                        names_from = c('EGRUPART', 'zona'),
                        values_from = c('catch', 'effort', 'peso_medio'))


# df = as.CatDynData(
#   x = cobaia,
#   step = 'month',
#   fleet.name = c('MIS_W', 'OTB_W'),
#   coleff = c(8, 10),
#   colcat = c(4,6),
#   colmbw = c(12,14),
#   nmult = 'thou',
#   unitseff = 'trip',
#   unitscat = 'kg',
#   unitsmbw = 'g',
#   season.dates=c(as.Date("2009-01-01"),
#                  as.Date("2009-12-31")))

df = as.CatDynData(
  x = cobaia,
  step = 'month',
  fleet.name = c('MIS_W'),
  coleff = c(8),
  colcat = c(4),
  colmbw = c(12),
  nmult = 'thou',
  unitseff = 'trip',
  unitscat = 'kg',
  unitsmbw = 'g',
  season.dates=c(as.Date("2009-01-01"),
                 as.Date("2009-12-31")))



## EXEMPLO DO PANELEIROTE

#2) Fit a 1-fleet 2P model with lognormal observation error and full exact
#likelihood including the dispersion parameter
M         <- 0.083 #1/Time step
N0.ini    <- 3.8 #billions
P1.ini    <- 1.3 #billions
P2.ini    <- 0.5 #billions
k.ini     <- 4.0e-05 #1/n of boats
alpha.ini <- 1.7 #adimensional
beta.ini  <- 0.6 #adimensional
#Note how to get reasonable initial value for dispersion parameter
psi.ini   <- 0.33*sd(log(df$Data$MIS_W$obscat.thou))^2
pars.ini  <- log(c(M,
                   N0.ini,
                   # P1.ini,
                   # P2.ini,
                   k.ini,
                   alpha.ini,
                   beta.ini,
                   psi.ini))
#Dates
P1    <- 2  #Selected by visual inspection of standard plot
P2    <- 6 #Selected by visual inspection of standard plot
dates <- c(head(df$Data$MIS_W$time.step,1),
           # P1,
           # P2,
           tail(df$Data$MIS_W$time.step,1))
df.ln.2P.ini <- catdynexp(x=df,
                             p=0,
                             par=pars.ini,
                             dates=dates,
                             distr=c("normal"))
plot(x=df.ln.2P.ini,
     leg.pos="topright",
     Biom.tstep=7,
     Cat.tstep=120,
     Biom.xpos=0.4,
     Biom.ypos=0,
     Cat.xpos=0.18,
     Cat.ypos=0.2)

#fit lognormal
lgahi.ln.2P.fit <- CatDynFit(x=lgahi,
                             p=2,
                             par=pars.ini,
                             dates=dates,
                             distr="lognormal",
                             method="spg",
                             itnmax=10)
#examine results
lgahi.ln.2P.pred.spg <- CatDynPred(lgahi.ln.2P.fit,"spg")
plot(x=lgahi.ln.2P.pred.spg,
     leg.pos="topright",
     Biom.tstep=7,
     Cat.tstep=120,
     Biom.xpos=0.18,
     Biom.ypos=0.1,
     Cat.xpos=0.18,
     Cat.ypos=0.2)
#
#Summary table for model selection
lgahi.sum <- CatDynSum(x=list(lgahi.apln.1P.fit,
                              lgahi.ln.2P.fit),
                       season=1990,
                       method=c("spg","spg"))
#Plot for correlations among parameter estimates
CatDynCor(x=list(lgahi.apln.1P.fit,
                 lgahi.ln.2P.fit),
          ttl=c("Adjusted Profile Lognormal 1P","Lognormal 2P"),
          method=c("spg","spg"),
          arr=c(2,1))
#Create neat table with optimization results
CatDynPar(x=lgahi.ln.2P.fit,method="spg")
#
  