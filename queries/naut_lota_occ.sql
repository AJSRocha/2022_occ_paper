select distinct p1.id id_viagem,
                p2.id id_venda,
                p3.id id_denominacao,
                p6.id id_caixa,
                p7.id id_spp,
                p9.id id_comp,
                case when rpo.desig = 'Norte' then 'NW' 
                when rpo.desig = 'Centro' then 'SW' else rpo.desig end zona,
                p1.data_fin,
                p2.data data_venda,
                emb.nome nome_navio,
                emb.matricula,
                emb.cfr,
                por.nome lota,
                por.codigo_slv codporto,
                case when p1.id in 
        (select x.viagem id 
            from 
        (select z.viagem,count(*) contagem 
           from pnab.viagem_metier z 
       group by z.viagem)x 
          where x.contagem <> 1) 
                then 'MIS_MIS_0_0_0' else met.desig end arte_eu,
                p2.peso_vendido,
                p4.desig cat_com,
                p5.desig denominacao,
                p5.codigo_slv esp_slv,
                p5.cod_fao cod_fao_venda,
                cast(substr(p2.data,6,2) as decimal(5,0)) mes,
                cast(substr(p2.data,1,4) as decimal(5,0)) ano,
                p3.peso_total peso_total_dom,
                p3.peso_amostrado peso_amostrado_dom,
                p3.n_caixas,
                p3.n_caixas_amostradas,
                p8.cod_fao,
                p6.peso_total peso_total_caixa,
                p6.peso_amostrado peso_am_caixa,
                p6.n_total n_total_caixa,
                p6.n_amostrados n_amostrados_caixa,
                p7.peso_total peso_total_spp,
                p7.n_total n_total_spp,
                p7.peso_amostrado_comprimentos peso_am_spp,
                p7.n_amostrado_comprimentos,
                p7.n_machos n_machos_tot,
                p7.n_femeas n_femeas_tot,
                p7.n_nao_observados n_nao_observados_tot,
                p7.n_indeterminados n_indeterminados_tot,
                p7.peso_indeterminados_amostr,
                p7.peso_machos_amostr,
                p7.peso_femeas_amostr,
                p7.peso_n_obs_amostr,
                p9.classe_comp,
                p9.n_machos,
                p9.n_femeas,
                p9.n_nao_observados,
                p9.n_indeterminados,
                p9.peso_machos,
                p9.peso_femeas,
                p9.peso_nao_observados,
                p9.peso_indeterminados
           from pnab.viagem p1,
                pnab.venda p2,
                pnab.embarcacao emb,
                pnab.porto por,
                pnab.regiao_porto rpo,
                pnab.viagem_metier v_met,
                pnab.metier met,
                pnab.denominacao p3,
                pnab.cat_comercial p4,
                pnab.denominacao_comercial p5,
                pnab.caixa p6,
                pnab.amostra_especie p7,
                pnab.especie_generica p8,
                pnab.comprimentos p9
          where p2.viagem=p1.id and 
                v_met.viagem=p1.id and 
                v_met.metier=met.id and 
                emb.id=p1.embarcacao and 
                por.id=p2.porto and 
                rpo.id=por.regiao and 
                p2.id=p3.origem and 
                p4.id=p3.cat_comercial and
                p5.id=p3.denominacao_comercial and 
                p7.caixa=p6.id and 
                p7.especie=p8.id and 
                p9.amostra=p7.id and 
                p6.denominacao=p3.id and 
                p3.estrat_amostragem=1 and 
                p1.id not in (select viagem id 
           from pnab.viagem_regiao where regiao <> 5) and
                p9.n_nao_observados is not null and 
                p8.cod_fao in ('OCC','OCT') and
                p2.data between '2017-01-01' and '2021-12-31' 
       UNION ALL /*ver viagens sem metier*/
select distinct p1.id id_viagem,
                p2.id id_venda,
                p3.id id_denominacao,
                p6.id id_caixa,
                p7.id id_spp,
                p9.id id_comp,
      case when rpo.desig = 'Norte' then 'NW' when 
                rpo.desig = 'Centro' then 'SW' else rpo.desig end zona,
                p1.data_fin,
                p2.data data_venda,
                emb.nome nome_navio,
                emb.matricula,
                emb.cfr,
                por.nome lota,
                por.codigo_slv codporto,
                'MIS_MIS_0_0_0' arte_eu,
                p2.peso_vendido,
                p4.desig cat_com,
                p5.desig denominacao,
                p5.codigo_slv esp_slv,
                p5.cod_fao cod_fao_venda,
                cast(substr(p2.data,6,2) as decimal(5,0)) mes,
                cast(substr(p2.data,1,4) as decimal(5,0)) ano,
                p3.peso_total peso_total_dom,
                p3.peso_amostrado peso_amostrado_dom,
                p3.n_caixas,
                p3.n_caixas_amostradas,
                p8.cod_fao,
                p6.peso_total peso_total_caixa,
                p6.peso_amostrado peso_am_caixa,
                p6.n_total n_total_caixa,
                p6.n_amostrados n_amostrados_caixa,
                p7.peso_total peso_total_spp,
                p7.n_total n_total_spp,
                p7.peso_amostrado_comprimentos peso_am_spp,
                p7.n_amostrado_comprimentos,
                p7.n_machos n_machos_tot,
                p7.n_femeas n_femeas_tot,
                p7.n_nao_observados n_nao_observados_tot,
                p7.n_indeterminados n_indeterminados_tot,
                p7.peso_indeterminados_amostr,
                p7.peso_machos_amostr,
                p7.peso_femeas_amostr,
                p7.peso_n_obs_amostr,
                p9.classe_comp,
                p9.n_machos,
                p9.n_femeas,
                p9.n_nao_observados,
                p9.n_indeterminados,
                p9.peso_machos,
                p9.peso_femeas,
                p9.peso_nao_observados,
                p9.peso_indeterminados
           from pnab.viagem p1,
                pnab.venda p2,
                pnab.embarcacao emb,
                pnab.porto por,
                pnab.regiao_porto rpo,
                pnab.denominacao p3,
                pnab.cat_comercial p4,
                pnab.denominacao_comercial p5,
                pnab.caixa p6,
                pnab.amostra_especie p7,
                pnab.especie_generica p8,
                pnab.comprimentos p9
          where p2.viagem=p1.id and 
                emb.id=p1.embarcacao and 
                por.id=p2.porto and
                rpo.id=por.regiao and 
                p2.id=p3.origem and p4.id=p3.cat_comercial and 
                p5.id=p3.denominacao_comercial and 
                p7.caixa=p6.id and p7.especie=p8.id and 
                p9.amostra=p7.id and
                p6.denominacao=p3.id and 
                p3.estrat_amostragem=1 and 
                p1.id not in 
        (select viagem id 
           from pnab.viagem_regiao 
          where regiao <> 5) and
                p9.n_nao_observados is not null and 
                p8.cod_fao in ('OCC','OCT') and
                p2.data between '2017-01-01' and '2021-12-31' and
                p1.id not in(
select distinct p1.id
           from pnab.viagem p1,
                pnab.venda p2,
                pnab.embarcacao emb,
                pnab.porto por,
                pnab.regiao_porto rpo,
                pnab.viagem_metier v_met,
                pnab.metier met,
                pnab.denominacao p3,
                pnab.cat_comercial p4,
                pnab.denominacao_comercial p5,
                pnab.caixa p6,
                pnab.amostra_especie p7,
                pnab.especie_generica p8,
                pnab.comprimentos p9
          where p2.viagem=p1.id and 
                v_met.viagem=p1.id and 
                v_met.metier=met.id and
                emb.id=p1.embarcacao and 
                por.id=p2.porto and 
                rpo.id=por.regiao and 
                p2.id=p3.origem and 
                p4.id=p3.cat_comercial and
                p5.id=p3.denominacao_comercial and 
                p7.caixa=p6.id and 
                p7.especie=p8.id and 
                p9.amostra=p7.id and
                p6.denominacao=p3.id and 
                p3.estrat_amostragem=1 and 
                p1.id not in 
        (select viagem id 
           from pnab.viagem_regiao 
          where regiao <> 5) and
                p8.cod_fao in  ('OCC','OCT') and
                p2.data between '2017-01-01' and '2021-12-31')