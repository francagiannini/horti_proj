library(tidyverse)
library(googledrive)
library(ggpubr)

### muestra ########
drive_download(
  "https://docs.google.com/spreadsheets/d/1k4rfBZTyPQuBmAWD0rGkJzfngDAOxkbmwfe19cXN9XM/edit#gid=1323869412",
  path = "data/muestra.xlsx",
  overwrite = TRUE
)

#as.vector(unique(muestra$Nombre_de_Cultivo))

muestra <- readxl::read_excel("data/muestra.xlsx")

muestra_cut <- muestra %>% select('Nombre_de_Cultivo', 
                                  'Productor_a', 
                                  'Peso_kg_m2_ciclo',
                                  as.vector(unique(muestra$Nombre_de_Cultivo)))


### MODELO BRUTO #1

#la sup_media es asignacion_sup_hort_quinta (%) x ha_hort_quinta (ha)

muestra_agregado <- 
  muestra %>% group_by(Productor_a , Nombre_de_Cultivo,.drop= TRUE) %>% 
  summarise("Prod_kg_ha_media"=mean((Peso_kg_m2_ciclo*10000), na.rm=T),
            "Prod_kg_ha_sd"=sd((Peso_kg_m2_ciclo*10000), na.rm=T),
            "n"=n(),
            "recurr"= unique(recurrencia),
            "Asig_sup_media"=mean( asignacion_sup_hort_quinta, na.rm=T),
            "Asig_sup_sd"=sd( asignacion_sup_hort_quinta, na.rm=T),
            "Sup_hort_quinta"=unique(Quinta_Superficie_Horticola),
  )

abast_quinta_especie <-data.frame('Productor_a'=c(muestra_agregado$Productor_a),
                                  'especie'=c(muestra_agregado$Nombre_de_Cultivo),
                                  'abast_quinta_especie'=c(muestra_agregado$Prod_kg_ha_media*((muestra_agregado$Asig_sup_media/100)*muestra_agregado$Sup_hort_quinta)*muestra_agregado$recurr))

abast_quinta <- 
  abast_quinta_especie %>% group_by(Productor_a,.drop= TRUE) %>% 
  summarise('abast_quinta'= sum(abast_quinta_especie))

write.table(muestra_agregado, "resumen_quintas.csv", sep="\t")

#Abastecimiento Bruto #1 en TONELADAS ANUALES
abast_mean=mean(abast_quinta$abast_quinta/1000, na.rm=T)
abast_cvc=abast_mean*191

#### MODELO BRUTO #2

mod2_muestra_agregado <- 
  muestra_agregado %>% group_by(Nombre_de_Cultivo,.drop= TRUE) %>% 
  summarise("Prod_kg_ha_media"=mean(Prod_kg_ha_media, na.rm=T),
            "Asig_sup_media"=mean(Asig_sup_media, na.rm=T),
  )

##### CENSO ####

drive_download(
  "https://docs.google.com/spreadsheets/d/1eTJ6EQRTkTNy85fOWT9ZTFBYgMwd0WLcCSp4HHR6BKM/edit#gid=296974404",
  path = "data/censo_recc.xlsx",
  overwrite = TRUE
)

censo <- readxl::read_excel("data/censo_recc.xlsx", sheet = 'prueba')

censo_recc <-
  censo %>% 
  pivot_longer(!Código, names_to = "Nombre_de_Cultivo", values_to = "recurrencia")

censo_recc_dep <- censo_recc %>% drop_na(0)

censo_ha <- censo %>% select('Código', 
                       'Quinta_Superficie_Horticola', 
)
censo_recc_dep <- merge(censo_recc_dep,censo_ha)

write.table(censo_recc_dep, "censo_recc_dep_pivot.csv", sep="\t")

mod2 <- merge(censo_recc_dep,mod2_muestra_agregado)

mod2 <- mod2%>% select('Productor_a'='Código', 
                       'Nombre_de_Cultivo',
                       'recurrencia',
                       'Prod_kg_ha_media',
                       'Asig_sup_media',
                       'Quinta_Superficie_Horticola'
)
                       
mod2_abast_quinta_especie <-data.frame('Productor_a'=c(mod2$Productor_a),
                                  'especie'=c(mod2$Nombre_de_Cultivo),
                                  'abast_quinta_especie'=c(mod2$Prod_kg_ha_media*((mod2$Asig_sup_media/100)*mod2$Quinta_Superficie_Horticola)*mod2$recurrencia))

mod2_abast_quinta <- 
  mod2_abast_quinta_especie %>% group_by(Productor_a,.drop= TRUE) %>% 
  summarise('abast_quinta'= sum(abast_quinta_especie))

#hay dos quintas que aparentemente no aportan 219 y 58 pero en la encuesta le faltan los datos de recurrencia error de censo
write.table(mod2_abast_quinta_especie, "resumen_quintas_191.csv", sep="\t")

#Abastecimiento Bruto #2 en TONELADAS ANUALES

mod2_abast_cvc=(sum(mod2_abast_quinta$abast_quinta))/1000

#### MODELO BRUTO #2 PLUS X TIPO

muestra_agregado_tipo <- muestra_agregado %>% 
  mutate('Nombre_de_Cultivo'=tolower(Nombre_de_Cultivo),
         'tipo_hortaliza'= recode_factor(Nombre_de_Cultivo,
                                         'acelga'='hoja',
                                         'achicoria'='hoja',
                                         'aji'='fruto',
                                         'ajo'='tallo_bulbo',
                                         'albahaca'='hoja',
                                         'alcaucil'='inflo_col',
                                         'apio'='tallo_bulbo',
                                         'aromaticas'='hoja',
                                         'arveja'='fruto',
                                         'batata'='tuberculo_raiz',
                                         'berenjenas'='fruto',
                                         'brocoli'='inflo_col',
                                         'brucelas_kale'='inflo_col',
                                         'calabaza'='fruto',
                                         'cebolla de bulbo'='tallo_bulbo',
                                         'cebolla verdeo'='tallo_bulbo',
                                         'chaucha'='fruto',
                                         'choclo'='fruto',
                                         'coliflor'='inflo_col',
                                         'endivia_escarola'='hoja',
                                         'espinacas'='hoja',
                                         'haba'='fruto',
                                         'hinojo'='tallo_bulbo',
                                         'lechuga'='hoja',
                                         'nabo'='tuberculo_raiz',
                                         'papa'='tuberculo_raiz',
                                         'pepino'='fruto',
                                         'perejil'='hoja',
                                         'pimiento'='fruto',
                                         'puerro'='tallo_bulbo',
                                         'rabanito'='tuberculo_raiz',
                                         'remolacha'='tuberculo_raiz',
                                         'repollo'='inflo_col',
                                         'rucula'='hoja',
                                         'tomate'='fruto',
                                         'zanahoria'='tuberculo_raiz',
                                         'zapallito verde y zucchini'='fruto',
                                         'zapallo'='fruto')
         
  ) %>% 
  group_by(tipo_hortaliza,.drop= TRUE) %>% 
  dplyr::summarise(
    "Prod_media_tipo"=mean(Prod_kg_ha_media, na.rm=T),
    "Prod_sd_tipo"=mean(Prod_kg_ha_sd, na.rm=T),
    "n_tipo"=n(),
    "recurr_media_tipo"= mean(recurr),
    "Asig_sup_media_tipo"=mean(Asig_sup_media, na.rm=T),
    "Asig_sup_sd_tipo"=mean(Asig_sup_sd, na.rm=T),
 #   "Sup_hort_quinta"=unique(Sup_hort_quinta),
  ) 

write_excel_csv2(muestra_agregado_tipo, "muestra_agregado_tipo.csv", delim=";")

#CENSO
mod2_tipo <- mod2 %>% 
  mutate('Nombre_de_Cultivo'=tolower(Nombre_de_Cultivo),
         'tipo_hortaliza'= recode_factor(Nombre_de_Cultivo,
                                         'acelga'='hoja',
                                         'achicoria'='hoja',
                                         'aji'='fruto',
                                         'ajo'='tallo_bulbo',
                                         'albahaca'='hoja',
                                         'alcaucil'='inflo_col',
                                         'apio'='tallo_bulbo',
                                         'aromaticas'='hoja',
                                         'arveja'='fruto',
                                         'batata'='tuberculo_raiz',
                                         'berenjenas'='fruto',
                                         'brocoli'='inflo_col',
                                         'brucelas_kale'='inflo_col',
                                         'calabaza'='fruto',
                                         'cebolla de bulbo'='tallo_bulbo',
                                         'cebolla verdeo'='tallo_bulbo',
                                         'chaucha'='fruto',
                                         'choclo'='fruto',
                                         'coliflor'='inflo_col',
                                         'endivia_escarola'='hoja',
                                         'espinacas'='hoja',
                                         'haba'='fruto',
                                         'hinojo'='tallo_bulbo',
                                         'lechuga'='hoja',
                                         'nabo'='tuberculo_raiz',
                                         'papa'='tuberculo_raiz',
                                         'pepino'='fruto',
                                         'perejil'='hoja',
                                         'pimiento'='fruto',
                                         'puerro'='tallo_bulbo',
                                         'rabanito'='tuberculo_raiz',
                                         'remolacha'='tuberculo_raiz',
                                         'repollo'='inflo_col',
                                         'rucula'='hoja',
                                         'tomate'='fruto',
                                         'zanahoria'='tuberculo_raiz',
                                         'zapallito verde y zucchini'='fruto',
                                         'zapallo'='fruto')
         
  ) %>% 
  group_by(Productor_a, tipo_hortaliza,.drop= TRUE) %>% 
  dplyr::summarise(
    "Prod_media_tipo"=mean(Prod_kg_ha_media, na.rm=T),
    "recurr_sum_tipo"= sum(recurrencia),
    "Asig_sup_media_tipo"=mean(Asig_sup_media, na.rm=T),
    "Sup_hort_quinta"=unique(Quinta_Superficie_Horticola),
  ) 

#Abastecimiento Bruto #2_PLUS X TIPO en TONELADAS ANUALES

#TOTAL 
mod2_abast_quinta_tipo <-data.frame('Productor_a'=c(mod2_tipo$Productor_a),
                                    'tipo_hortaliza'=c(mod2_tipo$tipo_hortaliza),
                                    'abast_quinta_tipo'=c(mod2_tipo$Prod_media_tipo*((mod2_tipo$Asig_sup_media_tipo/100)*mod2_tipo$Sup_hort_quinta)*mod2_tipo$recurr_sum_tipo))

mod2_plus_abast_quinta <- 
  mod2_abast_quinta_tipo %>% group_by(Productor_a,.drop= TRUE) %>% 
  summarise('abast_quinta'= sum(abast_quinta_tipo))

mod2_plus_abast_cvc=(sum(mod2_plus_abast_quinta$abast_quinta))/1000

#POR TIPO
mod2_abast_tipo <- 
  mod2_abast_quinta_tipo %>% group_by(tipo_hortaliza,.drop= TRUE) %>% 
  summarise('abast_tipo_cvc'= (sum(abast_quinta_tipo))/1000)

mod2_plus_abast_cvc=(sum(mod2_abast_tipo$abast_tipo_cvc))
