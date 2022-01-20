library(tidyverse)
library(googledrive)
library(ggpubr)

### muestra ########
drive_download(
  "https://docs.google.com/spreadsheets/d/1k4rfBZTyPQuBmAWD0rGkJzfngDAOxkbmwfe19cXN9XM/edit#gid=1323869412",
  path = "data/muestra.xlsx",
  overwrite = TRUE
)

muestra <- readxl::read_excel("data/muestra.xlsx", sheet = 'Para_cluster_nobinaria_modelo_p')

muestra_agregado <- muestra %>%  
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
                                         'zapallo_anco'='fruto',
                                         'cebolla de bulbo'='tallo_bulbo',
                                         'cebolla_de_verdeo'='tallo_bulbo',
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
                                         'zapallito_verde_y_zucchini'='fruto',
                                         'zapallo'='fruto')  ) %>%  
  group_by(Productor_a , Codigo, Nombre_de_Cultivo,tipo_hortaliza, .drop= TRUE) %>%
  summarise("Prod_kg_ha_media"=mean((Peso_kg_m2_ciclo*10000), na.rm=T),
            #"Prod_kg_ha_sd"=sd((Peso_kg_m2_ciclo*10000), na.rm=T),
            "n"=n(),
            "recurr"= unique(recurrencia),
            "Asig_sup_media"=mean( asignacion_sup_hort_quinta, na.rm=T),
            #"Asig_sup_sd"=sd( asignacion_sup_hort_quinta, na.rm=T),
            "Sup_hort_quinta"=unique(Quinta_Superficie_Horticola),
  )

abast_quinta_especie <-data.frame('Codigo'=c(muestra_agregado$Codigo),
                                  'Productor_a'=c(muestra_agregado$Productor_a),
                                  'Nombre_de_Cultivo'=c(muestra_agregado$Nombre_de_Cultivo),
                                  'tipo_hortaliza'=c(muestra_agregado$tipo_hortaliza),
                                  'abast_quinta_especie'=c(muestra_agregado$Prod_kg_ha_media*((muestra_agregado$Asig_sup_media/100)*muestra_agregado$Sup_hort_quinta)))
#Le saco la multiplicacion por recurrencia por especie 
#dado que luego la multiplicaré por la suma de todas las recurrencias del TIPO en cada quinta

abast_quinta_tipo <-   abast_quinta_especie %>% 
  group_by(Codigo, Productor_a, tipo_hortaliza,.drop= TRUE) %>% 
  summarise('abast_quinta_tipo'= mean(abast_quinta_especie))

## Infiero con el abastecimiento medio por tipo por quinta (20) 
## sobre otras especies que la quinta produce y no fueron muestreadas

##### CENSO datos de 20quintas (rec) ####

drive_download(
  "https://docs.google.com/spreadsheets/d/1eTJ6EQRTkTNy85fOWT9ZTFBYgMwd0WLcCSp4HHR6BKM/edit#gid=296974404",
  path = "data/censo_recc.xlsx",
  overwrite = TRUE
)

censo <- readxl::read_excel("data/censo_recc.xlsx", sheet = 'prueba')

censo_recc <-  censo %>% dplyr::select(-Quinta_Superficie_Horticola) %>%
  pivot_longer(!Codigo, names_to = "Nombre_de_Cultivo", values_to = "recurrencia") %>%
  drop_na(0)

censo_recc_tipo <- censo_recc %>%  
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
                                         'zapallo'='fruto')  ) %>%  
  group_by(Codigo, tipo_hortaliza, .drop= TRUE) %>%
  summarise("recurrencia_tipo"= sum(recurrencia))

censo20_recc_tipo <- censo_recc_tipo[censo_recc_tipo$Codigo %in% c('QH10',
                                                                   'QH106',
                                                                   'QH11',
                                                                   'QH113',
                                                                   'QH13',
                                                                   'QH150',
                                                                   'QH3',
                                                                   'QH35',
                                                                   'QH42',
                                                                   'QH451',
                                                                   'QH48',
                                                                   'QH60',
                                                                   'QH63',
                                                                   'QH66',
                                                                   'QH71',
                                                                   'QH75',
                                                                   'QH76',
                                                                   'QH79',
                                                                   'QH82',
                                                                   'QH9'),]

censo20_abast_tipo <- merge(censo20_recc_tipo, abast_quinta_tipo, by='Codigo', all=TRUE)

censo20_abast_tipo <-censo20_abast_tipo[censo20_abast_tipo[, "tipo_hortaliza.x"] == censo20_abast_tipo[, "tipo_hortaliza.y"],]

censo20_abast_tipo <- censo20_abast_tipo %>% 
  select(!'tipo_hortaliza.y')
#AHORA SÍ ABASTECIMIENTO (kg anuales) X TIPO EN CADA QUINTA
censo20_abast_tipo <-data.frame('Productor_a'=c(censo20_abast_tipo$Productor_a),
                                'Codigo'=c(censo20_abast_tipo$Codigo),
                                'tipo_hortaliza'=c(censo20_abast_tipo$tipo_hortaliza),
                                'abast_quinta_tipo'=c(censo20_abast_tipo$abast_quinta_tipo*censo20_abast_tipo$recurrencia_tipo))

###########################
#Agrego clasificaciones a tabla con valores de abastecimiento

#No me funcionaba leerlo directamente de mi carpeta ¡revisar!
drive_download(
  "https://docs.google.com/spreadsheets/d/1coUDpwTNx9x6v9h4KzlJLtdZ_PM479NZ/edit#gid=978840971",
  path = "clusters.xlsx",
  overwrite = TRUE
)
clusters <- readxl::read_excel("clusters.xlsx")

mac <- merge(censo20_abast_tipo, clusters, by='Codigo', all=FALSE)%>%
  dplyr::select(-name)

###########################

#### CLASE 1__MODELO BRUTO #1_modificado_Cluster  X TIPO 

mac_tipo1 <- mac %>% 
  group_by(class1,tipo_hortaliza,.drop= TRUE) %>% 
  dplyr::summarise('AtC1_mean'=mean(abast_quinta_tipo, na.rm = T),
                   "n_C1_tipo"=n(),
                   "class1"=unique(class1)) 

write_excel_csv2(mac_tipo1, "muestra_agregado_tipo_class1.xlsx", delim=";")

#### CLASE 2__MODELO BRUTO #1_modificado_Cluster X TIPO 

mac_tipo2 <- mac %>% 
  group_by(class2,tipo_hortaliza,.drop= TRUE) %>% 
  dplyr::summarise('AtC2_mean'=mean(abast_quinta_tipo, na.rm = T),
                   "n_C2_tipo"=n(),
                   "class2"=unique(class2)) 

write_excel_csv2(mac_tipo2, "muestra_agregado_tipo_class2.xlsx", delim=";")

#### CLASE 3__MODELO BRUTO #1_modificado_Cluster X TIPO 

mac_tipo3 <- mac %>% 
  group_by(class3,tipo_hortaliza,.drop= TRUE) %>% 
  dplyr::summarise('AtC3_mean'=mean(abast_quinta_tipo, na.rm = T),
                   "n_C3_tipo"=n(),
                   "class3"=unique(class3)) 

write_excel_csv2(mac_tipo3, "muestra_agregado_tipo_class3.xlsx", delim=";")

#Abastecimiento Bruto #1_CLUSTERS X TIPO en TONELADAS ANUALES

#Class1
mac_tipo1$At_c1 <- 0                                      
for (i in seq_along(mac_tipo1$class1)) {
  if (mac_tipo1$class1[[i]] == 1) { 
    mac_tipo1$At_c1[[i]] <- mac_tipo1$AtC1_mean[[i]]*65 
  } else  if (mac_tipo1$class1[[i]] == 2){
    mac_tipo1$At_c1[[i]] <- mac_tipo1$AtC1_mean[[i]]*55
  } else  if (mac_tipo1$class1[[i]] == 3){
  mac_tipo1$At_c1[[i]] <- mac_tipo1$AtC1_mean[[i]]*41
  } else  if (mac_tipo1$class1[[i]] == 4){
    mac_tipo1$At_c1[[i]] <- mac_tipo1$AtC1_mean[[i]]*30
  }
}

abast_c1_tipo <- mac_tipo1 %>% 
  group_by(tipo_hortaliza,.drop= TRUE) %>% 
  dplyr::summarise('At_sum_c1'=sum(At_c1)/1000,) 
abast_c1_tipo
abast_c1<-sum(abast_c1_tipo$At_sum_c1)
abast_c1

#Class2
mac_tipo2$At_c2 <- 0                                      
for (i in seq_along(mac_tipo2$class2)) {
  if (mac_tipo2$class2[[i]] == 1) { 
    mac_tipo2$At_c2[[i]] <- mac_tipo2$AtC2_mean[[i]]*98 
  } else  if (mac_tipo2$class2[[i]] == 2){
    mac_tipo2$At_c2[[i]] <- mac_tipo2$AtC2_mean[[i]]*93
  }
}

abast_c2_tipo <- mac_tipo2 %>% 
  group_by(tipo_hortaliza,.drop= TRUE) %>% 
  dplyr::summarise('At_sum_c2'=sum(At_c2)/1000,) 
abast_c2_tipo
abast_c2<-sum(abast_c2_tipo$At_sum_c2)
abast_c2

#Class3
mac_tipo3$At_c3 <- 0                                      
for (i in seq_along(mac_tipo3$class3)) {
  if (mac_tipo3$class3[[i]] == 1) { 
    mac_tipo3$At_c3[[i]] <- mac_tipo3$AtC3_mean[[i]]*70 
  } else  if (mac_tipo3$class3[[i]] == 2){
    mac_tipo3$At_c3[[i]] <- mac_tipo3$AtC3_mean[[i]]*50
  } else  if (mac_tipo3$class3[[i]] == 3){
    mac_tipo3$At_c3[[i]] <- mac_tipo3$AtC3_mean[[i]]*27
  } else  if (mac_tipo3$class3[[i]] == 4){
    mac_tipo3$At_c3[[i]] <- mac_tipo3$AtC3_mean[[i]]*44
  }
}

abast_c3_tipo <- mac_tipo3 %>% 
  group_by(tipo_hortaliza,.drop= TRUE) %>% 
  dplyr::summarise('At_sum_c3'=sum(At_c3)/1000,) 
abast_c3_tipo
abast_c3<-sum(abast_c3_tipo$At_sum_c3)
abast_c3
