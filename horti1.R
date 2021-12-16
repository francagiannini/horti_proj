library(tidyverse)
library(googledrive)
library(ggpubr)

### muestra ########
drive_download(
  "https://docs.google.com/spreadsheets/d/1mr8NAuNCDRg20TFOT69bobscBcu8RhfolD9qZOWsNFQ/edit#gid=579148914",
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

muestra_agregado <- 
  muestra %>% group_by(Productor_a , Nombre_de_Cultivo,.drop= TRUE) %>% 
  summarise("Prod_media"=mean(Peso_kg_m2_ciclo, na.rm=T),
            "Prod_sd"=sd(Peso_kg_m2_ciclo, na.rm=T),
            "n"=n(),
            "recurr"= unique(recurrencia),
            "Sup_media"=mean(asignacion_sup_hort_quinta, na.rm=T),
            "Sup_sd"=sd(asignacion_sup_hort_quinta, na.rm=T),
  )

abast_quinta_especie <-data.frame('Productor_a'=c(muestra_agregado$Productor_a),
                                  'especie'=c(muestra_agregado$Nombre_de_Cultivo),
                                  'abast_quinta_especie'=c(muestra_agregado$Prod_media*muestra_agregado$Sup_media*muestra_agregado$recurr))

abast_quinta <- 
  abast_quinta_especie %>% group_by(Productor_a,.drop= TRUE) %>% 
  summarise('abast_quinta'= sum(abast_quinta_especie))

write.table(muestra_agregado, "resumen_quintas.csv", sep="\t")

abast_mean=mean(abast_quinta$abast_quinta, na.rm=T)
abast_cvc=abast_mean*170


# resumen_muestra
muestra_res <- muestra %>% group_by(Nombre_de_Cultivo) %>% 
  summarise(tipo= unique(tipo_hortaliza),
            media_muestra=mean(Peso_kg_m2),
            sd_muestra=sd(Peso_kg_m2),
            n_muestra=n()#,
            #"IC"=mean_ci(Peso_kg_m2,ci=0.95)
  )

########### FAO  #####

drive_download(
  "https://drive.google.com/file/d/1wtpHKqKBdw0stfnGcx_Dastq2FUApvZj/view?ts=6115312d",
  path = "data/fao.xlsx",
  overwrite = TRUE
)

fao <- readxl::read_excel("data/fao.xlsx", sheet="Copia de conosur")

fao_resumen <- fao %>% group_by(Producto_a) %>% 
  #filter(Producto) %>%
  dplyr::summarise(
    media_fao=mean(`Valor_a`),
    sd_fao=sd(`Valor_a`),
    n_fao=n(),
    # ci=mean_ci(`Valor_a`,ci=0.95)
    #IC95_inf=media-qt(1 - (alpha / 2))* sd/sqrt(n),
    # IC95_sup=media+qt(1 - (alpha / 2))* sd/sqrt(n)
    
  ) %>% mutate('Nombre_de_Cultivo'=tolower(Producto_a))


merge_meta <- full_join(muestra_res, fao_resumen, by='Nombre_de_Cultivo')

#fao_resumen_filter <- fao_tabla %>% filter(n>3)

######### Forestplot spp  #########

#install.packages("meta")

library(meta)
merge_meta_dep <- merge_meta %>% drop_na()

#### MD ####
mm <- metacont(n_muestra, media_muestra,sd_muestra,
               n_fao,media_fao,sd_fao,
               studlab=paste(Nombre_de_Cultivo),
               data=merge_meta_dep,
               comb.random = TRUE)




forest(mm, xlab = "Productividad")


#### SMD ####
mm_est <-metacont(n_muestra, media_muestra,sd_muestra,
                  n_fao,media_fao,sd_fao,sm="SMD",
                  studlab=paste(Nombre_de_Cultivo),
                  data=merge_meta_dep,
                  comb.random = TRUE) 

forest(mm_est, xlab = "Productividad")

#### ROM ####

mm_rom<-metacont(n_muestra, media_muestra,sd_muestra,
                 n_fao,media_fao,sd_fao,sm="ROM",
                 studlab=paste(Nombre_de_Cultivo),
                 data=merge_meta_dep,
                 comb.random = TRUE) 

forest(mm_rom, xlab = "Productivity", lab.e = 'Experimental_2019', lab.c = 'FAOSTAT_2019')


### tipo #############

muestra_res_tipo <- muestra %>% 
  group_by(tipo_hortaliza) %>% 
  summarise(tipo= unique(tipo_hortaliza),
            media_muestra=mean(Peso_kg_m2_ciclo),
            sd_muestra=sd(Peso_kg_m2_ciclo),
            n_muestra=n()
  )

fao_resumen_tipo <- fao %>% 
  mutate('Producto_a'=tolower(Producto_a),
         'tipo_hortaliza'= recode_factor(Producto_a,
                                         "achicoria"='hoja',
                                         "aji"='fruto',
                                         "ajo"='tallo_bulbo',
                                         "alcaucil"='inflo_col',
                                         "apio"='tallo_bulbo',
                                         "arvejas"='fruto',
                                         "batata"='tuberculo_raiz',
                                         "berenjenas"='fruto',
                                         "brocoli"='inflo_col',
                                         "calabaza"='fruto',
                                         "cebolla verdeo"='tallo_bulbo',
                                         "chaucha"='fruto', 
                                         "choclo"='fruto',
                                         "coliflor"='inflo_col',
                                         "espinacas"='hoja',
                                         "haba"='fruto',
                                         "hinojo"='tallo_bulbo',
                                         "lechuga"='hoja',
                                         "nabo"='tuberculo_raiz',
                                         "papa"='tuberculo_raiz',
                                         "pepino"='fruto',
                                         "perejil"='hoja',
                                         "pimiento"='fruto',
                                         "puerro"='tallo_bulbo',
                                         "rabanito"='tuberculo_raiz',
                                         "remolacha"='tuberculo_raiz',
                                         "repollo"='inflo_col',
                                         "rucula"='hoja',        
                                         "tomates"='fruto',
                                         "zanahoria"='tuberculo_raiz',
                                         "zapallo"='fruto')
         
  ) %>% 
  group_by(tipo_hortaliza) %>% 
  dplyr::summarise(
    media_fao=mean(`Valor_a`),
    sd_fao=sd(`Valor_a`),
    n_fao=n()  
  ) 

merge_meta_tipo <- full_join(muestra_res_tipo, fao_resumen_tipo, by='tipo_hortaliza')

#### ROM ####

mm_rom_tipo <-metacont(n_muestra, media_muestra,sd_muestra,
                       n_fao,media_fao,sd_fao,
                       sm="ROM",
                       studlab=paste(tipo_hortaliza),
                       data=merge_meta_tipo,
                       random = TRUE) 

forest(mm_rom_tipo, xlab = "Productivity", lab.e = 'Experimental_2019', lab.c = 'FAOSTAT_2019')

## Lanfranconi ####

drive_download( 
  "https://docs.google.com/spreadsheets/d/12dlrZ0SDJwt2gMkhd9hMO6hcvBUQbPbo1YvehEvawFk/edit?usp=sharing", 
  path = "data/lanfranconi", overwrite = TRUE)

lanfranconi <-  readxl::read_excel("data/lanfranconi.xlsx", sheet="Copia de lanfranconi")[1:39,]


lanfranconi <- lanfranconi %>% mutate('Nombre_de_Cultivo'=tolower(Cultivo),

merge_meta_lan<- full_join(muestra_res, lanfranconi, by='Nombre_de_Cultivo') %>% drop_na(c(sd_muestra, Peso_kg_m2))

merge_meta_lan <- merge_meta_lan %>% mutate(cv_lan = sd_muestra/media_muestra,
                                            sd_lan = cv_lan*Peso_kg_m2)

mm_rom_lan<-metacont(n_muestra, media_muestra,sd_muestra,
                     n_muestra,Peso_kg_m2,sd_lan,
                     sm="ROM",
                     studlab=paste(Nombre_de_Cultivo),
                     data=merge_meta_lan,
                     comb.random = TRUE) 

forest(mm_rom_lan, xlab = "Productivity", lab.e = 'Experimental_2019', lab.c = 'Lanfranconi_1982')