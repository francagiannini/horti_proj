library(tidyverse)
library(lubridate)
library(sf)
library(spdep)


muestra <- readxl::read_excel("data/muestra.xlsx")
########## figura Variacion temporal de superficie asignada #################
muestra %>% count(Nombre_de_Cultivo, tipo_hortaliza)
muestra <- muestra %>% dplyr::mutate(mes=month(`Fecha muestra`))
ggplot(muestra,aes(y= `Sup asig x frec (%)...291`, x=mes))+ 
  geom_point() +
  geom_smooth()+
  facet_grid( rows = vars(tipo_hortaliza), scales = "free") +
  theme_light()+
  scale_x_continuous(name="Month", breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+ # eje x
  ylab("Asignación de superficie x recurrencia (%)")+ # eje y
  ggtitle("Variación anual de superficie asignada por tipo de hortaliza")
ggplot(muestra,aes(y= `% asignacion de sup hort quinta...25`, x=mes))+ 
  geom_point() +
  geom_smooth()+
  facet_grid( rows = vars(tipo_hortaliza), scales = "free") +
  theme_light()+scale_x_continuous(name="Month", breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+ # eje x
  ylab("Asignación de superficie (%)")+ # eje y
  ggtitle("Variación anual de superficie asignada por tipo de hortaliza")

########## figura Variacion temporal de Productividad #################
muestra %>% count(Nombre_de_Cultivo, tipo_hortaliza)

#table(muestra$Nombre_de_Cultivo,muestra$tipo_hortaliza)

#muestra$mes <- as.numeric(muestra$`Fecha mes`)

muestra <- muestra %>% dplyr::mutate(mes=month(`Fecha muestra`))

ggplot(muestra,aes(y= Peso_kg_m2_ciclo, x=mes))+ 
  #stat_summary(fun=mean, geom="line")+
  geom_point() +
  #geom_line()+
  geom_smooth()+
  facet_grid( rows = vars(tipo_hortaliza), scales = "free") +
  theme_light()+
  scale_x_continuous(name="Month", breaks=c(1,2,3,4,5,6,7,8,9,10,11,12))+ # eje x
  ylab("Productivity (kg/m2)")#+ # eje y
  #ggtitle("Variación anual de productividad por tipo de hortaliza")

ggplot(muestra,aes(y= Peso_kg_m2, x=mes, colour=Nombre_de_Cultivo))+ 
  geom_point() +
  geom_smooth(method = lm)+
  facet_grid( rows = vars(tipo_hortaliza), scales = "free") +
  theme_light()

library(nlme)
mod1 <-
  lme(
    Peso_kg_m2 ~ as.factor(mes),
    random = list(tipo_hortaliza = pdIdent( ~ 1)),
    weights=varComb(varIdent(form=~1|tipo_hortaliza)),
    data = muestra
  )

anova(mod1)
summary(mod1)
plot(mod1)  



muestra %>% filter(tipo_hortaliza=='hoja') %>% 
ggplot(aes(y= Peso_kg_m2, x=mes, colour=Nombre_de_Cultivo))+ 
geom_point() +
geom_smooth() +
theme_light()
  
  
muestra %>% filter(tipo_hortaliza=='fruto') %>% 
  ggplot(aes(y= Peso_kg_m2, x=mes, colour=Nombre_de_Cultivo))+ 
  geom_point() +
  geom_smooth()+
  theme_light()


muestra_agregado <- 
  muestra %>% group_by(Productor_a , Nombre_de_Cultivo,.drop= TRUE) %>% 
  summarise("Prod_media"=mean(Peso_kg_m2_mensual, na.rm=T),
            "Prod_sd"=sd(Peso_kg_m2_mensual, na.rm=T),
            "n"=n(),
            "recurr"= unique(recurrencia...273),
            "lat"=mean(GPS_latitud),
            "long"=mean(GPS_longitud)
  )

################# correlacion espacial #############
library(sf)
library(tmap)
library(googledrive)


# drive_download(
#   "https://drive.google.com/file/d/1uZ11MYL-daDDpR76gd1JBQJBPT8SVFej/view?usp=sharing",
#   path = "data/muestra_gpkg.gpkg",
#   overwrite = TRUE
# )

muestra_gpk <- st_read("data/muestra_gpkg.gpkg") 



muestra_gpk_j <- muestra_gpk %>%
  mutate(
    cultivo_rename = recode_factor(
      Nombre_de_Cultivo,
      "Aji picante" = "aji",
      "Albahaca" = "albahaca",
      "berenjena" = "berenjenas",
      "br_coli" = "brocoli",
      "calabac_n___za" = "calabaza",
      "cebolla_de_ver" = "cebolla verdeo",
      "espinaca" = "espinacas",
      "r_cula" = "rucula",
      "zapallito_verd" = "zapallito verde y zucchini",
      "zuchini" = "zapallito verde y zucchini"
    )
  ) %>%
  unite("id", c('Fecha.muestra', 'cultivo_rename', 'Productor_a', 'Foto_Cultivo')) %>%
  select("id")



muestra_j <-
  muestra %>% select(c(
    'Fecha muestra',
    'Nombre_de_Cultivo',
    'Productor_a',
    'Peso_kg_m2',
    'Foto_Cultivo',
    'tipo_hortaliza'
  )) %>%
  unite("id", c('Fecha muestra', 'Nombre_de_Cultivo', 'Productor_a', 'Foto_Cultivo'))

#length(muestra_gpk_j$id) == length(muestra_j$id)

#waldo::compare(muestra_gpk_j, muestra_j)

muestra_sf <- merge(muestra_j,muestra_gpk_j, by="id")

muestra_sf <- muestra_sf %>% unique()

#st_write(muestra_sf, "muestra_sf.gpkg")
#muestra_sf<- st_read("data/muestra_gpkg.gpkg")

summary(muestra_sf$Peso_kg_m2)

#Moran Index

muestra_sf <- st_as_sf(muestra_sf)

muestra_sf <- st_transform(muestra_sf, crs=32720)

tipo <- levels(as.factor(muestra_sf$tipo_hortaliza))

sp_index <- function(i){

muestra_tipo<- muestra_sf %>%  filter(tipo_hortaliza==paste(i)) %>% drop_na(Peso_kg_m2)

vecindarios <- dnearneigh(st_coordinates(muestra_tipo),
                          d1 = 5, d2 = 20000)

pesos_sp <- nb2listw(vecindarios, 
                     style = "W", 
                     zero.policy = TRUE)

i.moran <- moran.mc(
    as.numeric(muestra_tipo$Peso_kg_m2),
    listw = pesos_sp,
    nsim = 999,
    zero.policy = T
  )

i.geary <- geary.test(muestra_tipo$Peso_kg_m2, 
           pesos_sp, 
           zero.policy=TRUE)

cbind(
"tipo"=muestra_tipo$tipo_hortaliza,
"moran"=i.moran$statistic,
"moran_p-value"=i.moran$p.value,
"geary"=i.geary$statistic,
"geary_p-value"=i.geary$p.value)

}

indices <- lapply(tipo, sp_index)
indices_sp_table <- unique(do.call(rbind,indices));indices_sp_table


