library(tidyverse)
library(lubridate)
library(sf)
library(spdep)
library(googledrive)


### muestra ########
drive_download(
  "https://docs.google.com/spreadsheets/d/1Oxm_2O8KQpKgQKWDhB39I7Pbqdef2wYE/edit#gid=1608429386",
  path = "data/muestra.xlsx",
  overwrite = TRUE
)

muestra <- readxl::read_excel("data/muestra.xlsx")

prod_global <- muestra %>% group_by(Codigo) %>% 
  mutate(prod_media=mean(Peso_kg_m2_ciclo)) %>% 
  select(prod_media) %>% unique()

prod_tipo <- muestra %>% group_by(Codigo,tipo_hortaliza) %>% 
  mutate(prod_media=mean(Peso_kg_m2_ciclo)) %>% 
  select(prod_media) %>% unique() %>% 
  pivot_wider(names_from = tipo_hortaliza, 
              values_from = prod_media,
              names_prefix = 'prod_')

num_tipo <- muestra %>% group_by(Codigo) %>% 
   #mutate(prod_media=mean(Peso_kg_m2_mensual)) %>% 
  mutate(no_rows = nlevels(as.factor(tipo_hortaliza))) %>% 
  select(Codigo, no_rows) %>% 
  unique() 


sup_tipo <- muestra %>% group_by(Codigo,tipo_hortaliza) %>% 
  mutate(sup_mean_tipo=mean(asignacion_sup_hort_quinta)) %>% 
  select(sup_mean_tipo) %>% unique() %>% 
  pivot_wider(names_from = tipo_hortaliza, 
              values_from = sup_mean_tipo,
              names_prefix = 'sup_') 

rec_tipo <- muestra %>% group_by(Codigo,tipo_hortaliza) %>% 
  mutate(rec_sum_tipo=sum(recurrencia)) %>% 
  select(rec_sum_tipo) %>% unique() %>% 
  pivot_wider(names_from = tipo_hortaliza, 
              values_from = rec_sum_tipo,
              names_prefix = 'rec_') 

rtas <- bind_cols(prod_global, 
                  prod_tipo,
                  num_tipo, 
                  sup_tipo, 
                  rec_tipo
                  #.name_repair="universal"
                  ) %>% 
  mutate(Codigo=Codigo...1) %>%
  select(!starts_with("Codigo.."))

muestra20q <- readxl::read_excel("data/muestra.xlsx", sheet = 'Prueba_setVICKY_20quintas')#'20quintas')
#cambie la base de 20 quintas por la no binaria
base_quintas <- bind_cols(muestra20q,
                          rtas)%>% 
  mutate(Codigo=Codigo...1) %>% 
  select(!starts_with("Codigo..."))%>% 
  select(!starts_with("Observa")) %>%
  select(!starts_with("Motivo"))

write.table(base_quintas, "base_quintas.txt", sep="\t")

base_quintas_num<- base_quintas %>% select_if(is.numeric)

write.table(base_quintas_num, "base_quintas_num.txt", sep="\t")

