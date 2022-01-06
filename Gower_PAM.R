set.seed(1680) # for reproducibility
library(googledrive)
library(dplyr) # for data cleaning
#library(ISLR) # for college dataset
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization

### censo 191 ########
drive_download(
  "https://docs.google.com/spreadsheets/d/1IeSeE4nO9be7lHCwDUEaiJyu6J0DZ4lK/edit#gid=1690129552",
  path = "data/censo191.xlsx",
  overwrite = TRUE
)
censo191 <- readxl::read_excel("data/censo191.xlsx", sheet ="NOBINARIA_Base2_Mod_Censo_191")
#SET Vicky (1)
censo191_clean_vick <- censo191 %>% 
  mutate(Productor_a = Productor_a) %>%
  select(Productor_a, 
         Quinta_Acceso_Tierra,
         Quinta_Superficie_Horticola,
         Quinta_Superficie_cubierta_ha,
         Intalaciones_nro,
         Produccion_Cantidad_de_especies,
         Importancia_Economica_1_tipo,
         Comercializacion_nTipos,
         Agua_para_riego_fuente_categ,
         Agua_para_riego_tipo,
         Herramientas_Transporte_de_mercaderia,
         Agroquimicos_Aplicacion_Pulverizadora,
         Agroquimicos_Insumos,
         Practicas_productivas_nro,
         Perdidas_economicas_por_eventos_climaticos_n,
         Productor_principal_Sexo,
         Productor_principal_Pais_de_origen,
         Familia_del_PP_Reside_en_quinta,
         Medieria_UtilizaFF_Peon,
         Respondente_Experiencia_agricola_años,
         vur_2020_mean
  )



censo191_clean_vick$Productor_a <- as.factor(censo191_clean_vick$Productor_a)
censo191_clean_vick$Medieria_UtilizaFF_Peon <- as.factor(censo191_clean_vick$Medieria_UtilizaFF_Peon)
censo191_clean_vick$Familia_del_PP_Reside_en_quinta <- as.factor(censo191_clean_vick$Familia_del_PP_Reside_en_quinta)
censo191_clean_vick$Productor_principal_Pais_de_origen <- as.factor(censo191_clean_vick$Productor_principal_Pais_de_origen)
censo191_clean_vick$Productor_principal_Sexo <- as.factor(censo191_clean_vick$Productor_principal_Sexo)
censo191_clean_vick$Agroquimicos_Insumos <- as.factor(censo191_clean_vick$Agroquimicos_Insumos)
censo191_clean_vick$Agroquimicos_Aplicacion_Pulverizadora <- as.factor(censo191_clean_vick$Agroquimicos_Aplicacion_Pulverizadora)
censo191_clean_vick$Herramientas_Transporte_de_mercaderia <- as.factor(censo191_clean_vick$Herramientas_Transporte_de_mercaderia)
censo191_clean_vick$Agua_para_riego_tipo <- as.factor(censo191_clean_vick$Agua_para_riego_tipo)
censo191_clean_vick$Agua_para_riego_fuente_categ <- as.factor(censo191_clean_vick$Agua_para_riego_fuente_categ)
censo191_clean_vick$Importancia_Economica_1_tipo <- as.factor(censo191_clean_vick$Importancia_Economica_1_tipo)
censo191_clean_vick$Quinta_Acceso_Tierra <- as.factor(censo191_clean_vick$Quinta_Acceso_Tierra)


glimpse(censo191_clean_vick)


# Remove productor name before clustering

gower_dist <- daisy(censo191_clean_vick[, -1],
                    metric = "gower",
                    type = list(logratio = 3))
summary(gower_dist)
gower_mat <- as.matrix(gower_dist)

# Output most similar pair
censo191_clean_vick[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
# Output most dissimilar pair
censo191_clean_vick[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Calculate silhouette width for many k using PAM

sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}
# Plot sihouette width (higher is better)

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

#PAM 
pam_fit <- pam(gower_dist, diss = TRUE, k = 4)
pam_results <- censo191_clean_vick %>%
  dplyr::select(-Productor_a) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

censo191_clean_vick[pam_fit$medoids, ]

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE, perplexity = 5, step=5000, epsilon=10, dims=2)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = censo191_clean_vick$Productor_a)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))