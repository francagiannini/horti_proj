library(tidyselect)
library(magrittr)
library(corrplot)

base_quintas <- read.table("base_quintas.txt", 
                           header = T, 
                           sep = "\t", 
                           na.strings = "NA")
base_quintas[is.na(base_quintas)] <- 0

##### Variables continuas_CORRELACION ###########

data_sel_num<- base_quintas %>% select_if(is.numeric)
#colnames(data_sel_num)

data_sel_num<- data_sel_num %>% dplyr::select(!'acelga':'rabanito')

corr <- cor(data_sel_num, use = "pairwise.complete.obs", method = "spearman")

corr <- as.data.frame(corr)  
  
write_excel_csv2(corr, "corr.xlsx", delim=';')


corr0.4 <- as.data.frame(corr) %>% 
  rownames_to_column() %>%
  gather(key="variable", value="correlation", -rowname) %>%
  dplyr::filter(between(abs(correlation), 0.4, 0.99)) %>%
  dplyr::filter(rowname=='no_rows' |
                  rowname== "prod_media" |
                  rowname== "prod_hoja"|
                  rowname== "prod_fruto" |
                  rowname== "prod_tallo_bulbo"|
                  rowname== "prod_tuberculo_raiz"|
                  rowname== "prod_inflo_col"|
                  rowname== "prod_tallo_bulbo"|
                  rowname== "prod_tallo_bulbo"|
                  rowname== "sup_hoja"|
                  rowname== "sup_fruto"|
                  rowname== "sup_tallo_bulbo"|
                  rowname== "sup_tuberculo_raiz"|
                  rowname== "sup_inflo_col"|
                  rowname== "rec_hoja"|
                  rowname== "rec_fruto"|
                  rowname== "rec_tallo_bulbo"|
                  rowname== "rec_tuberculo_raiz"|
                  rowname== "rec_inflo_col")


write_excel_csv2(corr0.4, "corr0.4.xlsx", delim=';')

#########  VARIABLES CATEGORICAS #############

data_sel_cat <- base_quintas %>% select_if(is.character) 

##productividad media
data_sel_cat$prod_media <- base_quintas$prod_media

anov <- function(i){
  
colnames(data_sel_cat)[i]

data_sel <- base_quintas %>% select(prod_media, colnames(data_sel_cat)[i])

try(anova <- anova(lm(formula = prod_media~., data_sel)), TRUE)

a <- anova$`Pr(>F)`[1]

ifelse(is.numeric(a), a, "NA")

cbind(colnames(data_sel_cat)[i], a, a<0.05)

}
#resanova <- lapply(2:10,anov)

resanova <- lapply(2:ncol(data_sel_cat)-1,anov)

resanova <- as.data.frame(do.call("rbind", resanova))

colnames(resanova) <- c("variable","p","sign")

write_excel_csv2(resanova, "resanova_prod.csv", delim=";")
write.table(resanova, "resanova_prod.txt", sep="\t")

resanova %>% filter(p< 0.1)

##productividad POR TIPO: hoja
data_sel_cat <- base_quintas %>% select_if(is.character)
data_sel_cat$prod_hoja <- base_quintas$prod_hoja

anov <- function(i){
  
  colnames(data_sel_cat)[i]
  
  data_sel <- base_quintas %>% select(prod_hoja, colnames(data_sel_cat)[i])
  
  try(anova <- anova(lm(formula = prod_hoja~., data_sel)), TRUE)
  
  a <- anova$`Pr(>F)`[1]
  
  ifelse(is.numeric(a), a, "NA")
  
  cbind(colnames(data_sel_cat)[i], a, a<0.05)
  
}

resanova <- lapply(2:ncol(data_sel_cat)-1,anov)

resanova <- as.data.frame(do.call("rbind", resanova))

colnames(resanova) <- c("variable","p","sign")

write.table(resanova, "resanova_prod_hoja.txt", sep="\t")
write_excel_csv2(resanova, "resanova_hoja.csv", delim=";")

resanova %>% filter(p< 0.1)

##productividad POR TIPO: tallo_bulbo
data_sel_cat <- base_quintas %>% select_if(is.character)
data_sel_cat$prod_tallo_bulbo <- base_quintas$prod_tallo_bulbo

anov <- function(i){
  
  colnames(data_sel_cat)[i]
  
  data_sel <- base_quintas %>% select(prod_tallo_bulbo, colnames(data_sel_cat)[i])
  
  try(anova <- anova(lm(formula = prod_tallo_bulbo~., data_sel)), TRUE)
  
  a <- anova$`Pr(>F)`[1]
  
  ifelse(is.numeric(a), a, "NA")
  
  cbind(colnames(data_sel_cat)[i], a, a<0.05)
  
}

resanova <- lapply(2:ncol(data_sel_cat)-1,anov)

resanova <- as.data.frame(do.call("rbind", resanova))

colnames(resanova) <- c("variable","p","sign")

write.table(resanova, "resanova_prod_tallo_bulbo.txt", sep="\t")
write_excel_csv2(resanova, "resanova_prod_tallo_bulbo.csv", delim=";")
resanova %>% filter(p< 0.1)

##productividad POR TIPO: fruto
data_sel_cat <- base_quintas %>% select_if(is.character)
data_sel_cat$prod_fruto <- base_quintas$prod_fruto  


anov <- function(i){
  
  colnames(data_sel_cat)[i]
  
  data_sel <- base_quintas %>% select(prod_fruto, colnames(data_sel_cat)[i])
  
  try(anova <- anova(lm(formula = prod_fruto~., data_sel)), TRUE)
  
  a <- anova$`Pr(>F)`[1]
  
  ifelse(is.numeric(a), a, "NA")
  
  cbind(colnames(data_sel_cat)[i], a, a<0.05)
  
}

resanova <- lapply(2:ncol(data_sel_cat)-1,anov)

resanova <- as.data.frame(do.call("rbind", resanova))

colnames(resanova) <- c("variable","p","sign")

write.table(resanova, "resanova_prod_fruto.txt", sep="\t")
write_excel_csv2(resanova, "resanova_prod_fruto.csv", delim=";")
resanova %>% filter(p< 0.1)

##productividad POR TIPO: tuberculo_raiz
data_sel_cat <- base_quintas %>% select_if(is.character)
data_sel_cat$prod_tuberculo_raiz <- base_quintas$prod_tuberculo_raiz

anov <- function(i){
  
  colnames(data_sel_cat)[i]
  
  data_sel <- base_quintas %>% select(prod_tuberculo_raiz, colnames(data_sel_cat)[i])
  
  try(anova <- anova(lm(formula = prod_tuberculo_raiz~., data_sel)), TRUE)
  
  a <- anova$`Pr(>F)`[1]
  
  ifelse(is.numeric(a), a, "NA")
  
  cbind(colnames(data_sel_cat)[i], a, a<0.05)
  
}

resanova <- lapply(2:ncol(data_sel_cat)-1,anov)

resanova <- as.data.frame(do.call("rbind", resanova))

colnames(resanova) <- c("variable","p","sign")

write.table(resanova, "resanova_prod_tuberculo_raiz.txt", sep="\t")
write_excel_csv2(resanova, "resanova_prod_tuberculo_raiz.csv", delim=";")
resanova %>% filter(p< 0.1)

##productividad POR TIPO: inflo_col
data_sel_cat <- base_quintas %>% select_if(is.character)
data_sel_cat$prod_inflo_col <- base_quintas$prod_inflo_col

anov <- function(i){
  
  colnames(data_sel_cat)[i]
  
  data_sel <- base_quintas %>% select(prod_inflo_col, colnames(data_sel_cat)[i])
  
  try(anova <- anova(lm(formula = prod_inflo_col~., data_sel)), TRUE)
  
  a <- anova$`Pr(>F)`[1]
  
  ifelse(is.numeric(a), a, "NA")
  
  cbind(colnames(data_sel_cat)[i], a, a<0.05)
  
}

resanova <- lapply(2:ncol(data_sel_cat)-1,anov)

resanova <- as.data.frame(do.call("rbind", resanova))

colnames(resanova) <- c("variable","p","sign")

write.table(resanova, "resanova_prod_inflo_col.txt", sep="\t")
write_excel_csv2(resanova, "resanova_prod_inflo_col.csv", delim = ";")
resanova %>% filter(p< 0.1)


####### SUPERFICIE por TIPO ########

##superficie POR TIPO: hoja
data_sel_cat <- base_quintas %>% select_if(is.character)
data_sel_cat$sup_hoja <- base_quintas$sup_hoja

anov <- function(i){
  
  colnames(data_sel_cat)[i]
  
  data_sel <- base_quintas %>% select(sup_hoja, colnames(data_sel_cat)[i])
  
  try(anova <- anova(lm(formula = sup_hoja~., data_sel)), TRUE)
  
  a <- anova$`Pr(>F)`[1]
  
  ifelse(is.numeric(a), a, "NA")
  
  cbind(colnames(data_sel_cat)[i], a, a<0.05)
  
}

resanova <- lapply(2:ncol(data_sel_cat)-1,anov)

resanova <- as.data.frame(do.call("rbind", resanova))

colnames(resanova) <- c("variable","p","sign")

write.table(resanova, "resanova_sup_hoja.txt", sep="\t")
write_excel_csv2(resanova, "resanova_sup_hoja.csv", delim=";")

resanova %>% filter(p< 0.1)

##superficie POR TIPO: fruto
data_sel_cat <- base_quintas %>% select_if(is.character)
data_sel_cat$sup_fruto <- base_quintas$sup_fruto

anov <- function(i){
  
  colnames(data_sel_cat)[i]
  
  data_sel <- base_quintas %>% select(sup_fruto, colnames(data_sel_cat)[i])
  
  try(anova <- anova(lm(formula = sup_fruto~., data_sel)), TRUE)
  
  a <- anova$`Pr(>F)`[1]
  
  ifelse(is.numeric(a), a, "NA")
  
  cbind(colnames(data_sel_cat)[i], a, a<0.05)
  
}

resanova <- lapply(2:ncol(data_sel_cat)-1,anov)

resanova <- as.data.frame(do.call("rbind", resanova))

colnames(resanova) <- c("variable","p","sign")

write.table(resanova, "resanova_sup_fruto.txt", sep="\t")
write_excel_csv2(resanova, "resanova_sup_fruto.csv", delim=";")
resanova %>% filter(p< 0.1)

##superficie POR TIPO: inflo_col
data_sel_cat <- base_quintas %>% select_if(is.character)
data_sel_cat$sup_inflo_col <- base_quintas$sup_inflo_col

anov <- function(i){
  
  colnames(data_sel_cat)[i]
  
  data_sel <- base_quintas %>% select(sup_inflo_col, colnames(data_sel_cat)[i])
  
  try(anova <- anova(lm(formula = sup_inflo_col~., data_sel)), TRUE)
  
  a <- anova$`Pr(>F)`[1]
  
  ifelse(is.numeric(a), a, "NA")
  
  cbind(colnames(data_sel_cat)[i], a, a<0.05)
  
}

resanova <- lapply(2:ncol(data_sel_cat)-1,anov)

resanova <- as.data.frame(do.call("rbind", resanova))

colnames(resanova) <- c("variable","p","sign")

write.table(resanova, "resanova_sup_inflo_col.txt", sep="\t")
write_excel_csv2(resanova, "resanova_sup_inflo_col.csv", delim = ";")
resanova %>% filter(p< 0.1)

##superficie POR TIPO: tuberculo_raiz
data_sel_cat <- base_quintas %>% select_if(is.character)
data_sel_cat$sup_tuberculo_raiz <- base_quintas$sup_tuberculo_raiz

anov <- function(i){
  
  colnames(data_sel_cat)[i]
  
  data_sel <- base_quintas %>% select(sup_tuberculo_raiz, colnames(data_sel_cat)[i])
  
  try(anova <- anova(lm(formula = sup_tuberculo_raiz~., data_sel)), TRUE)
  
  a <- anova$`Pr(>F)`[1]
  
  ifelse(is.numeric(a), a, "NA")
  
  cbind(colnames(data_sel_cat)[i], a, a<0.05)
  
}

resanova <- lapply(2:ncol(data_sel_cat)-1,anov)

resanova <- as.data.frame(do.call("rbind", resanova))

colnames(resanova) <- c("variable","p","sign")

write.table(resanova, "resanova_sup_tuberculo_raiz.txt", sep="\t")
write_excel_csv2(resanova, "resanova_sup_tuberculo_raiz.csv", delim=";")
resanova %>% filter(p< 0.1)

##superficie POR TIPO: tallo_bulbo
data_sel_cat <- base_quintas %>% select_if(is.character)
data_sel_cat$sup_tallo_bulbo <- base_quintas$sup_tallo_bulbo

anov <- function(i){
  
  colnames(data_sel_cat)[i]
  
  data_sel <- base_quintas %>% select(sup_tallo_bulbo, colnames(data_sel_cat)[i])
  
  try(anova <- anova(lm(formula = sup_tallo_bulbo~., data_sel)), TRUE)
  
  a <- anova$`Pr(>F)`[1]
  
  ifelse(is.numeric(a), a, "NA")
  
  cbind(colnames(data_sel_cat)[i], a, a<0.05)
  
}

resanova <- lapply(2:ncol(data_sel_cat)-1,anov)

resanova <- as.data.frame(do.call("rbind", resanova))

colnames(resanova) <- c("variable","p","sign")

write.table(resanova, "resanova_sup_tallo_bulbo.txt", sep="\t")
write_excel_csv2(resanova, "resanova_sup_tallo_bulbo.csv", delim=";")
resanova %>% filter(p< 0.1)

####### recurrencia por TIPO ########

##recurrencia POR TIPO: hoja
data_sel_cat <- base_quintas %>% select_if(is.character)
data_sel_cat$rec_hoja <- base_quintas$rec_hoja

anov <- function(i){
  
  colnames(data_sel_cat)[i]
  
  data_sel <- base_quintas %>% select(rec_hoja, colnames(data_sel_cat)[i])
  
  try(anova <- anova(lm(formula = rec_hoja~., data_sel)), TRUE)
  
  a <- anova$`Pr(>F)`[1]
  
  ifelse(is.numeric(a), a, "NA")
  
  cbind(colnames(data_sel_cat)[i], a, a<0.05)
  
}

resanova <- lapply(2:ncol(data_sel_cat)-1,anov)

resanova <- as.data.frame(do.call("rbind", resanova))

colnames(resanova) <- c("variable","p","sign")

write.table(resanova, "resanova_rec_hoja.txt", sep="\t")
write_excel_csv2(resanova, "resanova_rec_hoja.csv", delim=";")

resanova %>% filter(p< 0.1)

##recurrencia POR TIPO: fruto
data_sel_cat <- base_quintas %>% select_if(is.character)
data_sel_cat$rec_fruto <- base_quintas$rec_fruto

anov <- function(i){
  
  colnames(data_sel_cat)[i]
  
  data_sel <- base_quintas %>% select(rec_fruto, colnames(data_sel_cat)[i])
  
  try(anova <- anova(lm(formula = rec_fruto~., data_sel)), TRUE)
  
  a <- anova$`Pr(>F)`[1]
  
  ifelse(is.numeric(a), a, "NA")
  
  cbind(colnames(data_sel_cat)[i], a, a<0.05)
  
}

resanova <- lapply(2:ncol(data_sel_cat)-1,anov)

resanova <- as.data.frame(do.call("rbind", resanova))

colnames(resanova) <- c("variable","p","sign")

write.table(resanova, "resanova_rec_fruto.txt", sep="\t")
write_excel_csv2(resanova, "resanova_rec_fruto.csv", delim=";")
resanova %>% filter(p< 0.1)

##recurrencia POR TIPO: inflo_col
data_sel_cat <- base_quintas %>% select_if(is.character)
data_sel_cat$rec_inflo_col <- base_quintas$rec_inflo_col

anov <- function(i){
  
  colnames(data_sel_cat)[i]
  
  data_sel <- base_quintas %>% select(rec_inflo_col, colnames(data_sel_cat)[i])
  
  try(anova <- anova(lm(formula = rec_inflo_col~., data_sel)), TRUE)
  
  a <- anova$`Pr(>F)`[1]
  
  ifelse(is.numeric(a), a, "NA")
  
  cbind(colnames(data_sel_cat)[i], a, a<0.05)
  
}

resanova <- lapply(2:ncol(data_sel_cat)-1,anov)

resanova <- as.data.frame(do.call("rbind", resanova))

colnames(resanova) <- c("variable","p","sign")

write.table(resanova, "resanova_rec_inflo_col.txt", sep="\t")
write_excel_csv2(resanova, "resanova_rec_inflo_col.csv", delim = ";")
resanova %>% filter(p< 0.1)

##recurrencia POR TIPO: tuberculo_raiz
data_sel_cat <- base_quintas %>% select_if(is.character)
data_sel_cat$rec_tuberculo_raiz <- base_quintas$rec_tuberculo_raiz

anov <- function(i){
  
  colnames(data_sel_cat)[i]
  
  data_sel <- base_quintas %>% select(rec_tuberculo_raiz, colnames(data_sel_cat)[i])
  
  try(anova <- anova(lm(formula = rec_tuberculo_raiz~., data_sel)), TRUE)
  
  a <- anova$`Pr(>F)`[1]
  
  ifelse(is.numeric(a), a, "NA")
  
  cbind(colnames(data_sel_cat)[i], a, a<0.05)
  
}

resanova <- lapply(2:ncol(data_sel_cat)-1,anov)

resanova <- as.data.frame(do.call("rbind", resanova))

colnames(resanova) <- c("variable","p","sign")

write.table(resanova, "resanova_rec_tuberculo_raiz.txt", sep="\t")
write_excel_csv2(resanova, "resanova_rec_tuberculo_raiz.csv", delim=";")
resanova %>% filter(p< 0.1)

##recurrencia POR TIPO: tallo_bulbo
data_sel_cat <- base_quintas %>% select_if(is.character)
data_sel_cat$rec_tallo_bulbo <- base_quintas$rec_tallo_bulbo

anov <- function(i){
  
  colnames(data_sel_cat)[i]
  
  data_sel <- base_quintas %>% select(rec_tallo_bulbo, colnames(data_sel_cat)[i])
  
  try(anova <- anova(lm(formula = rec_tallo_bulbo~., data_sel)), TRUE)
  
  a <- anova$`Pr(>F)`[1]
  
  ifelse(is.numeric(a), a, "NA")
  
  cbind(colnames(data_sel_cat)[i], a, a<0.05)
  
}

resanova <- lapply(2:ncol(data_sel_cat)-1,anov)

resanova <- as.data.frame(do.call("rbind", resanova))

colnames(resanova) <- c("variable","p","sign")

write.table(resanova, "resanova_rec_tallo_bulbo.txt", sep="\t")
write_excel_csv2(resanova, "resanova_rec_tallo_bulbo.csv", delim=";")
resanova %>% filter(p< 0.1)

##numero de tipos
data_sel_cat <- base_quintas %>% select_if(is.character)
data_sel_cat$no_rows<- base_quintas$no_rows

anov <- function(i){
  
  colnames(data_sel_cat)[i]
  
  data_sel <- base_quintas %>% select(no_rows, colnames(data_sel_cat)[i])
  
  try(anova <- anova(lm(formula = no_rows~., data_sel)), TRUE)
  
  a <- anova$`Pr(>F)`[1]
  
  ifelse(is.numeric(a), a, "NA")
  
  cbind(colnames(data_sel_cat)[i], a, a<0.05)
  
}

resanova <- lapply(2:ncol(data_sel_cat)-1,anov)

resanova <- as.data.frame(do.call("rbind", resanova))

colnames(resanova) <- c("variable","p","sign")

write.table(resanova, "resanova_no_rows.txt", sep="\t")
write_excel_csv2(resanova, "resanova_no_rows.csv", delim=";")
resanova %>% filter(p< 0.1)

