library(tidyverse)
library(corrplot)

base_quintas <- read.table("base_quintas.txt", 
                           header = T, 
                           sep = "\t", 
                           na.strings = "NA")

## Variables continuas_CORRELACION

data_sel_num<- base_quintas %>% select_if(is.numeric)
#colnames(data_sel_num)

data_sel_num<- base_quintas %>% select("Quinta..Superficie.Total",                                  
                                       "Quinta..Superficie.Hortícola",                              
                                       "Quinta..Superficie.cubierta..ha.",                          
                                       "Quinta..Superficie.con.invernaderos",                       
                                       "Quinta..Superficie.con.malla.antigranizo",                  
                                       "Quinta..Superficie.con.mediasombra",                        
                                       "Intalaciones_nro",                                        
                                       "Producción..Cantidad.de.especies",                      
                                       "Agua.para.riego..Superficie...Gravedad",                    
                                       "Agua.para.riego..Superficie...Goteo",                       
                                       "Agua.para.riego..Superficie...Aspersión",                   
                                       "Practicas.productivas_nro",                                 
                                       "Productor.principal..Edad",                                 
                                       "Familia.del.PP..Cantidad",                                  
                                       "Familia.del.PP..Cantidad.varones",                          
                                       "Familia.del.PP..Cantidad.vujeres",                          
                                       "Familia.del.PP..Cantidad.argentinos.as",                    
                                       "Familia.del.PP..Cantidad.bolivianos.as",                
                                       "Medieria..Cantidad",                                        
                                       "Medieria..Cantidad.varones",                                
                                       "Medieria..Cantidad.mujeres",                                
                                       "Medieria..Cantidad.argentinos",                             
                                       "Medieria..Cantidad.bolivia",                                
                                       "Peones..Cantidad",                                       
                                       "Trabajo.extra.predial..sector.horticola",                   
                                       "Trabajo.extra.predial..sector.agropecuario.no.hortícola",   
                                       "Trabajo.extra.predial..sector.no.agropecuario",             
                                       "Contrata.prepeación.de.la.tierra..Cantidad.de.días",        
                                       "Contrata.prepeación.de.la.tierra..Cantidad.de.personas",    
                                       "Siembra.y.riego..Cantidad.de.días",                         
                                       "Siembra.y.riego..Cantidad.de.personas",                     
                                       "Contrata.cosecha..acondic.y.embalaje..Cantidad.de.días",    
                                       "Contrata.cosecha..acondic.y.embalaje..Cantidad.de.personas",
                                       "Respondente..Experiencia.agrícola..años.",                  
                                       "Respondente..Ingreso.a.quinta..años.",                      
                                       "vur_2020_mean",                                             
                                       "Abasto_distancia..m.",                                      
                                       "SanMiguel_distancia..m.",                                   
                                       "prod_media",                                                
                                       "prod_hoja",                                                 
                                       "prod_fruto",                                                
                                       "prod_tallo_bulbo",                                          
                                       "prod_tuberculo_raiz",                                       
                                       "prod_inflo_col",                                            
                                       "no_rows",                                                   
                                       "sup_hoja",                                                  
                                       "sup_fruto",                                                 
                                       "sup_tallo_bulbo",                                           
                                       "sup_tuberculo_raiz",                                        
                                       "sup_inflo_col",                                             
                                       "rec_hoja",                                                  
                                       "rec_fruto",                                                 
                                       "rec_tallo_bulbo",                                           
                                       "rec_tuberculo_raiz",                                        
                                       "rec_inflo_col"               
)

corr <- cor(data_sel_num, use = "pairwise.complete.obs", method = "spearman")

# corrplot(corr, type="upper", 
#          order="alphabet", 
#          #p.mat = p.mat, 
#          tl.pos='n',
#          #p.mat = corr,
#          sig.level = 0.05)

# p.mat <- cor.test(data_sel_num, method = "spearman") 


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


write_excel_csv2(corr0.4, "corr0.4.csv", delim=';')

###variables categoricas

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

resanova <- lapply(2:ncol(data_sel_cat)-1,anov)

resanova <- as.data.frame(do.call("rbind", resanova))

colnames(resanova) <- c("variable","p","sign")

write.table(resanova, "resanova_prod.txt", sep="\t")

resanova %>% filter(p< 0.1)

