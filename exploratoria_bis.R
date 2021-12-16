library(agricolae)
library(tidyverse)

base_quintas <- read.table("base_quintas1.txt", 
                           header = T, 
                           sep = ";", 
                           na.strings = "NA",
                           dec = ",")

data_sel_num<- base_quintas %>% select_if(is.numeric) %>% 
  select(!'Medieria..Cantidad.argentinos')
data_sel_num$Congl_abaste <- as.factor(data_sel_num$Congl_abaste)

summary(data_sel_num)

anov <- function(i){
  
  colnames(data_sel_num)[i]
  
  data_sel <- base_quintas %>% select(Congl_abaste, colnames(data_sel_num)[i])
  
  form <- as.formula(paste(colnames(data_sel_num)[i],"Congl_abaste", sep = " ~ "))
  try(mod <- lm(formula = form, 
     data_sel_num))
  
  try(anova <- anova(mod)
      ,TRUE)
  
  a <- anova$`Pr(>F)`[1]
  
  ifelse(is.numeric(a), a, "NA")
  
  cbind(colnames(data_sel_num)[i], a, a<0.05)

  LSD<-LSD.test(mod,"Congl_abaste", 
           console = TRUE,
           alpha=0.1)
  LSD$groups
  add_rownames(LSD$groups, var="Congl")
  
  #ifelse(is.data.frame(LSD$groups), as.data.frame(LSD$groups), "NA")
}

#do.call("cbind",lapply(2:20,anov))

resanova <- lapply(2:ncol(data_sel_num)-1,anov)

#reduce(list(resanova, .id='Congl', all=T), merge)


resanova <- as.data.frame(do.call("bind_rows", 
                                  c(resanova#, .id='Congl'
                                    )))

resanova_forra <- bind_cols(resanova[[2]],resanova[[3]],
      resanova[[14]],
      resanova[[15]],
      resanova[[16]],resanova[[17]],
      #resanova[[19]],
      resanova[[25]],
      resanova[[26]],resanova[[27]],
      resanova[[28]],resanova[[29]],
      resanova[[31]],resanova[[50]],
      resanova[[35]],resanova[[51]],
      resanova[[38]],resanova[[52]],
      resanova[[49]],resanova[[53]],
      resanova[[54]],resanova[[55]],
      resanova[[57]],resanova[[58]],
      resanova[[59]],resanova[[60]] ,
      .id="Congl"#,all =  T,incomparables=TRUE
      )

write.table(resanova_forra, "resanova_congl_abas.txt", sep="\t", dec = ".")
write_excel_csv2(resanova, "resanova_congl_abas.csv", delim=";")

