library(tidyverse)

base_quintas <- read.table("base_quintas.txt", 
                           header = T, 
                           sep = "\t", 
                           na.strings = "NA")

data_sel_cat <- base_quintas %>% select_if(is.character)

data_sel_cat$prod_media <- base_quintas$prod_media

###variables cat

anov <- function(i){
  
colnames(data_sel_cat)[i]

data_sel <- base_quintas %>% select(prod_media, colnames(data_sel_cat)[i])

try(anova <- anova(lm(formula = prod_media~., data_sel)))

a <- anova$`Pr(>F)`[1]

ifelse(is.numeric(a), a, "NA")

cbind(colnames(data_sel_cat)[i], a, a<0.05)

}

resanova <- lapply(2:ncol(data_sel_cat)-1,anov)

resanova <- as.data.frame(do.call("rbind", resanova))

colnames(resanova) <- c("variable","p","sign")

resanova %>% filter(p< 0.05)



### Variables continuas

install.packages("corrplot")
library(corrplot)

data_sel_num<- base_quintas %>% select_if(is.numeric)

corr <- cor(data_sel_num, use = "pairwise.complete.obs", method = "spearman")

# corrplot(corr, type="upper", #order="hclust", 
#          #p.mat = p.mat, 
#          tl.pos='n',
#          sig.level = 0.05)
# 
# p.mat <- cor.test(data_sel_num, method = "spearman") 


corr0.4 <- as.data.frame(corr) %>% 
  rownames_to_column() %>%
  gather(key="variable", value="correlation", -rowname) %>%
  dplyr::filter(between(abs(correlation), 0.4, 0.99))

correlation, 