library(dplyr)

quintas_rf <- read.table("data/quintas_rf_dep.txt", header=T, sep = "\t", dec=".")

quintas_rf_d <- quintas_rf %>% select(!c(Productor_a, Congl_abaste))

quintas_rf_dep <- cbind(quintas_rf_d,"Congl_abaste"=as.factor(quintas_rf$Congl_abaste))

library(caret)

fitControl <- trainControl(method='repeatedcv', 
                                      number=5, 
                                      repeats=3)
set.seed(123)

mtry <- sqrt(ncol(quintas_rf_dep))

tunegrid <- expand.grid(.mtry=c(seq(5,ncol(quintas_rf_dep)-1,2 )))

library(randomForest)

rf_def <- train(Congl_abaste~., 
                    data= quintas_rf_dep, 
                    method='rf', 
                    metric='Accuracy',
                    type="classification",
                    tuneGrid=tunegrid, 
                    importance=T,
                    trControl=fitControl
                )

plot(rf_def)

impor <- as.data.frame(importance(rf_def$finalModel))
