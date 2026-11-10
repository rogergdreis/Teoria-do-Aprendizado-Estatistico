#limpar ambiente---------------------------------------------------------
rm(list = ls())

#bibliotecas-------------------------------------------------------------
library(ISLR2)
library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)
library(pROC)


#carregar dados----------------------------------------------------------

#semente - ponto de partida
set.seed(123)

#carregar dados
dados001<-ISLR::Default
dados001$default <- as.factor(dados001$default)   # variável-alvo (vivo/morto)
dim(dados001)

nrow(dados001)
ncol(dados001)

#desprezando valores NA
dados001<-na.omit(dados001)
dim(dados001)

#treino e teste -------------------------------------------------
particao001<-sample(1:nrow(dados001), size = 0.7*nrow(dados001), replace = F)
treino001 <- dados001[particao001, ]
teste001  <- dados001[-particao001, ]

#modelo-------------------------------------------------------
modelo001_rf <- randomForest(
  default ~ ., 
  data = treino001,
  ntree = 100,          # número de árvores
  mtry = floor(sqrt(ncol(treino001)-1)),  # nº de variáveis por split
  importance = TRUE
)
ncol(treino001)


modelo001_rf

modelo001_rf$err.rate[100,"OOB"]

#avaliar desempenho -----------------------------------------------------
predicao001 <- predict(modelo001_rf, newdata = teste001, type = "class")
matrizconfusao001 <- confusionMatrix(predicao001, teste001$default)
print(matrizconfusao001)

predicao001
matrizconfusao001$overall
matrizconfusao001$overall['Accuracy']


#rotina para entendermos o melhor modelo ---------------------------------
mtry_usados<- ncol(treino001)-1
ntrees <- c(100,250, 500, 1000,1500)
oob <- numeric(length(ntrees)*mtry_usados)
acuracia<-numeric(length(ntrees)*mtry_usados)
vetorntrees<-numeric(length(ntrees)*mtry_usados)
vetormtry<-numeric(length(ntrees)*mtry_usados)
vetormelhorponto<-numeric(length(ntrees)*mtry_usados)
acuraciaroc<-numeric(length(ntrees)*mtry_usados)
espsen<-numeric(length(ntrees)*mtry_usados)
a<-length(ntrees)*mtry_usados
modelos <- vector("list",a ) 
vetoraucrf <- vector("list",a )


j<-as.numeric(length(ntrees))
k<-mtry_usados
n<-1
i<-1
m<-1
dados001$default
for (i in 1:j) {
  for (m in 1:k) {
    #n<-2
    #i<-2
    #m<-2
    vetorntrees[n]<-ntrees[i]
    vetormtry[n]<-m
    set.seed(123)
    tmp <- randomForest(default ~ ., 
                        data = treino001, 
                        ntree = ntrees[i], 
                        mtry = m)
    modelos[[n]] <-tmp
    oob[n] <- tmp$err.rate[ntrees[i], "OOB"]
    predicao001t <- predict(tmp, newdata = teste001)
    matrizconfusao001t <- confusionMatrix(predicao001t, teste001$default)
    acuracia[n]<-matrizconfusao001t$overall['Accuracy']
    #youdden
    set.seed(123)
    predicao001roc <- predict(tmp, newdata = teste001, type ="prob")
    rprobparacatg<-as.factor(ifelse(predicao001roc[,2]>0.5,"Yes","No"))
    matrizconfusao001roc <- confusionMatrix(rprobparacatg, 
                                            teste001$default, 
                                            positive="Yes")
    #Curva roc e AUC
    set.seed(123)
    aucrf<-roc(teste001$default,predicao001roc[,2])
    vetoraucrf[[n]]<-aucrf
    order(aucrf$sensitivities+aucrf$specificities, decreasing =T )[1]
    melhorponto<-aucrf$thresholds[order(aucrf$sensitivities+aucrf$specificities, decreasing =T)[1]]
    #utilizando o melhor ponte de corte
    rprobparacatg<-as.factor(ifelse(predicao001roc[,2]>melhorponto,"Yes","No"))
    matrizconfusao001roc1 <- confusionMatrix(rprobparacatg, teste001$default, positive = "Yes")
    vetormelhorponto[n]<-melhorponto
    acuraciaroc[n]<-matrizconfusao001roc1$overall['Accuracy']
    a<-order(aucrf$sensitivities+aucrf$specificities, decreasing =T )[1]
    espsen[n]<-aucrf$sensitivities[a]+aucrf$specificities[a]
    #contador n
    n<-n+1
  }
}
is.list(modelos)
# dataset do melhor modelo ------------------------------------------------
informacao_rf<-data.frame(NTREES = vetorntrees, 
                          MTRY = vetormtry,
                          OOB = oob,
                          ACURACIA = acuracia,
                          ACURACIAROC=acuraciaroc,
                          MELHORPONTO=vetormelhorponto,
                          ESPECI_SENSIBI=espsen)


informacao_rf


# graficos ----------------------------------------------------------------
#As 10 melhores especificidade+sensibilidade
order(informacao_rf$ESPECI_SENSIBI, decreasing = T)
informacao_rf[order(informacao_rf$ESPECI_SENSIBI, decreasing = T)[1:10],]

modelos[[13]]

#As 10 melhores 
order(informacao_rf$ACURACIAROC, decreasing = T)
informacao_rf[order(informacao_rf$ACURACIAROC, decreasing = T)[1:10],]

#Os 10 melhores OOB
order(informacao_rf$OOB, decreasing = F)
informacao_rf[order(informacao_rf$OOB, decreasing = F)[1:10],]

#grafico mtry X Acuracia
ggplot(informacao_rf, aes(x = MTRY, 
                          y = ACURACIAROC,
                          colour = as.factor(NTREES),
                          label = round(OOB,2),
                          size= round(OOB,2)
)) +
  geom_point() + 
  geom_line(size=1)+
  geom_text(size = 4, vjust = -1.5) +         # Adiciona textos acima dos pontos
  labs(title = "Acuraciaroc vs mtry", x = "mtry", y = "AcuraciaROC",color="ntress" )

#mtry x OOB
ggplot(informacao_rf, aes(x = MTRY, 
                          y = OOB, 
                          label = round(ACURACIAROC,2),
                          colour = as.factor(NTREES),
                          size = round(ACURACIA,2)
)) +
  geom_line(size=1) + 
  geom_point() +  
  geom_text(size = 4, vjust = -1.5) +         # Adiciona textos acima dos pontos
  labs(title = "Erro OOB vs mtry", 
       x = "mtry", 
       y = "OOB", 
       color="ntress",
       size="Acuraciaroc")

#mtry x OOB
ggplot(informacao_rf, aes(x = NTREES, 
                          y = OOB, 
                          label = round(ACURACIAROC,2),
                          colour = as.factor(MTRY),
                          size = ACURACIAROC
)) +
  geom_line(size=1) + 
  geom_point() +  
  geom_text(size = 4, vjust = -1.5) +         # Adiciona textos acima dos pontos
  labs(title = "Erro OOB vs ntrees", x = "ntrees", y = "OOB", color="mtry")


#modelo escolhido-------------------------------------------------------
set.seed(123)
modelo001_final<-modelos[[13]]
melhorponto<-informacao_rf$MELHORPONTO[13]
modelo001_final
modelo001_final$err.rate[500,1]
predicao001roc <- predict(tmp, newdata = teste001, type ="prob")
predicao001final <- predict(modelo001_final, newdata = teste001,type="prob")
rprobparacatg<-as.factor(ifelse(predicao001final[,2]>melhorponto,"Yes","No"))

matrizconfusao002t <- confusionMatrix(rprobparacatg, teste001$default, positive = "Yes")
matrizconfusao002t$overall['Accuracy']