#limpeza
rm(list=ls())

#regressão Logistica
#bibliotecas
library(caret)
library(pROC)
library(ISLR)


#carregar dados da biblioteca ISLR
dados2<-ISLR::OJ
names(dados2)
str(dados2)

#gerando aleatório
nrow(dados2)
seq_len(nrow(dados2))
1:nrow(dados2)

#separando em treino e teste
set.seed(123) # reprodutibilidade

particao <- sample(1:nrow(dados2), size = 0.7*nrow(dados2))
dados2treino <- dados2[particao, ]
dados2teste  <- dados2[-particao, ]
prop.table(table(dados2treino$Purchase)); 
prop.table(table(dados2teste$Purchase))

names(dados2treino)

#gerando o modelo
modelologistio01<-glm(Purchase~.,
                      data = dados2treino, 
                      family = binomial)
formula(modelologistio01)
summary(modelologistio01)


#melhorando o modelo
step_backward <- step(modelologistio01, 
                      direction='backward', 
                      trace=0)
modelologistio01<-step_backward
formula(modelologistio01)
summary(modelologistio01)


#predição
prob_modelologistico01<-predict(modelologistio01,
                                newdata = dados2teste, 
                                type = "response")
prob_modelologistico01

# Matriz de confusão e métricas com limiar escolhido")
valordecorte <- 0.5
pred_class <- ifelse(prob_modelologistico01 < valordecorte, "CH", "MM")
tabela_predicao1 <- table(Modelo = pred_class, Real = dados2teste$Purchase)
tabela_predicao1

accuracy <- mean(pred_class == dados2teste$Purchase)
sensitivity <- mean(pred_class[dados2teste$Purchase=="CH"] == "CH")
specificity <- mean(pred_class[dados2teste$Purchase=="MM"]  == "MM")
vetor1<-c(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity)
vetor1

#ROC, AUC e escolha de limiar (método de Youden)
roc_obj <- roc(response = dados2teste$Purchase , 
               predictor = prob_modelologistico01, 
               levels = c("CH","MM"),
               direction = "<")


plot(roc_obj, main = sprintf("ROC - Default (AUC = %.3f)", auc(roc_obj)))
melhor_coordenada<- coords(roc_obj, "best", ret = c("threshold","sensitivity","specificity"))
melhor_coordenada[1]
melhor_coordenada
#melhor coordenanda balanceada
round(roc_obj$sensitivities, digits = 2)==round(roc_obj$specificities, digits = 2)
roc_obj$thresholds[round(roc_obj$sensitivities, digits = 2)==round(roc_obj$specificities, digits = 2)]
which(round(roc_obj$sensitivities, digits = 2)==round(roc_obj$specificities, digits = 2))
melhor_coordenada3<-roc_obj$thresholds[which(round(roc_obj$sensitivities, digits = 2)==round(roc_obj$specificities, digits = 2))]
melhor_coordenada3

#melhor coordenada pensando na melhor acuracia
roc_obj$sensitivities+roc_obj$specificities
which.max(roc_obj$sensitivities+roc_obj$specificities)
melhor_coordenada2<-roc_obj$thresholds[which.max(roc_obj$sensitivities+roc_obj$specificities)]
melhor_coordenada2

#apresentação da busca do melhor valor
plot(roc_obj$thresholds, roc_obj$sensitivities,col="white",lwd=1)
lines(roc_obj$thresholds, roc_obj$specificities, col="red",lwd=2)
lines(roc_obj$thresholds, roc_obj$sensitivities, col="blue",lwd=1)
abline(v=melhor_coordenada3, col="green")
abline(v=melhor_coordenada[1], col="blue")
abline(v=melhor_coordenada2, col="brown")
abline(v=0.5, col="black")

#usando a melhor coordenada/melhor acuracia 
as.numeric(melhor_coordenada[1])
resposta_modelologistico01<-ifelse(prob_modelologistico01 < as.numeric(melhor_coordenada[1]),
                                   "CH",
                                   "MM")
resposta_modelologistico01<-as.factor(resposta_modelologistico01)

#matriz confusão melhor acuracia
tabela_predicao2 <- table(Modelo = resposta_modelologistico01, Real = dados2teste$Purchase)
tabela_predicao2

accuracy <- mean(resposta_modelologistico01 == dados2teste$Purchase)
sensitivity <- mean(resposta_modelologistico01[dados2teste$Purchase=="CH"] == "CH")
specificity <- mean(resposta_modelologistico01[dados2teste$Purchase=="MM"]  == "MM")
vetor2<-c(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity)
vetor2
#usando a coordenada mais balanceada
resposta_modelologistico01<-ifelse(prob_modelologistico01 < melhor_coordenada3,
                                   "CH",
                                   "MM")
resposta_modelologistico01<-as.factor(resposta_modelologistico01)

#matriz confusão melhor acuracia
tabela_predicao3 <- table(Modelo = resposta_modelologistico01, Real = dados2teste$Purchase)
tabela_predicao3

accuracy <- mean(resposta_modelologistico01 == dados2teste$Purchase)
sensitivity <- mean(resposta_modelologistico01[dados2teste$Purchase=="CH"] == "CH")
specificity <- mean(resposta_modelologistico01[dados2teste$Purchase=="MM"]  == "MM")
vetor3<-c(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity)
vetor3

#corte 0,5
vetor1
#corte melhor valor/melhor acuracia
vetor2
#corte mais balanceado
vetor3

