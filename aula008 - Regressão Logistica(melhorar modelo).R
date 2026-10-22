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


# Exercícios
# Construir a regressão logistica da base:
# a) ISLR::Default, definindo como varíavel dependente student
# Carregar dados
dados_a <- Default
names(dados_a)
str(dados_a)

#gerando aleatório
nrow(dados_a)
seq_len(nrow(dados_a))
1:nrow(dados_a)

#separando em treino e teste
set.seed(123) # reprodutibilidade

particao_a <- sample(1:nrow(dados_a), size = 0.7*nrow(dados_a))
dados_treino_a <- dados_a[particao_a, ]
dados_teste_a  <- dados_a[-particao_a, ]
prop.table(table(dados_treino_a$student)); 
prop.table(table(dados_teste_a$student))

names(dados_treino_a)

#gerando o modelo
modelologistico_a<-glm(student~.,
                      data = dados_treino_a, 
                      family = binomial)
formula(modelologistico_a)
summary(modelologistico_a)


#melhorando o modelo
step_backward_a <- step(modelologistico_a, 
                      direction='backward', 
                      trace=0)
modelologistico_a <- step_backward_a
formula(modelologistico_a)
summary(modelologistico_a)


#predição
prob_modelologistico_a<-predict(modelologistico_a,
                                newdata = dados_teste_a, 
                                type = "response")
prob_modelologistico_a

# Matriz de confusão e métricas com limiar escolhido")
valordecorte_a <- 0.5
pred_class_a <- ifelse(prob_modelologistico_a < valordecorte_a, "No", "Yes")
tabela_predicao_a1 <- table(Modelo = pred_class_a, Real = dados_teste_a$student)
tabela_predicao_a1

accuracy_a <- mean(pred_class_a == dados_teste_a$student)
sensitivity_a <- mean(pred_class_a[dados_teste_a$student=="Yes"] == "Yes")
specificity_a <- mean(pred_class_a[dados_teste_a$student=="No"]  == "No")
vetor_a1<-c(accuracy = accuracy_a, sensitivity = sensitivity_a, specificity = specificity_a)
vetor_a1

#ROC, AUC e escolha de limiar (método de Youden)
roc_obj_a <- roc(response = dados_teste_a$student , 
               predictor = prob_modelologistico_a, 
               levels = c("No","Yes"),
               direction = "<")


plot(roc_obj_a, main = sprintf("ROC - Default (AUC = %.3f)", auc(roc_obj_a)))
melhor_coordenada_a1<- coords(roc_obj_a, "best", ret = c("threshold","sensitivity","specificity"))
melhor_coordenada_a1[1]
melhor_coordenada_a1

#melhor coordenanda balanceada
round(roc_obj_a$sensitivities, digits = 2)==round(roc_obj_a$specificities, digits = 2)
roc_obj_a$thresholds[round(roc_obj_a$sensitivities, digits = 2)==round(roc_obj_a$specificities, digits = 2)]
which(round(roc_obj_a$sensitivities, digits = 2)==round(roc_obj_a$specificities, digits = 2))
melhor_coordenada_a3<-roc_obj_a$thresholds[which(round(roc_obj_a$sensitivities, digits = 2)==round(roc_obj_a$specificities, digits = 2))]
melhor_coordenada_a3

#melhor coordenada pensando na melhor acuracia
roc_obj_a$sensitivities+roc_obj_a$specificities
which.max(roc_obj_a$sensitivities+roc_obj_a$specificities)
melhor_coordenada_a2<-roc_obj_a$thresholds[which.max(roc_obj_a$sensitivities+roc_obj_a$specificities)]
melhor_coordenada_a2

#apresentação da busca do melhor valor
plot(roc_obj_a$thresholds, roc_obj_a$sensitivities,col="white",lwd=1)
lines(roc_obj_a$thresholds, roc_obj_a$specificities, col="red",lwd=2)
lines(roc_obj_a$thresholds, roc_obj_a$sensitivities, col="blue",lwd=1)
abline(v=melhor_coordenada_a3, col="green")
abline(v=melhor_coordenada_a1[1], col="blue")
abline(v=melhor_coordenada_a2, col="brown")
abline(v=0.5, col="black")

#usando a melhor coordenada/melhor acuracia 
as.numeric(melhor_coordenada_a1[1])
resposta_modelologistico_a<-ifelse(prob_modelologistico_a < as.numeric(melhor_coordenada_a1[1]),
                                   "No",
                                   "Yes")
resposta_modelologistico_a<-as.factor(resposta_modelologistico_a)

#matriz confusão melhor acuracia
tabela_predicao_a2 <- table(Modelo = resposta_modelologistico_a, Real = dados_teste_a$student)
tabela_predicao_a2

accuracy_a <- mean(resposta_modelologistico_a == dados_teste_a$student)
sensitivity_a <- mean(resposta_modelologistico_a[dados_teste_a$student=="Yes"] == "Yes")
specificity_a <- mean(resposta_modelologistico_a[dados_teste_a$student=="No"]  == "No")
vetor_a2<-c(accuracy = accuracy_a, sensitivity = sensitivity_a, specificity = specificity_a)
vetor_a2
#usando a coordenada mais balanceada
resposta_modelologistico_a<-ifelse(prob_modelologistico_a < melhor_coordenada_a3,
                                   "No",
                                   "Yes")
resposta_modelologistico_a<-as.factor(resposta_modelologistico_a)

#matriz confusão melhor acuracia
tabela_predicao_a3 <- table(Modelo = resposta_modelologistico_a, Real = dados_teste_a$student)
tabela_predicao_a3

accuracy_a <- mean(resposta_modelologistico_a == dados_teste_a$student)
sensitivity_a <- mean(resposta_modelologistico_a[dados_teste_a$student=="Yes"] == "Yes")
specificity_a <- mean(resposta_modelologistico_a[dados_teste_a$student=="No"]  == "No")
vetor_a3<-c(accuracy = accuracy_a, sensitivity = sensitivity_a, specificity = specificity_a)
vetor_a3

#corte 0,5
vetor_a1
#corte melhor valor/melhor acuracia
vetor_a2
#corte mais balanceado
vetor_a3

# b)ISLR2::Smarket, definindo como variavel dependente Direction
# Carregar dados
dados_b <- Smarket
names(dados_b)
str(dados_b)

#gerando aleatório
nrow(dados_b)
seq_len(nrow(dados_b))
1:nrow(dados_b)

#separando em treino e teste
set.seed(123) # reprodutibilidade

particao_b <- sample(1:nrow(dados_b), size = 0.7*nrow(dados_b))
dados_treino_b <- dados_b[particao_b, ]
dados_teste_b  <- dados_b[-particao_b, ]
prop.table(table(dados_treino_b$Direction)); 
prop.table(table(dados_teste_b$Direction))

names(dados_treino_b)

#gerando o modelo
modelologistico_b<-glm(Direction~.,
                       data = dados_treino_b, 
                       family = binomial)
formula(modelologistico_b)
summary(modelologistico_b)


#melhorando o modelo
step_backward_b <- step(modelologistico_b, 
                        direction='backward', 
                        trace=0)
modelologistico_b <- step_backward_b
formula(modelologistico_b)
summary(modelologistico_b)


#predição
prob_modelologistico_b<-predict(modelologistico_b,
                                newdata = dados_teste_b, 
                                type = "response")
prob_modelologistico_b

# Matriz de confusão e métricas com limiar escolhido")
valordecorte_b <- 0.5
pred_class_b <- ifelse(prob_modelologistico_b < valordecorte_b, "Down", "Up")
tabela_predicao_b1 <- table(Modelo = pred_class_b, Real = dados_teste_b$Direction)
tabela_predicao_b1

accuracy_b <- mean(pred_class_b == dados_teste_b$Direction)
sensitivity_b <- mean(pred_class_b[dados_teste_b$Direction=="Up"] == "Up")
specificity_b <- mean(pred_class_b[dados_teste_b$Direction=="Down"]  == "Down")
vetor_b1<-c(accuracy = accuracy_b, sensitivity = sensitivity_b, specificity = specificity_b)
vetor_b1

#ROC, AUC e escolha de limiar (método de Youden)
roc_obj_b <- roc(response = dados_teste_b$Direction , 
                 predictor = prob_modelologistico_b, 
                 levels = c("Down","Up"),
                 direction = "<")


plot(roc_obj_b, main = sprintf("ROC - Default (AUC = %.3f)", auc(roc_obj_b)))
melhor_coordenada_b1<- coords(roc_obj_b, "best", ret = c("threshold","sensitivity","specificity"))
melhor_coordenada_b1[1]
melhor_coordenada_b1

#melhor coordenanda balanceada
round(roc_obj_b$sensitivities, digits = 2)==round(roc_obj_b$specificities, digits = 2)
roc_obj_b$thresholds[round(roc_obj_b$sensitivities, digits = 2)==round(roc_obj_b$specificities, digits = 2)]
which(round(roc_obj_b$sensitivities, digits = 2)==round(roc_obj_b$specificities, digits = 2))
melhor_coordenada_b3<-roc_obj_b$thresholds[which(round(roc_obj_b$sensitivities, digits = 2)==round(roc_obj_b$specificities, digits = 2))]
melhor_coordenada_b3

#melhor coordenada pensando na melhor acuracia
roc_obj_b$sensitivities+roc_obj_b$specificities
which.max(roc_obj_b$sensitivities+roc_obj_b$specificities)
melhor_coordenada_b2<-roc_obj_b$thresholds[which.max(roc_obj_b$sensitivities+roc_obj_b$specificities)]
melhor_coordenada_b2

#apresentação da busca do melhor valor
plot(roc_obj_b$thresholds, roc_obj_b$sensitivities,col="white",lwd=1)
lines(roc_obj_b$thresholds, roc_obj_b$specificities, col="red",lwd=2)
lines(roc_obj_b$thresholds, roc_obj_b$sensitivities, col="blue",lwd=1)
abline(v=melhor_coordenada_b3, col="green")
abline(v=melhor_coordenada_b1[1], col="blue")
abline(v=melhor_coordenada_b2, col="brown")
abline(v=0.5, col="black")

#usando a melhor coordenada/melhor acuracia 
as.numeric(melhor_coordenada_b1[1])
resposta_modelologistico_b<-ifelse(prob_modelologistico_b < as.numeric(melhor_coordenada_b1[1]),
                                   "Down",
                                   "Up")
resposta_modelologistico_b<-as.factor(resposta_modelologistico_b)

#matriz confusão melhor acuracia
tabela_predicao_b2 <- table(Modelo = resposta_modelologistico_b, Real = dados_teste_b$Direction)
tabela_predicao_b2

accuracy_b <- mean(resposta_modelologistico_b == dados_teste_b$Direction)
sensitivity_b <- mean(resposta_modelologistico_b[dados_teste_b$Direction=="Up"] == "Up")
specificity_b <- mean(resposta_modelologistico_b[dados_teste_b$Direction=="Down"]  == "Down")
vetor_b2<-c(accuracy = accuracy_b, sensitivity = sensitivity_b, specificity = specificity_b)
vetor_b2
#usando a coordenada mais balanceada
resposta_modelologistico_b<-ifelse(prob_modelologistico_b < melhor_coordenada_b3,
                                   "Down",
                                   "Up")
resposta_modelologistico_b<-as.factor(resposta_modelologistico_b)

#matriz confusão melhor acuracia
tabela_predicao_b3 <- table(Modelo = resposta_modelologistico_b, Real = dados_teste_b$Direction)
tabela_predicao_b3

accuracy_b <- mean(resposta_modelologistico_b == dados_teste_b$Direction)
sensitivity_b <- mean(resposta_modelologistico_b[dados_teste_b$Direction=="Up"] == "Up")
specificity_b <- mean(resposta_modelologistico_b[dados_teste_b$Direction=="Down"]  == "Down")
vetor_b3<-c(accuracy = accuracy_b, sensitivity = sensitivity_b, specificity = specificity_b)
vetor_b3

#corte 0,5
vetor_b1
#corte melhor valor/melhor acuracia
vetor_b2
#corte mais balanceado
vetor_b3

# c) ISLR2::Weekly, definindo como variavel dependente Direction
# Carregar dados
dados_c <- Weekly
names(dados_c)
str(dados_c)

#gerando aleatório
nrow(dados_c)
seq_len(nrow(dados_c))
1:nrow(dados_c)

#separando em treino e teste
set.seed(123) # reprodutibilidade

particao_c <- sample(1:nrow(dados_c), size = 0.7*nrow(dados_c))
dados_treino_c <- dados_c[particao_c, ]
dados_teste_c  <- dados_c[-particao_c, ]
prop.table(table(dados_treino_c$Direction)); 
prop.table(table(dados_teste_c$Direction))

names(dados_treino_c)

#gerando o modelo
modelologistico_c<-glm(Direction~.,
                       data = dados_treino_c, 
                       family = binomial)
formula(modelologistico_c)
summary(modelologistico_c)


#melhorando o modelo
step_backward_c <- step(modelologistico_c, 
                        direction='backward', 
                        trace=0)
modelologistico_c <- step_backward_c
formula(modelologistico_c)
summary(modelologistico_c)


#predição
prob_modelologistico_c<-predict(modelologistico_c,
                                newdata = dados_teste_c, 
                                type = "response")
prob_modelologistico_c

# Matriz de confusão e métricas com limiar escolhido")
valordecorte_c <- 0.5
pred_class_c <- ifelse(prob_modelologistico_c < valordecorte_c, "Down", "Up")
tabela_predicao_c1 <- table(Modelo = pred_class_c, Real = dados_teste_c$Direction)
tabela_predicao_c1

accuracy_c <- mean(pred_class_c == dados_teste_c$Direction)
sensitivity_c <- mean(pred_class_c[dados_teste_c$Direction=="Up"] == "Up")
specificity_c <- mean(pred_class_c[dados_teste_c$Direction=="Down"]  == "Down")
vetor_c1<-c(accuracy = accuracy_c, sensitivity = sensitivity_c, specificity = specificity_c)
vetor_c1

#ROC, AUC e escolha de limiar (método de Youden)
roc_obj_c <- roc(response = dados_teste_c$Direction , 
                 predictor = prob_modelologistico_c, 
                 levels = c("Down","Up"),
                 direction = "<")


plot(roc_obj_c, main = sprintf("ROC - Default (AUC = %.3f)", auc(roc_obj_c)))
melhor_coordenada_c1<- coords(roc_obj_c, "best", ret = c("threshold","sensitivity","specificity"))
melhor_coordenada_c1[1]
melhor_coordenada_c1

#melhor coordenanda balanceada
round(roc_obj_c$sensitivities, digits = 2)==round(roc_obj_c$specificities, digits = 2)
roc_obj_c$thresholds[round(roc_obj_c$sensitivities, digits = 2)==round(roc_obj_c$specificities, digits = 2)]
which(round(roc_obj_c$sensitivities, digits = 2)==round(roc_obj_c$specificities, digits = 2))
melhor_coordenada_c3<-roc_obj_c$thresholds[which(round(roc_obj_c$sensitivities, digits = 2)==round(roc_obj_c$specificities, digits = 2))]
melhor_coordenada_c3

#melhor coordenada pensando na melhor acuracia
roc_obj_c$sensitivities+roc_obj_c$specificities
which.max(roc_obj_c$sensitivities+roc_obj_c$specificities)
melhor_coordenada_c2<-roc_obj_c$thresholds[which.max(roc_obj_c$sensitivities+roc_obj_c$specificities)]
melhor_coordenada_c2

#apresentação da busca do melhor valor
plot(roc_obj_c$thresholds, roc_obj_c$sensitivities,col="white",lwd=1)
lines(roc_obj_c$thresholds, roc_obj_c$specificities, col="red",lwd=2)
lines(roc_obj_c$thresholds, roc_obj_c$sensitivities, col="blue",lwd=1)
abline(v=melhor_coordenada_c3, col="green")
abline(v=melhor_coordenada_c1[1], col="blue")
abline(v=melhor_coordenada_c2, col="brown")
abline(v=0.5, col="black")

#usando a melhor coordenada/melhor acuracia 
as.numeric(melhor_coordenada_c1[1])
resposta_modelologistico_c<-ifelse(prob_modelologistico_c < as.numeric(melhor_coordenada_c1[1]),
                                   "Down",
                                   "Up")
resposta_modelologistico_c<-as.factor(resposta_modelologistico_c)

#matriz confusão melhor acuracia
tabela_predicao_c2 <- table(Modelo = resposta_modelologistico_c, Real = dados_teste_c$Direction)
tabela_predicao_c2

accuracy_c <- mean(resposta_modelologistico_c == dados_teste_c$Direction)
sensitivity_c <- mean(resposta_modelologistico_c[dados_teste_c$Direction=="Up"] == "Up")
specificity_c <- mean(resposta_modelologistico_c[dados_teste_c$Direction=="Down"]  == "Down")
vetor_c2<-c(accuracy = accuracy_c, sensitivity = sensitivity_c, specificity = specificity_c)
vetor_c2
#usando a coordenada mais balanceada
resposta_modelologistico_c<-ifelse(prob_modelologistico_c < melhor_coordenada_c3,
                                   "Down",
                                   "Up")
resposta_modelologistico_c<-as.factor(resposta_modelologistico_c)

#matriz confusão melhor acuracia
tabela_predicao_c3 <- table(Modelo = resposta_modelologistico_c, Real = dados_teste_c$Direction)
tabela_predicao_c3

accuracy_c <- mean(resposta_modelologistico_c == dados_teste_c$Direction)
sensitivity_c <- mean(resposta_modelologistico_c[dados_teste_c$Direction=="Up"] == "Up")
specificity_c <- mean(resposta_modelologistico_c[dados_teste_c$Direction=="Down"]  == "Down")
vetor_c3<-c(accuracy = accuracy_c, sensitivity = sensitivity_c, specificity = specificity_c)
vetor_c3

#corte 0,5
vetor_c1
#corte melhor valor/melhor acuracia
vetor_c2
#corte mais balanceado
vetor_c3

# d) ISLR2::Caravan, definindo como variavel dependente Purchase
# Carregar dados
dados_d <- Caravan
names(dados_d)
str(dados_d)

#gerando aleatório
nrow(dados_c)
seq_len(nrow(dados_c))
1:nrow(dados_c)

#separando em treino e teste
set.seed(123) # reprodutibilidade

particao_c <- sample(1:nrow(dados_c), size = 0.7*nrow(dados_c))
dados_treino_c <- dados_c[particao_c, ]
dados_teste_c  <- dados_c[-particao_c, ]
prop.table(table(dados_treino_c$Direction)); 
prop.table(table(dados_teste_c$Direction))

names(dados_treino_c)

#gerando o modelo
modelologistico_c<-glm(Direction~.,
                       data = dados_treino_c, 
                       family = binomial)
formula(modelologistico_c)
summary(modelologistico_c)


#melhorando o modelo
step_backward_c <- step(modelologistico_c, 
                        direction='backward', 
                        trace=0)
modelologistico_c <- step_backward_c
formula(modelologistico_c)
summary(modelologistico_c)


#predição
prob_modelologistico_c<-predict(modelologistico_c,
                                newdata = dados_teste_c, 
                                type = "response")
prob_modelologistico_c

# Matriz de confusão e métricas com limiar escolhido")
valordecorte_c <- 0.5
pred_class_c <- ifelse(prob_modelologistico_c < valordecorte_c, "Down", "Up")
tabela_predicao_c1 <- table(Modelo = pred_class_c, Real = dados_teste_c$Direction)
tabela_predicao_c1

accuracy_c <- mean(pred_class_c == dados_teste_c$Direction)
sensitivity_c <- mean(pred_class_c[dados_teste_c$Direction=="Up"] == "Up")
specificity_c <- mean(pred_class_c[dados_teste_c$Direction=="Down"]  == "Down")
vetor_c1<-c(accuracy = accuracy_c, sensitivity = sensitivity_c, specificity = specificity_c)
vetor_c1

#ROC, AUC e escolha de limiar (método de Youden)
roc_obj_c <- roc(response = dados_teste_c$Direction , 
                 predictor = prob_modelologistico_c, 
                 levels = c("Down","Up"),
                 direction = "<")


plot(roc_obj_c, main = sprintf("ROC - Default (AUC = %.3f)", auc(roc_obj_c)))
melhor_coordenada_c1<- coords(roc_obj_c, "best", ret = c("threshold","sensitivity","specificity"))
melhor_coordenada_c1[1]
melhor_coordenada_c1

#melhor coordenanda balanceada
round(roc_obj_c$sensitivities, digits = 2)==round(roc_obj_c$specificities, digits = 2)
roc_obj_c$thresholds[round(roc_obj_c$sensitivities, digits = 2)==round(roc_obj_c$specificities, digits = 2)]
which(round(roc_obj_c$sensitivities, digits = 2)==round(roc_obj_c$specificities, digits = 2))
melhor_coordenada_c3<-roc_obj_c$thresholds[which(round(roc_obj_c$sensitivities, digits = 2)==round(roc_obj_c$specificities, digits = 2))]
melhor_coordenada_c3

#melhor coordenada pensando na melhor acuracia
roc_obj_c$sensitivities+roc_obj_c$specificities
which.max(roc_obj_c$sensitivities+roc_obj_c$specificities)
melhor_coordenada_c2<-roc_obj_c$thresholds[which.max(roc_obj_c$sensitivities+roc_obj_c$specificities)]
melhor_coordenada_c2

#apresentação da busca do melhor valor
plot(roc_obj_c$thresholds, roc_obj_c$sensitivities,col="white",lwd=1)
lines(roc_obj_c$thresholds, roc_obj_c$specificities, col="red",lwd=2)
lines(roc_obj_c$thresholds, roc_obj_c$sensitivities, col="blue",lwd=1)
abline(v=melhor_coordenada_c3, col="green")
abline(v=melhor_coordenada_c1[1], col="blue")
abline(v=melhor_coordenada_c2, col="brown")
abline(v=0.5, col="black")

#usando a melhor coordenada/melhor acuracia 
as.numeric(melhor_coordenada_c1[1])
resposta_modelologistico_c<-ifelse(prob_modelologistico_c < as.numeric(melhor_coordenada_c1[1]),
                                   "Down",
                                   "Up")
resposta_modelologistico_c<-as.factor(resposta_modelologistico_c)

#matriz confusão melhor acuracia
tabela_predicao_c2 <- table(Modelo = resposta_modelologistico_c, Real = dados_teste_c$Direction)
tabela_predicao_c2

accuracy_c <- mean(resposta_modelologistico_c == dados_teste_c$Direction)
sensitivity_c <- mean(resposta_modelologistico_c[dados_teste_c$Direction=="Up"] == "Up")
specificity_c <- mean(resposta_modelologistico_c[dados_teste_c$Direction=="Down"]  == "Down")
vetor_c2<-c(accuracy = accuracy_c, sensitivity = sensitivity_c, specificity = specificity_c)
vetor_c2
#usando a coordenada mais balanceada
resposta_modelologistico_c<-ifelse(prob_modelologistico_c < melhor_coordenada_c3,
                                   "Down",
                                   "Up")
resposta_modelologistico_c<-as.factor(resposta_modelologistico_c)

#matriz confusão melhor acuracia
tabela_predicao_c3 <- table(Modelo = resposta_modelologistico_c, Real = dados_teste_c$Direction)
tabela_predicao_c3

accuracy_c <- mean(resposta_modelologistico_c == dados_teste_c$Direction)
sensitivity_c <- mean(resposta_modelologistico_c[dados_teste_c$Direction=="Up"] == "Up")
specificity_c <- mean(resposta_modelologistico_c[dados_teste_c$Direction=="Down"]  == "Down")
vetor_c3<-c(accuracy = accuracy_c, sensitivity = sensitivity_c, specificity = specificity_c)
vetor_c3

#corte 0,5
vetor_c1
#corte melhor valor/melhor acuracia
vetor_c2
#corte mais balanceado
vetor_c3

# d) ISLR2::Caravan, definindo como variavel dependente Purchase
# Carregar dados
dados_d <- Caravan
names(dados_d)
str(dados_d)

#gerando aleatório
nrow(dados_d)
seq_len(nrow(dados_d))
1:nrow(dados_d)

#separando em treino e teste
set.seed(123) # reprodutibilidade

particao_d <- sample(1:nrow(dados_d), size = 0.7*nrow(dados_d))
dados_treino_d <- dados_d[particao_d, ]
dados_teste_d  <- dados_d[-particao_d, ]
prop.table(table(dados_treino_d$Purchase)); 
prop.table(table(dados_teste_d$Purchase))

names(dados_treino_d)

#gerando o modelo
modelologistico_d<-glm(Purchase~.,
                       data = dados_treino_d, 
                       family = binomial)
formula(modelologistico_d)
summary(modelologistico_d)


#melhorando o modelo
step_backward_d <- step(modelologistico_d, 
                        direction='backward', 
                        trace=0)
modelologistico_d <- step_backward_d
formula(modelologistico_d)
summary(modelologistico_d)


#predição
prob_modelologistico_d<-predict(modelologistico_d,
                                newdata = dados_teste_d, 
                                type = "response")
prob_modelologistico_d

# Matriz de confusão e métricas com limiar escolhido")
valordecorte_d <- 0.5
pred_class_d <- ifelse(prob_modelologistico_d < valordecorte_d, "No", "Yes")
tabela_predicao_d1 <- table(Modelo = pred_class_d, Real = dados_teste_d$Purchase)
tabela_predicao_d1

accuracy_d <- mean(pred_class_d == dados_teste_d$Purchase)
sensitivity_d <- mean(pred_class_d[dados_teste_d$Purchase=="Yes"] == "Yes")
specificity_d <- mean(pred_class_d[dados_teste_d$Purchase=="No"]  == "No")
vetor_d1<-c(accuracy = accuracy_d, sensitivity = sensitivity_d, specificity = specificity_d)
vetor_d1

#ROC, AUC e escolha de limiar (método de Youden)
roc_obj_d <- roc(response = dados_teste_d$Purchase , 
                 predictor = prob_modelologistico_d, 
                 levels = c("No","Yes"),
                 direction = "<")


plot(roc_obj_d, main = sprintf("ROC - Default (AUC = %.3f)", auc(roc_obj_d)))
melhor_coordenada_d1<- coords(roc_obj_d, "best", ret = c("threshold","sensitivity","specificity"))
melhor_coordenada_d1[1]
melhor_coordenada_d1

#melhor coordenanda balanceada
round(roc_obj_d$sensitivities, digits = 2)==round(roc_obj_d$specificities, digits = 2)
roc_obj_d$thresholds[round(roc_obj_d$sensitivities, digits = 2)==round(roc_obj_d$specificities, digits = 2)]
which(round(roc_obj_d$sensitivities, digits = 2)==round(roc_obj_d$specificities, digits = 2))
melhor_coordenada_d3<-roc_obj_d$thresholds[which(round(roc_obj_d$sensitivities, digits = 2)==round(roc_obj_d$specificities, digits = 2))]
melhor_coordenada_d3

#melhor coordenada pensando na melhor acuracia
roc_obj_d$sensitivities+roc_obj_d$specificities
which.max(roc_obj_d$sensitivities+roc_obj_d$specificities)
melhor_coordenada_d2<-roc_obj_d$thresholds[which.max(roc_obj_d$sensitivities+roc_obj_d$specificities)]
melhor_coordenada_d2

#apresentação da busca do melhor valor
plot(roc_obj_d$thresholds, roc_obj_d$sensitivities,col="white",lwd=1)
lines(roc_obj_d$thresholds, roc_obj_d$specificities, col="red",lwd=2)
lines(roc_obj_d$thresholds, roc_obj_d$sensitivities, col="blue",lwd=1)
abline(v=melhor_coordenada_d3, col="green")
abline(v=melhor_coordenada_d1[1], col="blue")
abline(v=melhor_coordenada_d2, col="brown")
abline(v=0.5, col="black")

#usando a melhor coordenada/melhor acuracia 
as.numeric(melhor_coordenada_d1[1])
resposta_modelologistico_d<-ifelse(prob_modelologistico_d < as.numeric(melhor_coordenada_d1[1]),
                                   "No",
                                   "Yes")
resposta_modelologistico_d<-as.factor(resposta_modelologistico_d)

#matriz confusão melhor acuracia
tabela_predicao_d2 <- table(Modelo = resposta_modelologistico_d, Real = dados_teste_d$Purchase)
tabela_predicao_d2

accuracy_d <- mean(resposta_modelologistico_d == dados_teste_d$Purchase)
sensitivity_d <- mean(resposta_modelologistico_d[dados_teste_d$Purchase=="Yes"] == "Yes")
specificity_d <- mean(resposta_modelologistico_d[dados_teste_d$Purchase=="No"]  == "No")
vetor_d2<-c(accuracy = accuracy_d, sensitivity = sensitivity_d, specificity = specificity_d)
vetor_d2
#usando a coordenada mais balanceada
resposta_modelologistico_d<-ifelse(prob_modelologistico_d < melhor_coordenada_d3,
                                   "No",
                                   "Yes")
resposta_modelologistico_d<-as.factor(resposta_modelologistico_d)

#matriz confusão melhor acuracia
tabela_predicao_d3 <- table(Modelo = resposta_modelologistico_d, Real = dados_teste_d$Purchase)
tabela_predicao_d3

accuracy_d <- mean(resposta_modelologistico_d == dados_teste_d$Purchase)
sensitivity_d <- mean(resposta_modelologistico_d[dados_teste_d$Purchase=="Yes"] == "Yes")
specificity_d <- mean(resposta_modelologistico_d[dados_teste_d$Purchase=="No"]  == "No")
vetor_d3<-c(accuracy = accuracy_d, sensitivity = sensitivity_d, specificity = specificity_d)
vetor_d3

#corte 0,5
vetor_d1
#corte melhor valor/melhor acuracia
vetor_d2
#corte mais balanceado
vetor_d3
