library(glmnet)  # glmnet(), cv.glmnet()
library(dplyr)   # %>%, mutate, etc.

set.seed(42)

#Dados
data(iris)
dados001<-iris
dados001

# Transformamos Species em fator com 3 níveis (já é por padrão).
dados001$Species<-as.factor(dados001$Species)
str(dados001)

## Treino / Teste (70% / 30%)
n <- nrow(dados001)
particao <- sample(seq_len(n), size = 0.7 * n)
treino <- dados001[particao, ]
teste  <- dados001[-particao, ]

## padronizar com média e sd do treino, e aplicar no teste.
num_cols <- sapply(treino, is.numeric)
num_cols
treino_means <- sapply(treino[, num_cols, drop = FALSE], mean)
treino_sds   <- sapply(treino[, num_cols, drop = FALSE], sd)
teste_means <- sapply(teste[, num_cols, drop = FALSE], mean)
teste_sds   <- sapply(teste[, num_cols, drop = FALSE], sd)


treino_means
teste_means
treino_sds
teste_sds

mean(treino[,4])
sd(treino[,4])

treino
treino
num_cols
treino_means
treino_sds

#normalizando com varredura
#normalizacao_apl <- function(dados, media, desviopadrao, numeroc) {
#  saida <- dados
#  saida[, numeroc] <- sweep(saida[, numeroc, drop = FALSE], 2, media, `-`)
#  saida[, numeroc] <- sweep(saida[, numeroc, drop = FALSE], 2, desviopadrao, `/`)
#  saida
#}

#treino_normalizado <- normalizacao_apl(treino, treino_means, treino_sds, num_cols)
#teste_normalizado <- normalizacao_apl(teste, treino_means, treino_sds, num_cols)

#treino
#treino_normalizado


#normalizando manualmente
treino
num_cols
treino_means[1]
treino_sds[1]


A<-treino

A[,1]<-(treino[,1]-treino_means[1])/treino_sds[1]
A[,2]<-(treino[,2]-treino_means[2])/treino_sds[2]
A[,3]<-(treino[,3]-treino_means[3])/treino_sds[3]
A[,4]<-(treino[,4]-treino_means[4])/treino_sds[4]
treino_normalizado<-A
treino_normalizado

C<-teste
C[,1]<-(teste[,1]-teste_means[1])/teste_sds[1]
C[,2]<-(teste[,2]-teste_means[2])/teste_sds[2]
C[,3]<-(teste[,3]-teste_means[3])/teste_sds[3]
C[,4]<-(teste[,4]-teste_means[4])/teste_sds[4]
C
teste_normalizado<-C
teste_normalizado

#B<-treino_normalizado

#comparando
#A==B

## glmnet (multinomial com regularização)

# Matriz (numéricas e vetor de fatores)
X_treino <- as.matrix(treino_normalizado[, num_cols, drop = FALSE])
X_teste <- as.matrix(teste_normalizado  [,  num_cols, drop = FALSE])
y_treino <- treino_normalizado$Species
y_teste  <- teste_normalizado$Species
treino_normalizado
X_treino
y_treino

# CV para escolher lambda (mantendo alpha=0.5 como elástico geral; ajuste se quiser)
cv_fit <- cv.glmnet(x = X_treino, 
                    y = y_treino, 
                    family = "multinomial", 
                    type.measure = "class",
                    alpha = 0.5,            
                    nfolds = 5)

# Melhor lambda (1se é mais parcimonioso; lambda.min é o mínimo do erro)
best_lambda <- cv_fit$lambda.1se
best_lambda


# Ajuste final no melhor 
probs_glmnet <- predict(cv_fit, 
                        newx = X_teste, 
                        s = best_lambda, 
                        type = "response")[,,1]
probs_glmnet
round(probs_glmnet, digits = 2)

# Classe predita
colnames(probs_glmnet)
max.col(probs_glmnet, ties.method = "first")
pred_glmnet <- colnames(probs_glmnet)[max.col(probs_glmnet, ties.method = "first")]
pred_glmnet
pred_glmnet <- factor(pred_glmnet, levels = levels(y_teste))
pred_glmnet

# Métricas de avaliação (Confusion Matrix, Accuracy)
matriz_confusao <- function(ocorrido, predicao) {
  table(OCORRIDO = ocorrido, PREDICAO = predicao)
}

acuracia <- function(ocorrido, predicao) {
  mean(ocorrido == predicao)
}

## Avaliação glmnet
matrizconfusao_glmnet <- matriz_confusao(y_teste, pred_glmnet)
matrizconfusao_glmnet
acuracia_glmnet <- acuracia(y_teste, pred_glmnet)
acuracia_glmnet


#Exercícios
# 1) Carseats — prever ShelveLoc (Bad/Medium/Good)

#Dados
dados_a<-ISLR2::Carseats
dados_a
str(dados_a)

## padronizar com média e sd do treino, e aplicar no teste.
num_cols_a <- sapply(dados_a, is.numeric)
num_cols_a
dados_means_a <- sapply(dados_a[, num_cols_a, drop = FALSE], mean)
dados_sds_a   <- sapply(dados_a[, num_cols_a, drop = FALSE], sd)

dados_means_a
dados_sds_a

a<-dados_a

a[,1]<-(dados_a[,1]-dados_means_a[1])/dados_sds_a[1]
a[,2]<-(dados_a[,2]-dados_means_a[2])/dados_sds_a[2]
a[,3]<-(dados_a[,3]-dados_means_a[3])/dados_sds_a[3]
a[,4]<-(dados_a[,4]-dados_means_a[4])/dados_sds_a[4]
a[,5]<-(dados_a[,5]-dados_means_a[5])/dados_sds_a[5]
a[,6]<-(dados_a[,6]-dados_means_a[6])/dados_sds_a[6]
a[,8]<-(dados_a[,8]-dados_means_a[7])/dados_sds_a[7] 
a[,9]<-(dados_a[,9]-dados_means_a[8])/dados_sds_a[8]
dados_normalizado_a<-a
dados_normalizado_a

## Treino / Teste (70% / 30%)
n <- nrow(dados_normalizado_a)
particao_a <- sample(seq_len(n), size = 0.7 * n)
treino_a <- dados_normalizado_a[particao_a, ]
teste_a  <- dados_normalizado_a[-particao_a, ]

## glmnet (multinomial com regularização)

# Matriz (numéricas e vetor de fatores)
x_treino_a <- as.matrix(treino_a[, num_cols_a, drop = FALSE])
x_teste_a <- as.matrix(teste_a  [,  num_cols_a, drop = FALSE])
y_treino_a <- treino_a$ShelveLoc
y_teste_a  <- teste_a$ShelveLoc
treino_a
x_treino_a
y_treino_a

# CV para escolher lambda (mantendo alpha=0.5 como elástico geral; ajuste se quiser)
cv_fit_a <- cv.glmnet(x = x_treino_a, 
                    y = y_treino_a, 
                    family = "multinomial", 
                    type.measure = "class",
                    alpha = 0.5,            
                    nfolds = 5)

# Melhor lambda (1se é mais parcimonioso; lambda.min é o mínimo do erro)
best_lambda_a <- cv_fit_a$lambda.1se
best_lambda_a


# Ajuste final no melhor 
probs_glmnet_a <- predict(cv_fit_a, 
                        newx = x_teste_a, 
                        s = best_lambda_a, 
                        type = "response")[,,1]
probs_glmnet_a
round(probs_glmnet_a, digits = 2)

# Classe predita
colnames(probs_glmnet_a)
max.col(probs_glmnet_a, ties.method = "first")
pred_glmnet_a <- colnames(probs_glmnet_a)[max.col(probs_glmnet_a, ties.method = "first")]
pred_glmnet_a
pred_glmnet_a <- factor(pred_glmnet_a, levels = levels(y_teste_a))
pred_glmnet_a

# Métricas de avaliação (Confusion Matrix, Accuracy)
matriz_confusao <- function(ocorrido, predicao) {
  table(OCORRIDO = ocorrido, PREDICAO = predicao)
}

acuracia <- function(ocorrido, predicao) {
  mean(ocorrido == predicao)
}

## Avaliação glmnet
matrizconfusao_glmnet_a <- matriz_confusao(y_teste_a, pred_glmnet_a)
matrizconfusao_glmnet_a
acuracia_glmnet_a <- acuracia(y_teste_a, pred_glmnet_a)
acuracia_glmnet_a

# 2) Auto — prever origin (1/2/3)

#Dados
dados_b<-ISLR2::Auto
dados_b
dados_b$origin<-as.factor(dados_b$origin)
str(dados_b)

## padronizar com média e sd do treino, e aplicar no teste.
num_cols_b <- sapply(dados_b, is.numeric)
num_cols_b
dados_means_b <- sapply(dados_b[, num_cols_b, drop = FALSE], mean)
dados_sds_b   <- sapply(dados_b[, num_cols_b, drop = FALSE], sd)

dados_means_b
dados_sds_b

b<-dados_b

b[,1]<-(dados_b[,1]-dados_means_b[1])/dados_sds_b[1]
b[,2]<-(dados_b[,2]-dados_means_b[2])/dados_sds_b[2]
b[,3]<-(dados_b[,3]-dados_means_b[3])/dados_sds_b[3]
b[,4]<-(dados_b[,4]-dados_means_b[4])/dados_sds_b[4]
b[,5]<-(dados_b[,5]-dados_means_b[5])/dados_sds_b[5]
b[,6]<-(dados_b[,6]-dados_means_b[6])/dados_sds_b[6]
b[,7]<-(dados_b[,7]-dados_means_b[7])/dados_sds_b[7]

dados_normalizado_b<-b
dados_normalizado_b

## Treino / Teste (70% / 30%)
n <- nrow(dados_normalizado_b)
particao_b <- sample(seq_len(n), size = 0.7 * n)
treino_b <- dados_normalizado_b[particao_b, ]
teste_b  <- dados_normalizado_b[-particao_b, ]

## glmnet (multinomial com regularização)

# Matriz (numéricas e vetor de fatores)
x_treino_b <- as.matrix(treino_b[, num_cols_b, drop = FALSE])
x_teste_b <- as.matrix(teste_b  [,  num_cols_b, drop = FALSE])
y_treino_b <- treino_b$origin
y_teste_b  <- teste_b$origin

treino_b
x_treino_b
y_treino_b

# CV para escolher lambda (mantendo alpha=0.5 como elástico geral; ajuste se quiser)
cv_fit_b <- cv.glmnet(x = x_treino_b, 
                      y = y_treino_b, 
                      family = "multinomial", 
                      type.measure = "class",
                      alpha = 0.5,            
                      nfolds = 5)

# Melhor lambda (1se é mais parcimonioso; lambda.min é o mínimo do erro)
best_lambda_b <- cv_fit_b$lambda.1se
best_lambda_b


# Ajuste final no melhor 
probs_glmnet_b <- predict(cv_fit_b, 
                          newx = x_teste_b, 
                          s = best_lambda_b, 
                          type = "response")[,,1]
probs_glmnet_b
round(probs_glmnet_b, digits = 2)

# Classe predita
colnames(probs_glmnet_b)
max.col(probs_glmnet_b, ties.method = "first")
pred_glmnet_b <- colnames(probs_glmnet_b)[max.col(probs_glmnet_b, ties.method = "first")]
pred_glmnet_b
pred_glmnet_b <- factor(pred_glmnet_b, levels = levels(y_teste_b))
pred_glmnet_b

# Métricas de avaliação (Confusion Matrix, Accuracy)
matriz_confusao <- function(ocorrido, predicao) {
  table(OCORRIDO = ocorrido, PREDICAO = predicao)
}

acuracia <- function(ocorrido, predicao) {
  mean(ocorrido == predicao)
}

## Avaliação glmnet
matrizconfusao_glmnet_b <- matriz_confusao(y_teste_b, pred_glmnet_b)
matrizconfusao_glmnet_b
acuracia_glmnet_b <- acuracia(y_teste_b, pred_glmnet_b)
acuracia_glmnet_b