# Regressao Logistica
# Bibliotecas

library(caret)
library(pROC)
library(ISLR)
library(ISLR2)

# Carregar dados
dados <- mtcars
names(dados)
str(dados)
dados$am <-ifelse(dados$am==1, "manual", "auto")
dados$am <- as.factor(dados$am)

# Gerando aleatório
nrow(dados)
seq_len(nrow(dados))

# Separando em treino e teste
set.seed(123) # reprodutibilidade
particao <- sample(seq_len(nrow(dados)), size = 0.7 * nrow(dados))
dados_treino <- dados[particao, ]
dados_teste <- dados[-particao, ]
prop.table(table(dados_treino$am))
prop.table(table(dados_teste$am))

# Gerando o modelo
modelo <- glm(am~.,
              data = dados_treino,
              family = binomial())
formula(modelo)
summary(modelo)

# Predição
prob_modelo <- predict(modelo,
                       newdata = dados_teste,
                       type = "response")

prob_modelo

# matriz confusão e métricas com limiar escolhido
valordecorte <- 0.5

pred_class <- ifelse(prob_modelo > valordecorte, "manual", "auto")
pred_class

tabela_predicao <- table(Modelo = pred_class, Real = dados_teste$am)
tabela_predicao

accuracy <- mean(pred_class == dados_teste$am)
sensitivity <- mean(pred_class[dados_teste$am=="auto"] == "auto")
specificity <- mean(pred_class[dados_teste$am=="manual"] == "manual")
c(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity)


# Exercícios
# Construir a regressão logistica da base:
# a) ISLR::Default, definindo como varíavel dependente student
# Carregar dados
dados_a <- Default
names(dados_a)
str(dados_a)

# Gerando aleatório
nrow(dados_a)
seq_len(nrow(dados_a))

# Separando em treino e teste
set.seed(123) # reprodutibilidade
particao_a <- sample(seq_len(nrow(dados_a)), size = 0.7 * nrow(dados_a))
dados_treino_a <- dados_a[particao_a, ]
dados_teste_a <- dados_a[-particao_a, ]
prop.table(table(dados_treino_a$student))
prop.table(table(dados_teste_a$student))

# Gerando o modelo
modelo_a <- glm(student~.,
                data = dados_treino_a,
                family = binomial())
formula(modelo_a)
summary(modelo_a)

# Predição
prob_modelo_a <- predict(modelo_a,
                         newdata = dados_teste_a,
                         type = "response")

prob_modelo_a

# matriz confusão e métricas com limiar escolhido
valordecorte_a <- 0.5

pred_class_a <- ifelse(prob_modelo_a > valordecorte_a, "No", "Yes")
pred_class_a

tabela_predicao_a <- table(Modelo = pred_class_a, Real = dados_teste_a$student)
tabela_predicao_a

accuracy <- mean(pred_class_a == dados_teste_a$student)
sensitivity <- mean(pred_class_a[dados_teste_a$student=="Yes"] == "Yes")
specificity <- mean(pred_class_a[dados_teste_a$student=="No"] == "No")
c(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity)


# b)ISLR2::Smarket, definindo como variavel dependente Direction
# Carregar dados
dados_b <- Smarket
names(dados_b)
str(dados_b)

# Gerando aleatório
nrow(dados_b)
seq_len(nrow(dados_b))

# Separando em treino e teste
set.seed(123) # reprodutibilidade
particao_b <- sample(seq_len(nrow(dados_b)), size = 0.7 * nrow(dados_b))
dados_treino_b <- dados_b[particao_b, ]
dados_teste_b <- dados_b[-particao_b, ]
prop.table(table(dados_treino_b$Direction))
prop.table(table(dados_teste_b$Direction))

# Gerando o modelo
modelo_b <- glm(Direction~.,
                data = dados_treino_b,
                family = binomial())
formula(modelo_b)
summary(modelo_b)

# Predição
prob_modelo_b <- predict(modelo_b,
                         newdata = dados_teste_b,
                         type = "response")

prob_modelo_b

# matriz confusão e métricas com limiar escolhido
valordecorte_b <- 0.5

pred_class_b <- ifelse(prob_modelo_b > valordecorte_b, "Down", "Up")
pred_class_b

tabela_predicao_b <- table(Modelo = pred_class_b, Real = dados_teste_b$Direction)
tabela_predicao_b

accuracy <- mean(pred_class_b == dados_teste_b$Direction)
sensitivity <- mean(pred_class_b[dados_teste_b$Direction=="Up"] == "Up")
specificity <- mean(pred_class_b[dados_teste_b$Direction=="Down"] == "Down")
c(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity)

# c) ISLR2::Weekly, definindo como variavel dependente Direction
# Carregar dados
dados_c <- Weekly
names(dados_c)
str(dados_c)

# Gerando aleatório
nrow(dados_c)
seq_len(nrow(dados_c))

# Separando em treino e teste
set.seed(123) # reprodutibilidade
particao_c <- sample(seq_len(nrow(dados_c)), size = 0.7 * nrow(dados_c))
dados_treino_c <- dados_c[particao_c, ]
dados_teste_c <- dados_c[-particao_c, ]
prop.table(table(dados_treino_c$Direction))
prop.table(table(dados_teste_c$Direction))

# Gerando o modelo
modelo_c <- glm(Direction~.,
                data = dados_treino_c,
                family = binomial())
formula(modelo_c)
summary(modelo_c)

# Predição
prob_modelo_c <- predict(modelo_c,
                         newdata = dados_teste_c,
                         type = "response")

prob_modelo_c

# matriz confusão e métricas com limiar escolhido
valordecorte_c <- 0.5

pred_class_c <- ifelse(prob_modelo_c > valordecorte_c, "Down", "Up")
pred_class_c

tabela_predicao_c <- table(Modelo = pred_class_c, Real = dados_teste_c$Direction)
tabela_predicao_c

accuracy <- mean(pred_class_c == dados_teste_c$Direction)
sensitivity <- mean(pred_class_c[dados_teste_c$Direction=="Up"] == "Up")
specificity <- mean(pred_class_c[dados_teste_c$Direction=="Down"] == "Down")
c(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity)

# d) ISLR2::Caravan, definindo como variavel dependente Purchase
# Carregar dados
dados_d <- Caravan
names(dados_d)
str(dados_d)

# Gerando aleatório
nrow(dados_d)
seq_len(nrow(dados_d))

# Separando em treino e teste
set.seed(123) # reprodutibilidade
particao_d <- sample(seq_len(nrow(dados_d)), size = 0.7 * nrow(dados_d))
dados_treino_d <- dados_d[particao_d, ]
dados_teste_d <- dados_d[-particao_d, ]
prop.table(table(dados_treino_d$Purchase))
prop.table(table(dados_teste_d$Purchase))

# Gerando o modelo
modelo_d <- glm(Purchase~.,
                data = dados_treino_d,
                family = binomial())
formula(modelo_d)
summary(modelo_d)

# Predição
prob_modelo_d <- predict(modelo_d,
                         newdata = dados_teste_d,
                         type = "response")

prob_modelo_d

# matriz confusão e métricas com limiar escolhido
valordecorte_d <- 0.5

pred_class_d <- ifelse(prob_modelo_d > valordecorte_d, "Yes", "No")
pred_class_d

tabela_predicao_d <- table(Modelo = pred_class_d, Real = dados_teste_d$Purchase)
tabela_predicao_d

accuracy <- mean(pred_class_d == dados_teste_d$Purchase)
sensitivity <- mean(pred_class_d[dados_teste_d$Purchase=="No"] == "No")
specificity <- mean(pred_class_d[dados_teste_d$Purchase=="Yes"] == "Yes")
c(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity)
