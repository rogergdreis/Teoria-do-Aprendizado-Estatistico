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
