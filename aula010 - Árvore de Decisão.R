library(rpart)
library(rpart.plot)

# Criando o dataframe
dados <- data.frame(
  estadocivil = as.factor(c("solteiro", "casado", "solteiro", "casado", "solteiro")),
  renda = c(1000, 2000, 3500, 4000, 5000),
  compra = c(0, 1, 0, 1, 1)
)

# Classificação: transformar a resposta em fator (0/1 -> Nao/Sim)
dados$compra <- factor(dados$compra, levels = c(0, 1), labels = c("Nao", "Sim"))

dados

# Criterio GINI
arvore001 <- rpart(formula = compra ~ estadocivil + renda,
                   data = dados,
                   method = "class",
                   parms = list(split = "gini"),
                   control = rpart.control(cp = 0.0,      # ou "information" para entropia
                                           minsplit = 2,  # deixa crescer (poucos dados)
                                           minbucket = 1,
                                           maxdepth =  4,
                                           xval = 10
                                           ))

arvore001

#grafico
rpart.plot(arvore001,
           type = 3,
           extra = 104,
           under = T,
           faclen = 0,
           fallen.leaves = T,
           main = "Arvore"
)


summary(dados)

#novo data.frame para predicao
novos<-data.frame(
  estadocivil = as.factor(c("solteiro","solteiro","casado")),
  renda       = c(3000, 6000, 2500)
)

novos

#predicao
pred_novos_classes  <- predict(arvore001,
                               novos,
                               type="class")

pred_novos_classes

#criando novo dataframe
novos$credito<-pred_novos_classes

novos

# Exibir tabela de complexidade
arvore001$cptable
printcp(arvore001)
plotcp(arvore001) # gráfico de erro x cp

arvore001$cptable[,'xerror']

# Selecionar cp ótimo
cp_otimo <- arvore001$cptable[which.min(arvore001$cptable[,'xerror']), 'CP']
cp_otimo

# Podar a árvore
arvore001_podada <- prune(arvore001, cp = cp_otimo)

# Visualizar árvores
rpart.plot(arvore001, main = "Árvore Completa")
rpart.plot(arvore001_podada, main = "Árvore Podada (CP ótimo)")

#Exercícios
# 1) Carseats — prever ShelveLoc (Bad/Medium/Good)
dados_carset <- ISLR::Carseats
dados_carset
str(dados_carset)

## Treino / Teste (70% / 30%)
n <- nrow(dados_carset)
particao_carset <- sample(seq_len(n), size = 0.7 * n)
treino_carset <- dados_carset[particao_carset, ]
teste_carset <- dados_carset[-particao_carset, ]

# Criterio GINI
arvore_carset <- rpart(formula = ShelveLoc ~ .,
                   data = treino_carset,
                   method = "class",
                   parms = list(split = "gini"),
                   control = rpart.control(cp = 0.0,      # ou "information" para entropia
                                           minsplit = 2,  # deixa crescer (poucos dados)
                                           minbucket = 1,
                                           maxdepth =  4,
                                           xval = 10
                   ))

arvore_carset

#grafico
rpart.plot(arvore_carset,
           type = 3,
           extra = 104,
           under = T,
           faclen = 0,
           fallen.leaves = T,
           main = "Arvore"
)

summary(dados_carset)

# teste para predicao
teste_carset

#predicao
pred_carset  <- predict(arvore_carset,
                               teste_carset,
                               type="class")

pred_carset

#criando novo dataframe
teste_carset$predicao<-pred_carset

teste_carset

# 2) College — prever Private (No/Yes)
dados_college <- ISLR::College
dados_college
str(dados_college)

## Treino / Teste (70% / 30%)
n <- nrow(dados_college)
particao_college <- sample(seq_len(n), size = 0.7 * n)
treino_college <- dados_college[particao_college, ]
teste_college <- dados_college[-particao_college, ]

# Criterio GINI
arvore_college <- rpart(formula = Private ~ .,
                       data = treino_college,
                       method = "class",
                       parms = list(split = "gini"),
                       control = rpart.control(cp = 0.0,      # ou "information" para entropia
                                               minsplit = 2,  # deixa crescer (poucos dados)
                                               minbucket = 1,
                                               maxdepth =  4,
                                               xval = 10
                       ))

arvore_college

#grafico
rpart.plot(arvore_college,
           type = 3,
           extra = 104,
           under = T,
           faclen = 0,
           fallen.leaves = T,
           main = "Arvore"
)

summary(dados_college)

# teste para predicao
teste_college

#predicao
pred_college  <- predict(arvore_college,
                        teste_college,
                        type="class")

pred_college

#criando novo dataframe
teste_college$predicao<-pred_college

teste_college

# 3) iris — prever Species (setosa/versicolor/virginica)
dados_iris <- iris
dados_iris
str(dados_iris)

## Treino / Teste (70% / 30%)
n <- nrow(dados_iris)
particao_iris <- sample(seq_len(n), size = 0.7 * n)
treino_iris <- dados_iris[particao_iris, ]
teste_iris <- dados_iris[-particao_iris, ]

# Criterio GINI
arvore_iris <- rpart(formula = Species ~ .,
                        data = treino_iris,
                        method = "class",
                        parms = list(split = "gini"),
                        control = rpart.control(cp = 0.0,      # ou "information" para entropia
                                                minsplit = 2,  # deixa crescer (poucos dados)
                                                minbucket = 1,
                                                maxdepth =  4,
                                                xval = 10
                        ))

arvore_iris

#grafico
rpart.plot(arvore_iris,
           type = 3,
           extra = 104,
           under = T,
           faclen = 0,
           fallen.leaves = T,
           main = "Arvore"
)

summary(dados_iris)

# teste para predicao
teste_iris

#predicao
pred_iris  <- predict(arvore_iris,
                         teste_iris,
                         type="class")

pred_iris

#criando novo dataframe
teste_iris$predicao<-pred_iris

teste_iris
