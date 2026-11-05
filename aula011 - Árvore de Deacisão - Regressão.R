library(rpart)
library(rpart.plot)

#Criando o dataframe
dados02 <- data.frame(
  Estadocivil = as.factor(c("solteiro","casado","solteiro","casado","casado","solteiro")),
  renda       = c(1000, 2000, 3500,3000 ,4000, 5000),
  Compra      = c(100, 213, 250, 345, 348, 330)
)

dados02
str(dados02)

# Cirtério GINI
arvore003 <- rpart(formula = Compra ~ Estadocivil + renda,
                   data    = dados02,
                   method  = "anova",
                   #parms   = list(split = "gini"),   # ou "information" para entropia
                   control = rpart.control(cp        = 0.0001,   # deixa crescer (poucos dados)
                                           minsplit  = 2,
                                           minbucket = 1,
                                           maxdepth  = 6,
                                           xval      = 10
                   ))

arvore003

#grafico
# Visualizar árvore
rpart.plot(arvore003, 
           type = 2, 
           fallen.leaves = TRUE)


#novo data.frame para predicao
novos<-data.frame(
  Estadocivil = as.factor(c("solteiro","solteiro","casado")),
  renda       = c(3200, 4850, 2500),
  Compra = c(0,0,0)
)

novos

#predicao
pred_novos_probabilidade <- predict(arvore003,newdata =  novos)


pred_novos_probabilidade

novos$Compra<-pred_novos_probabilidade


novos

#Resolver os exercícios da tarefa 006

# a) ISLR::Carseats, definindo como variável dependente sales (vendas)dados001<-ISLR::Carseatsstr(dados001)
library(rpart)
library(rpart.plot)

#Criando o dataframe
dados_carseats <- ISLR::Carseats

dados_carseats
str(dados_carseats)

#separando em treino e teste
set.seed(42) # reprodutibilidade

## Treino / Teste (70% / 30%)
n <- nrow(dados_carseats)
particao_carseats <- sample(seq_len(n), size = 0.7 * n)
treino_carseats <- dados_carseats[particao_carseats, ]
teste_carseats <- dados_carseats[-particao_carseats, ]

# Cirtério GINI
arvore_carseats <- rpart(formula = Sales ~ .,
                   data    = treino_carseats,
                   method  = "anova",
                   #parms   = list(split = "gini"),   # ou "information" para entropia
                   control = rpart.control(cp        = 0.0001,   # deixa crescer (poucos dados)
                                           minsplit  = 2,
                                           minbucket = 1,
                                           maxdepth  = 6,
                                           xval      = 10
                   ))

arvore_carseats

#grafico
# Visualizar árvore
rpart.plot(arvore_carseats, 
           type = 2, 
           fallen.leaves = TRUE)


summary(dados_carseats)

# teste para predicao
teste_carseats

#predicao
pred_carseats  <- predict(arvore_carseats, newdata = teste_carseats)
                        
pred_carseats

#criando novo dataframe
teste_carseats$predicao<-pred_carseats

teste_carseats

#calcular o R
RSS<-sum((teste_carseats$Sales-teste_carseats$predicao)^2)
TSS<-sum((teste_carseats$Sales-mean(teste_carseats$Sales))^2)
Rquadrado<-1-RSS/TSS
Rquadrado

# Exibir tabela de complexidade
arvore_carseats$cptable
printcp(arvore_carseats)
plotcp(arvore_carseats) # gráfico de erro x cp

arvore_carseats$cptable[,'xerror']

# Selecionar cp ótimo
cp_otimo <- arvore_carseats$cptable[which.min(arvore_carseats$cptable[,'xerror']), 'CP']
cp_otimo

# Podar a árvore
poda_carseats <- prune(arvore_carseats, cp = cp_otimo)
poda_carseats

# Visualizar árvores
rpart.plot(arvore_carseats, main = "Árvore Completa")
rpart.plot(poda_carseats, main = "Árvore Podada (CP ótimo)")

# Predição com a árvore podada
pred_carseats_podada <- predict(poda_carseats, newdata = teste_carseats)
pred_carseats_podada

# Adicionando a predição podada ao dataframe de teste
teste_carseats$predicao_podada <- pred_carseats_podada

# Calcular o R² da Árvore Podada
RSS_podada <- sum((teste_carseats$Sales - teste_carseats$predicao_podada)^2)
TSS_podada <- sum((teste_carseats$Sales - mean(teste_carseats$Sales))^2) # Igual ao TSS original
Rquadrado_podada <- 1 - RSS_podada / TSS_podada
Rquadrado_podada

# Comparação direta dos resultados
comparacao_r2 <- data.frame(
  Modelo    = c("Árvore Completa", "Árvore Podada (CP Ótimo)"),
  RSS       = c(RSS, RSS_podada),
  R_Quadrado = c(Rquadrado, Rquadrado_podada)
)

comparacao_r2

# b) ISLR::Credit, definindo como variável dependente balance (saldo do cartão de crédito)dados001<-ISLR::Creditstr(dados001)
library(rpart)
library(rpart.plot)

#Criando o dataframe
dados_credit <- ISLR::Credit

dados_credit
str(dados_credit)

#separando em treino e teste
set.seed(42) # reprodutibilidade

## Treino / Teste (70% / 30%)
n <- nrow(dados_credit)
particao_credit <- sample(seq_len(n), size = 0.7 * n)
treino_credit <- dados_credit[particao_credit, ]
teste_credit <- dados_credit[-particao_credit, ]

# Cirtério GINI
arvore_credit <- rpart(formula = Balance ~ .,
                         data    = treino_credit,
                         method  = "anova",
                         #parms   = list(split = "gini"),   # ou "information" para entropia
                         control = rpart.control(cp        = 0.0001,   # deixa crescer (poucos dados)
                                                 minsplit  = 2,
                                                 minbucket = 1,
                                                 maxdepth  = 6,
                                                 xval      = 10
                         ))

arvore_credit

#grafico
# Visualizar árvore
rpart.plot(arvore_credit, 
           type = 2, 
           fallen.leaves = TRUE)


summary(dados_credit)

# teste para predicao
teste_credit

#predicao
pred_credit  <- predict(arvore_credit, newdata = teste_credit)

pred_credit

#criando novo dataframe
teste_credit$predicao<-pred_credit
teste_credit

#calcular o R
RSS<-sum((teste_credit$Balance-teste_credit$predicao)^2)
TSS<-sum((teste_credit$Balance-mean(teste_credit$Balance))^2)
Rquadrado<-1-RSS/TSS
Rquadrado

# Exibir tabela de complexidade
arvore_credit$cptable
printcp(arvore_credit)
plotcp(arvore_credit) # gráfico de erro x cp

arvore_credit$cptable[,'xerror']

# Selecionar cp ótimo
cp_otimo <- arvore_credit$cptable[which.min(arvore_credit$cptable[,'xerror']), 'CP']
cp_otimo

# Podar a árvore
poda_credit <- prune(arvore_credit, cp = cp_otimo)
poda_credit

# Visualizar árvores
rpart.plot(arvore_credit, main = "Árvore Completa")
rpart.plot(poda_credit, main = "Árvore Podada (CP ótimo)")

# Predição com a árvore podada
pred_credit_podada <- predict(poda_credit, newdata = teste_credit)
pred_credit_podada

# Adicionando a predição podada ao dataframe de teste
teste_credit$predicao_podada <- pred_credit_podada

# Calcular o R² da Árvore Podada
RSS_podada <- sum((teste_credit$Balance - teste_credit$predicao_podada)^2)
TSS_podada <- sum((teste_credit$Balance - mean(teste_credit$Balance))^2) # Igual ao TSS original
Rquadrado_podada <- 1 - RSS_podada / TSS_podada
Rquadrado_podada

# Comparação direta dos resultados
comparacao_r2 <- data.frame(
  Modelo    = c("Árvore Completa", "Árvore Podada (CP Ótimo)"),
  RSS       = c(RSS, RSS_podada),
  R_Quadrado = c(Rquadrado, Rquadrado_podada)
)

comparacao_r2

# c) ISLR::Hitters, definindo como variável dependente salary (salários dos jogadores de baisebol)dados001<-ISLR::Hittersstr(dados001)
library(rpart)
library(rpart.plot)

#Criando o dataframe
dados_hitters <- ISLR::Hitters
dados_hitters <- subset(dados_hitters, !is.na(Salary))
dados_hitters
str(dados_hitters)

#separando em treino e teste
set.seed(42) # reprodutibilidade

## Treino / Teste (70% / 30%)
n <- nrow(dados_hitters)
particao_hitters <- sample(seq_len(n), size = 0.7 * n)
treino_hitters <- dados_hitters[particao_hitters, ]
teste_hitters <- dados_hitters[-particao_hitters, ]

# Cirtério GINI
arvore_hitters <- rpart(formula = Salary ~ .,
                       data    = treino_hitters,
                       method  = "anova",
                       #parms   = list(split = "gini"),   # ou "information" para entropia
                       control = rpart.control(cp        = 0.0001,   # deixa crescer (poucos dados)
                                               minsplit  = 2,
                                               minbucket = 1,
                                               maxdepth  = 6,
                                               xval      = 10
                       ))

arvore_hitters

#grafico
# Visualizar árvore
rpart.plot(arvore_hitters, 
           type = 2, 
           fallen.leaves = TRUE)


summary(dados_hitters)

# teste para predicao
teste_hitters

#predicao
pred_hitters  <- predict(arvore_hitters, newdata = teste_hitters)

pred_hitters

#criando novo dataframe
teste_hitters$predicao<-pred_hitters
teste_hitters

#calcular o R
RSS<-sum((teste_hitters$Salary-teste_hitters$predicao)^2)
TSS<-sum((teste_hitters$Salary-mean(teste_hitters$Salary))^2)
Rquadrado<-1-RSS/TSS
Rquadrado

# Exibir tabela de complexidade
arvore_hitters$cptable
printcp(arvore_hitters)
plotcp(arvore_hitters) # gráfico de erro x cp

arvore_hitters$cptable[,'xerror']

# Selecionar cp ótimo
cp_otimo <- arvore_hitters$cptable[which.min(arvore_hitters$cptable[,'xerror']), 'CP']
cp_otimo

# Podar a árvore
poda_hitters <- prune(arvore_hitters, cp = cp_otimo)
poda_hitters

# Visualizar árvores
rpart.plot(arvore_hitters, main = "Árvore Completa")
rpart.plot(poda_hitters, main = "Árvore Podada (CP ótimo)")

# Predição com a árvore podada
pred_hitters_podada <- predict(poda_hitters, newdata = teste_hitters)
pred_hitters_podada

# Adicionando a predição podada ao dataframe de teste
teste_hitters$predicao_podada <- pred_hitters_podada

# Calcular o R² da Árvore Podada
RSS_podada <- sum((teste_hitters$Salary - teste_hitters$predicao_podada)^2)
TSS_podada <- sum((teste_hitters$Salary - mean(teste_hitters$Salary))^2) # Igual ao TSS original
Rquadrado_podada <- 1 - RSS_podada / TSS_podada
Rquadrado_podada

# Comparação direta dos resultados
comparacao_r2 <- data.frame(
  Modelo    = c("Árvore Completa", "Árvore Podada (CP Ótimo)"),
  RSS       = c(RSS, RSS_podada),
  R_Quadrado = c(Rquadrado, Rquadrado_podada)
)

comparacao_r2

# d) ISLR2::Wage, definindo como variável dependente wage (salário)dados001<-ISLR::Wagestr(dados001)
library(rpart)
library(rpart.plot)

#Criando o dataframe
dados_wage <- ISLR::Wage

dados_wage
str(dados_wage)

#separando em treino e teste
set.seed(42) # reprodutibilidade

## Treino / Teste (70% / 30%)
n <- nrow(dados_wage)
particao_wage <- sample(seq_len(n), size = 0.7 * n)
treino_wage <- dados_wage[particao_wage, ]
teste_wage <- dados_wage[-particao_wage, ]

# Cirtério GINI
arvore_wage <- rpart(formula = wage ~ .,
                        data    = treino_wage,
                        method  = "anova",
                        #parms   = list(split = "gini"),   # ou "information" para entropia
                        control = rpart.control(cp        = 0.0001,   # deixa crescer (poucos dados)
                                                minsplit  = 2,
                                                minbucket = 1,
                                                maxdepth  = 6,
                                                xval      = 10
                        ))

arvore_wage

#grafico
# Visualizar árvore
rpart.plot(arvore_wage, 
           type = 2, 
           fallen.leaves = TRUE)


summary(dados_wage)

# teste para predicao
teste_wage

#predicao
pred_wage  <- predict(arvore_wage, newdata = teste_wage)

pred_wage

#criando novo dataframe
teste_wage$predicao<-pred_wage
teste_wage

#calcular o R
RSS<-sum((teste_wage$wage-teste_wage$predicao)^2)
TSS<-sum((teste_wage$wage-mean(teste_wage$wage))^2)
Rquadrado<-1-RSS/TSS
Rquadrado

# Exibir tabela de complexidade
arvore_wage$cptable
printcp(arvore_wage)
plotcp(arvore_wage) # gráfico de erro x cp

arvore_wage$cptable[,'xerror']

# Selecionar cp ótimo
cp_otimo <- arvore_wage$cptable[which.min(arvore_wage$cptable[,'xerror']), 'CP']
cp_otimo

# Podar a árvore
poda_wage <- prune(arvore_wage, cp = cp_otimo)
poda_wage

# Visualizar árvores
rpart.plot(arvore_wage, main = "Árvore Completa")
rpart.plot(poda_wage, main = "Árvore Podada (CP ótimo)")

# Predição com a árvore podada
pred_wage_podada <- predict(poda_wage, newdata = teste_wage)
pred_wage_podada

# Adicionando a predição podada ao dataframe de teste
teste_wage$predicao_podada <- pred_wage_podada

# Calcular o R² da Árvore Podada
RSS_podada <- sum((teste_wage$wage - teste_wage$predicao_podada)^2)
TSS_podada <- sum((teste_wage$wage - mean(teste_wage$wage))^2) # Igual ao TSS original
Rquadrado_podada <- 1 - RSS_podada / TSS_podada
Rquadrado_podada

# Comparação direta dos resultados
comparacao_r2 <- data.frame(
  Modelo    = c("Árvore Completa", "Árvore Podada (CP Ótimo)"),
  RSS       = c(RSS, RSS_podada),
  R_Quadrado = c(Rquadrado, Rquadrado_podada)
)

comparacao_r2
