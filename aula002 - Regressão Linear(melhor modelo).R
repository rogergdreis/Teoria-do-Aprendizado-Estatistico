# Aula TEA 001 Regressão Linear

# limpeza
rm(list = ls())

# carregando os dados
dados001 <- mtcars
names(dados001)
str(dados001)

# modelo
modelo001 <- lm(mpg ~ wt + qsec + am, data=dados001)
formula(modelo001)
summary(modelo001)

# Exercícios
# Construir a regressão linear multivariada da base:
# a) ISLR::Auto, definindo como variável dependente mpg (milhas por galão)
# carregando os dados
dados_a <- ISLR::Auto
names(dados_a)
str(dados_a)

# modelo
modelo_a <- lm(mpg ~ displacement + horsepower + weight + year + origin, data=dados_a)
formula(modelo_a)
summary(modelo_a)

# b) ISLR::Carsets, definindo como variável dependente sales(vendas)
# carregando os dados
dados_b <- ISLR::Carseats
names(dados_b)
str(dados_b)

# modelo
modelo_b <- lm(Sales ~ CompPrice + Income + Advertising + Price + Age, data=dados_b)
formula(modelo_b)
summary(modelo_b)

# c) ISLR::Credit, definindo como variável dependente balance(saldo do cartão de credito)
# carregando os dados
dados_c <- ISLR::Credit
names(dados_c)
str(dados_c)

# modelo
modelo_c <- lm(Balance ~ Income + Rating, data=dados_c)
formula(modelo_c)
summary(modelo_c)

# d) ISRL::Hitters, definindo como variável dependente salary(salário dos jogadores de beisebol)
# carregando os dados
dados_d <- ISLR::Hitters
names(dados_d)
str(dados_d)

# modelo
modelo_d <- lm(Salary ~ AtBat + Hits + Walks + CAtBat + CRuns + CRBI + CWalks + PutOuts, data=dados_d)
formula(modelo_d)
summary(modelo_d)

# e) ISLR2::Wage, definindo como variável dependente wage(salário)
# carregando os dados
dados_e <- ISLR2::Wage
names(dados_e)
str(dados_e)

# modelo
modelo_e <- lm(wage ~ age + logwage, data=dados_e)
formula(modelo_e)
summary(modelo_e)

# f) ISLR2::College, definindo como variável dependente outstate(mensalidade de aluno de fora do estado)
# carregando os dados
dados_f <- ISLR2::College
names(dados_f)
str(dados_f)

# modelo
modelo_f <- lm(Outstate ~ Apps + Accept + Top10perc + F.Undergrad + Room.Board + Personal + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate, data=dados_f)
formula(modelo_f)
summary(modelo_f)

# g) ISRL2::OJ, definindo como variável dependente PriceMM()
# carregando os dados
dados_g <- ISLR2::OJ
names(dados_g)
str(dados_g)

# modelo
modelo_g <- lm(PriceMM ~ DiscMM + SalePriceMM, data=dados_g)
formula(modelo_g)
summary(modelo_g)
