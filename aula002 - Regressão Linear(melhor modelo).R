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

# Y aproximado
b0<-modelo001$coefficients[1]
b1<-modelo001$coefficients[2]
b2<-modelo001$coefficients[3]
b3<-modelo001$coefficients[4]

yaprox<-b0+b1*dados001$wt+b2*dados001$qsec+b3*dados001$am
yaprox

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

# Y aproximado
b0_a<-modelo_a$coefficients[1]
b1_a<-modelo_a$coefficients[2]
b2_a<-modelo_a$coefficients[3]
b3_a<-modelo_a$coefficients[4]
b4_a<-modelo_a$coefficients[5]
b5_a<-modelo_a$coefficients[6]

yaprox_a<-b0_a+b1_a*dados_a$displacement+b2_a*dados_a$horsepower+b3_a*dados_a$weight+b4_a*dados_a$year+b5_a*dados_a$origin
yaprox_a

# b) ISLR::Carsets, definindo como variável dependente sales(vendas)
# carregando os dados
dados_b <- ISLR::Carseats
names(dados_b)
str(dados_b)

# modelo
modelo_b <- lm(Sales ~ CompPrice + Income + Advertising + Price + Age, data=dados_b)
formula(modelo_b)
summary(modelo_b)

# Y aproximado
b0_b<-modelo_b$coefficients[1]
b1_b<-modelo_b$coefficients[2]
b2_b<-modelo_b$coefficients[3]
b3_b<-modelo_b$coefficients[4]
b4_b<-modelo_b$coefficients[5]
b5_b<-modelo_b$coefficients[6]

yaprox_b<-b0_b+b1_b*dados_b$CompPrice+b2_b*dados_b$Income+b3_b*dados_b$Advertising+b4_b*dados_b$Price+b5_b*dados_b$Age
yaprox_b

# c) ISLR::Credit, definindo como variável dependente balance(saldo do cartão de credito)
# carregando os dados
dados_c <- ISLR::Credit
names(dados_c)
str(dados_c)

# modelo
modelo_c <- lm(Balance ~ Income + Rating, data=dados_c)
formula(modelo_c)
summary(modelo_c)

# Y aproximado
b0_c<-modelo_c$coefficients[1]
b1_c<-modelo_c$coefficients[2]
b2_c<-modelo_c$coefficients[3]

yaprox_c<-b0_c+b1_c*dados_c$Income+b2_c*dados_c$Rating
yaprox_c

# d) ISRL::Hitters, definindo como variável dependente salary(salário dos jogadores de beisebol)
# carregando os dados
dados_d <- ISLR::Hitters
names(dados_d)
str(dados_d)

# modelo
modelo_d <- lm(Salary ~ AtBat + Hits + Walks + CAtBat + CRuns + CRBI + CWalks + PutOuts, data=dados_d)
formula(modelo_d)
summary(modelo_d)

# Y aproximado
b0_d<-modelo_d$coefficients[1]
b1_d<-modelo_d$coefficients[2]
b2_d<-modelo_d$coefficients[3]
b3_d<-modelo_d$coefficients[4]
b4_d<-modelo_d$coefficients[5]
b5_d<-modelo_d$coefficients[6]
b6_d<-modelo_d$coefficients[7]
b7_d<-modelo_d$coefficients[8]
b8_d<-modelo_d$coefficients[9]

yaprox_d<-b0_d+b1_d*dados_d$AtBat+b2_d*dados_d$Hits+b3_d*dados_d$Walks+b4_d*dados_d$CAtBat+b5_d*dados_d$CRuns+b6_d*dados_d$CRBI+b7_d*dados_d$CWalks+b8_d*dados_d$PutOuts
yaprox_d

# e) ISLR2::Wage, definindo como variável dependente wage(salário)
# carregando os dados
dados_e <- ISLR2::Wage
names(dados_e)
str(dados_e)

# modelo
modelo_e <- lm(wage ~ age + logwage, data=dados_e)
formula(modelo_e)
summary(modelo_e)

# Y aproximado
b0_e<-modelo_e$coefficients[1]
b1_e<-modelo_e$coefficients[2]
b2_e<-modelo_e$coefficients[3]

yaprox_e<-b0_e+b1_e*dados_e$age+b2_e*dados_e$logwage
yaprox_e

# f) ISLR2::College, definindo como variável dependente outstate(mensalidade de aluno de fora do estado)
# carregando os dados
dados_f <- ISLR2::College
names(dados_f)
str(dados_f)

# modelo
modelo_f <- lm(Outstate ~ Apps + Accept + Top10perc + F.Undergrad + Room.Board + Personal + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate, data=dados_f)
formula(modelo_f)
summary(modelo_f)

# Y aproximado
b0_f<-modelo_f$coefficients[1]
b1_f<-modelo_f$coefficients[2]
b2_f<-modelo_f$coefficients[3]
b3_f<-modelo_f$coefficients[4]
b4_f<-modelo_f$coefficients[5]
b5_f<-modelo_f$coefficients[6]
b6_f<-modelo_f$coefficients[7]
b7_f<-modelo_f$coefficients[8]
b8_f<-modelo_f$coefficients[9]
b9_f<-modelo_f$coefficients[10]
b10_f<-modelo_f$coefficients[11]
b11_f<-modelo_f$coefficients[12]

yaprox_f<-b0_f+b1_f*dados_f$Apps+b2_f*dados_f$Accept+b3_f*dados_f$Top10perc+b4_f*dados_f$F.Undergrad+b5_f*dados_f$Room.Board+b6_f*dados_f$Personal+b7_f*dados_f$Terminal+b8_f*dados_f$S.F.Ratio+b9_f*dados_f$perc.alumni+b10_f*dados_f$Expend+b11_f*dados_f$Grad.Rate
yaprox_f

# g) ISRL2::OJ, definindo como variável dependente PriceMM()
# carregando os dados
dados_g <- ISLR2::OJ
names(dados_g)
str(dados_g)

# modelo
modelo_g <- lm(PriceMM ~ DiscMM + SalePriceMM, data=dados_g)
formula(modelo_g)
summary(modelo_g)

# Y aproximado
b0_g<-modelo_g$coefficients[1]
b1_g<-modelo_g$coefficients[2]
b2_g<-modelo_g$coefficients[3]

yaprox_g<-b0_g+b1_g*dados_g$DiscMM+b2_g*dados_g$SalePriceMM
yaprox_g
