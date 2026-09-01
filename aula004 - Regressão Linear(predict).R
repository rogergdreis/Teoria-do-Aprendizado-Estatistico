# Aula TEA 001 Regressão Linear

# limpeza
rm(list = ls())

# carregando os dados
dados001 <- mtcars
names(dados001)
str(dados001)

# modelo
modelo001_vazio <- lm(mpg ~ 1, data = dados001)
modelo001_completo <- lm(mpg ~ cyl + disp + hp + drat + wt + qsec + vs + am + gear + carb, data=dados001)

# modelo melhorado
step_forward <- step(modelo001_vazio,
                     scope = formula(modelo001_completo),
                     direction = 'forward',
                     trace = 0)

step_backward <- step(modelo001_completo,
                      direction = 'backward',
                      trace = 0)

step_both <- step(modelo001_completo,
                  direction = 'both',
                  trace = 0)

# verificar qual esta melhor
formula(step_forward)
formula(step_backward)
formula(step_both)

summary(step_forward)
summary(step_backward)
summary(step_both)

# predição manual
b0<-step_both$coefficients[1]
b1<-step_both$coefficients[2]
b2<-step_both$coefficients[3]
b3<-step_both$coefficients[4]

predicao002<-b0+b1*dados001$wt+b2*dados001$qsec+b3*dados001$am
predicao002

# predição
predicao001 <- predict(step_both, newdata = dados001)
predicao001

predicaodados <- data.frame(mpg = dados001$mpg,
                            predicao1 = predicao001,
                            predicao2 = predicao002)
predicaodados

# calcular o R
RSS <- sum((dados001$mpg-predicaodados$predicao1)^2)
TSS <- sum((dados001$mpg-mean(dados001$mpg))^2)
Rquadrado <- 1-RSS/TSS
Rquadrado

# Exercícios
# Construir a regressão linear multivariada da base:
# a) ISLR::Auto, definindo como variável dependente mpg (milhas por galão)
# carregando os dados
dados_a <- ISLR::Auto
names(dados_a)
str(dados_a)

# modelo
modelo_vazio_a <- lm(mpg ~ 1, data = dados_a)
modelo_completo_a <- lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration + year + origin, data=dados_a)

# modelo melhorado
step_forward_a <- step(modelo_vazio_a,
                       scope = formula(modelo_completo_a),
                       direction = 'forward',
                       trace = 0)

step_backward_a <- step(modelo_completo_a,
                        direction = 'backward',
                        trace = 0)

step_both_a <- step(modelo_completo_a,
                    direction = 'both',
                    trace = 0)

# verificar qual esta melhor
formula(step_forward_a)
formula(step_backward_a)
formula(step_both_a)

summary(step_forward_a)
summary(step_backward_a)
summary(step_both_a)

# predição manual
b0_a<-step_forward_a$coefficients[1]
b1_a<-step_forward_a$coefficients[2]
b2_a<-step_forward_a$coefficients[3]
b3_a<-step_forward_a$coefficients[4]

predicao_a2<-b0_a+b1_a*dados_a$weight+b2_a*dados_a$year+b3_a*dados_a$origin
predicao_a2

# predição
predicao_a1 <- predict(step_forward_a, newdata = dados_a)
predicao_a1

predicaodados_a <- data.frame(mpg = dados_a$mpg,
                            predicao1 = predicao_a1,
                            predicao2 = predicao_a2)
predicaodados_a

# calcular o R
RSS_a <- sum((dados_a$mpg-predicaodados_a$predicao1)^2)
TSS_a <- sum((dados_a$mpg-mean(dados_a$mpg))^2)
Rquadrado_a <- 1-RSS_a/TSS_a
Rquadrado_a

# b) ISLR::Carsets, definindo como variável dependente sales(vendas)
# carregando os dados
dados_b <- ISLR::Carseats
names(dados_b)
str(dados_b)

# modelo
modelo_vazio_b <- lm(Sales ~ 1, data = dados_b)
modelo_completo_b <- lm(Sales ~ CompPrice + Income + Advertising + Population + Price + Age + Education, data=dados_b)

# modelo melhorado
step_forward_b <- step(modelo_vazio_b,
                       scope = formula(modelo_completo_b),
                       direction = 'forward',
                       trace = 0)

step_backward_b <- step(modelo_completo_b,
                        direction = 'backward',
                        trace = 0)

step_both_b <- step(modelo_completo_b,
                    direction = 'both',
                    trace = 0)

# verificar qual esta melhor
formula(step_forward_b)
formula(step_backward_b)
formula(step_both_b)

summary(step_forward_b)
summary(step_backward_b)
summary(step_both_b)

# predição manual
b0_b<-step_forward_b$coefficients[1]
b1_b<-step_forward_b$coefficients[2]
b2_b<-step_forward_b$coefficients[3]
b3_b<-step_forward_b$coefficients[4]
b4_b<-step_forward_b$coefficients[5]
b5_b<-step_forward_b$coefficients[6]

predicao_b2<-b0_b+b1_b*dados_b$Price+b2_b*dados_b$CompPrice+b3_b*dados_b$Advertising+b4_b*dados_b$Age+b5_b*dados_b$Income
predicao_b2

# predição
predicao_b1 <- predict(step_forward_b, newdata = dados_b)
predicao_b1

predicaodados_b <- data.frame(Sales = dados_b$Sales,
                              predicao1 = predicao_b1,
                              predicao2 = predicao_b2)
predicaodados_b

# calcular o R
RSS_b <- sum((dados_b$Sales-predicaodados_b$predicao1)^2)
TSS_b <- sum((dados_b$Sales-mean(dados_b$Sales))^2)
Rquadrado_b <- 1-RSS_b/TSS_b
Rquadrado_b

# c) ISLR::Credit, definindo como variável dependente balance(saldo do cartão de credito)
# carregando os dados
dados_c <- ISLR::Credit
names(dados_c)
str(dados_c)

# modelo
modelo_vazio_c <- lm(Balance ~ 1, data = dados_c)
modelo_completo_c <- lm(Balance ~ Income + Limit + Rating + Cards + Age + Education, data=dados_c)

# modelo melhorado
step_forward_c <- step(modelo_vazio_c,
                       scope = formula(modelo_completo_c),
                       direction = 'forward',
                       trace = 0)

step_backward_c <- step(modelo_completo_c,
                        direction = 'backward',
                        trace = 0)

step_both_c <- step(modelo_completo_c,
                    direction = 'both',
                    trace = 0)

# verificar qual esta melhor
formula(step_forward_c)
formula(step_backward_c)
formula(step_both_c)

summary(step_forward_c)
summary(step_backward_c)
summary(step_both_c)

# predição manual
b0_c<-step_forward_c$coefficients[1]
b1_c<-step_forward_c$coefficients[2]
b2_c<-step_forward_c$coefficients[3]
b3_c<-step_forward_c$coefficients[4]
b4_c<-step_forward_c$coefficients[5]
b5_c<-step_forward_c$coefficients[6]

predicao_c2<-b0_c+b1_c*dados_c$Rating+b2_c*dados_c$Income+b3_c*dados_c$Limit+b4_c*dados_c$Age+b5_c*dados_c$Cards
predicao_c2

# predição
predicao_c1 <- predict(step_forward_c, newdata = dados_c)
predicao_c1

predicaodados_c <- data.frame(Balance = dados_c$Balance,
                              predicao1 = predicao_c1,
                              predicao2 = predicao_c2)
predicaodados_c

# calcular o R
RSS_b <- sum((dados_b$Sales-predicaodados_b$predicao1)^2)
TSS_b <- sum((dados_b$Sales-mean(dados_b$Sales))^2)
Rquadrado_b <- 1-RSS_b/TSS_b
Rquadrado_b

# d) ISRL::Hitters, definindo como variável dependente salary(salário dos jogadores de beisebol)
# carregando os dados
dados_d <- ISLR::Hitters
names(dados_d)
str(dados_d)

# modelo
modelo_vazio_d <- lm(Salary ~ 1, data = dados_d)
modelo_completo_d <- lm(Salary ~ AtBat + Hits + HmRun + Runs + RBI + Walks + Years + CAtBat + CHits + CHmRun + CRuns + CRBI + CWalks + PutOuts + Assists + Errors, data=dados_d)

# modelo melhorado
step_forward_d <- step(modelo_vazio_d,
                       scope = formula(modelo_completo_d),
                       direction = 'forward',
                       trace = 0)

step_backward_d <- step(modelo_completo_d,
                        direction = 'backward',
                        trace = 0)

step_both_d <- step(modelo_completo_d,
                    direction = 'both',
                    trace = 0)

# verificar qual esta melhor
formula(step_forward_d)
formula(step_backward_d)
formula(step_both_d)

summary(step_forward_d)
summary(step_backward_d)
summary(step_both_d)

# Y aproximado
b0_d<-step_forward_d$coefficients[1]
b1_d<-step_forward_d$coefficients[2]
b2_d<-step_forward_d$coefficients[3]
b3_d<-step_forward_d$coefficients[4]
b4_d<-step_forward_d$coefficients[5]
b5_d<-step_forward_d$coefficients[6]

yaprox_d<-b0_d+b1_d*dados_d$CRBI+b2_d*dados_d$Hits+b3_d*dados_d$PutOuts+b4_d*dados_d$AtBat+b5_d*dados_d$Walks
yaprox_d

# e) ISLR2::Wage, definindo como variável dependente wage(salário)
# carregando os dados
dados_e <- ISLR2::Wage
names(dados_e)
str(dados_e)

# modelo
modelo_vazio_e <- lm(wage ~ 1, data = dados_e)
modelo_completo_e <- lm(wage ~ year + age + logwage, data=dados_e)

# modelo melhorado
step_forward_e <- step(modelo_vazio_e,
                       scope = formula(modelo_completo_e),
                       direction = 'forward',
                       trace = 0)

step_backward_e <- step(modelo_completo_e,
                        direction = 'backward',
                        trace = 0)

step_both_e <- step(modelo_completo_e,
                    direction = 'both',
                    trace = 0)

# verificar qual esta melhor
formula(step_forward_e)
formula(step_backward_e)
formula(step_both_e)

summary(step_forward_e)
summary(step_backward_e)
summary(step_both_e)

# Y aproximado
b0_e<-step_forward_e$coefficients[1]
b1_e<-step_forward_e$coefficients[2]
b2_e<-step_forward_e$coefficients[3]

yaprox_e<-b0_e+b1_e*dados_e$age+b2_e*dados_e$logwage
yaprox_e

# f) ISLR2::College, definindo como variável dependente outstate(mensalidade de aluno de fora do estado)
# carregando os dados
dados_f <- ISLR2::College
names(dados_f)
str(dados_f)

# modelo
modelo_vazio_f <- lm(Outstate ~ 1, data = dados_f)
modelo_completo_f <- lm(Outstate ~ Apps + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + P.Undergrad + Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate, data=dados_f)

# modelo melhorado
step_forward_f <- step(modelo_vazio_f,
                       scope = formula(modelo_completo_f),
                       direction = 'forward',
                       trace = 0)

step_backward_f <- step(modelo_completo_f,
                        direction = 'backward',
                        trace = 0)

step_both_f <- step(modelo_completo_f,
                    direction = 'both',
                    trace = 0)

# verificar qual esta melhor
formula(step_forward_f)
formula(step_backward_f)
formula(step_both_f)

summary(step_forward_f)
summary(step_backward_f)
summary(step_both_f)

# Y aproximado
b0_f<-step_forward_f$coefficients[1]
b1_f<-step_forward_f$coefficients[2]
b2_f<-step_forward_f$coefficients[3]
b3_f<-step_forward_f$coefficients[4]
b4_f<-step_forward_f$coefficients[5]
b5_f<-step_forward_f$coefficients[6]
b6_f<-step_forward_f$coefficients[7]
b7_f<-step_forward_f$coefficients[8]
b8_f<-step_forward_f$coefficients[9]
b9_f<-step_forward_f$coefficients[10]
b10_f<-step_forward_f$coefficients[11]
b11_f<-step_forward_f$coefficients[12]
b12_f<-step_forward_f$coefficients[13]

yaprox_f<-b0_f+b1_f*dados_f$Expend+b2_f*dados_f$Room.Board+b3_f*dados_f$perc.alumni+b4_f*dados_f$Grad.Rate+b5_f*dados_f$F.Undergrad+b6_f*dados_f$Accept+b7_f*dados_f$Apps+b8_f*dados_f$Top10perc+b9_f*dados_f$S.F.Ratio+b10_f*dados_f$Terminal+b11_f*dados_f$Personal+b12_f*dados_f$Enroll
yaprox_f

# g) ISRL2::OJ, definindo como variável dependente PriceMM()
# carregando os dados
dados_g <- ISLR2::OJ
names(dados_g)
str(dados_g)

# modelo
modelo_vazio_g <- lm(PriceMM ~ 1, data = dados_g)
modelo_completo_g <- lm(PriceMM ~ WeekofPurchase + StoreID + PriceCH + DiscCH + DiscMM + SpecialCH + SpecialMM + LoyalCH + SalePriceMM + SalePriceCH + PriceDiff + PctDiscMM + PctDiscCH + ListPriceDiff + STORE, data=dados_g)

# modelo melhorado
step_forward_g <- step(modelo_vazio_g,
                       scope = formula(modelo_completo_g),
                       direction = 'forward',
                       trace = 0)

step_backward_g <- step(modelo_completo_g,
                        direction = 'backward',
                        trace = 0)

step_both_g <- step(modelo_completo_g,
                    direction = 'both',
                    trace = 0)

# verificar qual esta melhor
formula(step_forward_g)
formula(step_backward_g)
formula(step_both_g)

summary(step_forward_g)
summary(step_backward_g)
summary(step_both_g)

# Y aproximado
b0_g<-step_forward_g$coefficients[1]
b1_g<-step_forward_g$coefficients[2]
b2_g<-step_forward_g$coefficients[3]

yaprox_g<-b0_g+b1_g*dados_g$ListPriceDiff+b2_g*dados_g$PriceCH
yaprox_g