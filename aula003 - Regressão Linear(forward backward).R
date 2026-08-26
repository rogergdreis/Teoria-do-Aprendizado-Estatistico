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

# Y aproximado
b0<-step_backward$coefficients[1]
b1<-step_backward$coefficients[2]
b2<-step_backward$coefficients[3]
b3<-step_backward$coefficients[4]

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

# Y aproximado
b0_a<-step_forward_a$coefficients[1]
b1_a<-step_forward_a$coefficients[2]
b2_a<-step_forward_a$coefficients[3]
b3_a<-step_forward_a$coefficients[4]

yaprox_a<-b0_a+b1_a*dados_a$weight+b2_a*dados_a$year+b3_a*dados_a$origin
yaprox_a

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

# Y aproximado
b0_b<-step_forward_b$coefficients[1]
b1_b<-step_forward_b$coefficients[2]
b2_b<-step_forward_b$coefficients[3]
b3_b<-step_forward_b$coefficients[4]
b4_b<-step_forward_b$coefficients[5]
b5_b<-step_forward_b$coefficients[6]

yaprox_b<-b0_b+b1_b*dados_b$CompPrice+b2_b*dados_b$Income+b3_b*dados_b$Advertising+b4_b*dados_b$Price+b5_b*dados_b$Age
yaprox_b

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

# Y aproximado
b0_c<-step_forward_c$coefficients[1]
b1_c<-step_forward_c$coefficients[2]
b2_c<-step_forward_c$coefficients[3]
b3_c<-step_forward_c$coefficients[4]
b4_c<-step_forward_c$coefficients[5]
b5_c<-step_forward_c$coefficients[6]

yaprox_c<-b0_c+b1_c*dados_c$Rating+b2_c*dados_c$Income+b3_c*dados_c$Limit+b4_c*dados_c$Age+b5_c*dados_c$Cards
yaprox_c

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
