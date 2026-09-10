#Aula TEA 001 Regressão Linear com variaveis factor

#limpeza
rm(list=ls())

# dataset embutido no R

dados001 <- mtcars
str(dados001)
names(dados001)
dados001$cyl<-as.factor(dados001$cyl)
dados001
dados001$cyl
str(dados001)


#modelo
modelo_vazio <- lm(mpg ~ 1, data = dados001)
modelo_completo <- lm(mpg ~ wt + hp + disp + drat + qsec + cyl + vs + am + gear + carb, 
                      data = dados001)
summary(modelo_completo)


#procura do melhor modelo
step_forward <- step(modelo_vazio, 
                     scope=formula(modelo_completo), 
                     direction='forward', 
                     trace=0)
step_backward <- step(modelo_completo, 
                      direction='backward', 
                      trace=0)
step_both<- step(modelo_completo, 
                 direction = "both",
                 trace=0)


formula(step_forward)
formula(step_backward)
formula(step_both)

summary(step_forward)
summary(step_backward)
summary(step_both)


#predicao manual
step_forward$coefficients
b0<-step_forward$coefficients[1]
b1<-step_forward$coefficients[2]
b2<-step_forward$coefficients[3]
b3<-step_forward$coefficients[4]
b4<-step_forward$coefficients[5]
b5<-step_forward$coefficients[6]

dados001$cyl
dados001$cyl==4

predicao002<-
  b0 + 
  b1*dados001$wt + 
  b2*(dados001$cyl==6) +
  b3*(dados001$cyl==8) +
  b4*dados001$hp +
  b5*dados001$am
predicao002

#predição
predicao001<-predict(step_forward,
                     newdata =dados001)

predicaodados<-data.frame(mpg =dados001$mpg, 
                          predicao1 = predicao001, 
                          predicao2 = predicao002)
predicaodados

#calcular o R
RSS<-sum((dados001$mpg-predicaodados$predicao1)^2)
TSS<-sum((dados001$mpg-mean(dados001$mpg))^2)
Rquadrado<-1-RSS/TSS
Rquadrado






# b) ISLR::Carsets, definindo como variável dependente sales(vendas)
# carregando os dados
dados_b <- ISLR::Carseats
names(dados_b)
str(dados_b)

# modelo
modelo_vazio_b <- lm(Sales ~ 1, data = dados_b)
modelo_completo_b <- lm(Sales ~ ., data=dados_b)

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
# formula(step_both_b)

summary(step_forward_b)
summary(step_backward_b)
summary(step_both_b)

# predição manual
b0_b<-step_backward_b$coefficients[1]
b1_b<-step_backward_b$coefficients[2]
b2_b<-step_backward_b$coefficients[3]
b3_b<-step_backward_b$coefficients[4]
b4_b<-step_backward_b$coefficients[5]
b5_b<-step_backward_b$coefficients[6]
b6_b<-step_backward_b$coefficients[7]
b7_b<-step_backward_b$coefficients[8]



predicao_b2 <- b0_b +
  b1_b*dados_b$CompPrice +
  b2_b*dados_b$Income +
  b3_b*dados_b$Advertising +
  b4_b*dados_b$Price +
  b5_b*(dados_b$ShelveLoc=="Good") +
  b6_b*(dados_b$ShelveLoc=="Medium") +
  b7_b*dados_b$Age

predicao_b2

# predição
predicao_b1 <- predict(step_backward_b, newdata = dados_b)
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
modelo_completo_c <- lm(Balance ~ ., data=dados_c)

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
b6_c<-step_forward_c$coefficients[7]

predicao_c2<-b0_c +
  b1_c*dados_c$Rating +
  b2_c*dados_c$Income +
  b3_c*(dados_c$Student=="Yes") +
  b4_c*dados_c$Limit +
  b5_c*dados_c$Cards +
  b6_c*dados_c$Age

predicao_c2

# predição
predicao_c1 <- predict(step_forward_c, newdata = dados_c)
predicao_c1

predicaodados_c <- data.frame(Balance = dados_c$Balance,
                              predicao1 = predicao_c1,
                              predicao2 = predicao_c2)
predicaodados_c

# calcular o R
RSS_c <- sum((dados_c$Balance-predicaodados_c$predicao1)^2)
TSS_c <- sum((dados_c$Balance-mean(dados_c$Balance))^2)
Rquadrado_c <- 1-RSS_c/TSS_c
Rquadrado_c

# d) ISRL::Hitters, definindo como variável dependente salary(salário dos jogadores de beisebol)
# carregando os dados
dados_d <- ISLR::Hitters
names(dados_d)
str(dados_d)
dados_d <- na.omit(dados_d)

# modelo
modelo_vazio_d <- lm(Salary ~ 1, data = dados_d)
modelo_completo_d <- lm(Salary ~ ., data=dados_d)

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

# predição manual
b0_d<-step_forward_d$coefficients[1]
b1_d<-step_forward_d$coefficients[2]
b2_d<-step_forward_d$coefficients[3]
b3_d<-step_forward_d$coefficients[4]
b4_d<-step_forward_d$coefficients[5]
b5_d<-step_forward_d$coefficients[6]
b6_d<-step_forward_d$coefficients[7]
b7_d<-step_forward_d$coefficients[8]
b8_d<-step_forward_d$coefficients[9]
b9_d<-step_forward_d$coefficients[10]
b10_d<-step_forward_d$coefficients[11]

predicao_d2<-b0_d +
  b1_d*dados_d$CRBI +
  b2_d*dados_d$Hits +
  b3_d*dados_d$PutOuts +
  b4_d*(dados_d$Division=="W") +
  b5_d*dados_d$AtBat +
  b6_d*dados_d$Walks +
  b7_d*dados_d$CWalks +
  b8_d*dados_d$CRuns +
  b9_d*dados_d$CAtBat +
  b10_d*dados_d$Assists
  
predicao_d2

# predição
predicao_d1 <- predict(step_forward_d, newdata = dados_d)
predicao_d1

predicaodados_d <- data.frame(Salary = dados_d$Salary,
                              predicao1 = predicao_d1,
                              predicao2 = predicao_d2)
predicaodados_d

# calcular o R
RSS_d <- sum((dados_d$Salary-predicaodados_d$predicao1)^2)
TSS_d <- sum((dados_d$Salary-mean(dados_d$Salary))^2)
Rquadrado_d <- 1-RSS_d/TSS_d
Rquadrado_d

# e) ISLR2::Wage, definindo como variável dependente wage(salário)
# carregando os dados
dados_e <- ISLR2::Wage
names(dados_e)
str(dados_e)

dados_e$region <- NULL
dados_e$logwage <- NULL

# modelo
modelo_vazio_e <- lm(wage ~ 1, data = dados_e)
modelo_completo_e <- lm(wage ~ ., data = dados_e)


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

# predição manual
b0_e<-step_forward_e$coefficients[1]
b1_e<-step_forward_e$coefficients[2]
b2_e<-step_forward_e$coefficients[3]
b3_e<-step_forward_e$coefficients[4]
b4_e<-step_forward_e$coefficients[5]
b5_e<-step_forward_e$coefficients[6]
b6_e<-step_forward_e$coefficients[7]
b7_e<-step_forward_e$coefficients[8]
b8_e<-step_forward_e$coefficients[9]
b9_e<-step_forward_e$coefficients[10]
b10_e<-step_forward_e$coefficients[11]
b11_e<-step_forward_e$coefficients[12]
b12_e<-step_forward_e$coefficients[13]
b13_e<-step_forward_e$coefficients[14]
b14_e<-step_forward_e$coefficients[15]
b15_e<-step_forward_e$coefficients[16]
b16_e<-step_forward_e$coefficients[17]

predicao_e2<-b0_e +
  b1_e*(dados_e$education=="2. HS Grad") +
  b2_e*(dados_e$education=="3. Some College") +
  b3_e*(dados_e$education=="4. College Grad") +
  b4_e*(dados_e$education=="5. Advanced Degree") +
  b5_e*(dados_e$maritl=="2. Married")  +
  b6_e*(dados_e$maritl=="3. Widowed") +
  b7_e*(dados_e$maritl=="4. Divorced") +
  b8_e*(dados_e$maritl=="5. Separated") +
  b9_e*(dados_e$health_ins=="2. No") +
  b10_e*dados_e$year +
  b11_e*(dados_e$health=="2. >=Very Good") +
  b12_e*dados_e$age +
  b13_e*(dados_e$jobclass=="2. Information") +
  b14_e*(dados_e$race=="2. Black") +
  b15_e*(dados_e$race=="3. Asian") +
  b16_e*(dados_e$race=="4. Other")
  
predicao_e2

# predição
predicao_e1 <- predict(step_forward_e, newdata = dados_e)
predicao_e1

predicaodados_e <- data.frame(wage = dados_e$wage,
                              predicao1 = predicao_e1,
                              predicao2 = predicao_e2)
predicaodados_e

# calcular o R
RSS_e <- sum((dados_e$wage-predicaodados_e$predicao1)^2)
TSS_e <- sum((dados_e$wage-mean(dados_e$wage))^2)
Rquadrado_e <- 1-RSS_e/TSS_e
Rquadrado_e
