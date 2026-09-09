#Aula TAE 005

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