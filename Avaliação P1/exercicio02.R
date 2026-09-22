# Verificar cada variável que aparece como categórica e transformar em factor; 
# construir um modelo adequado; 
# melhorar o modelo se possível;
# realizar a predição de duas maneiras distintas.

# ==============================================================================
# Exercício 2 – Boston Housing (Preço de imóveis)
# ==============================================================================

# O Boston Housing reúne dados de 506 bairros de Boston. As variáveis incluem:

# - crim: taxa de criminalidade.
# - zn: proporção de terrenos residenciais zoneados para lotes com mais de
# 25.000 pés quadrados.
# - indus: proporção de acres destinados a atividades comerciais
# - chas: Rio Charles (= 1 se a área faz divisa com o rio; 0 caso contrário)
# - nox: concentração de óxidos nítricos (poluição).
# - rm: número médio de cômodos por residência.
# - age: proporção de unidades ocupadas construídas antes de 1940.
# - dis: média ponderada das distâncias até cinco centros de emprego de Boston.
# - rad: índice de acessibilidade a rodovias radiais.
# - tax: taxa de imposto sobre imóveis.
# - ptratio: razão aluno-professor por município.
# - lstat: percentual de população de baixa renda.
# - medv: valor mediano das casas (em milhares de dólares).

# Construa um modelo de regressão para prever o preço das casas (medv).

# LIMPEZA ======================================================================
rm(list=ls())

# BIBLIOTECAS ==================================================================
library(ISLR)
library(ISLR2)

# CARREGAR OS DADOS ============================================================
dados <- Boston
names(dados)
str(dados)
dados$chas<-as.factor(dados$chas)
dados$rad<-as.factor(dados$rad)

# MODELOS ======================================================================
modelo_vazio <- lm(medv ~ 1, data = dados)
modelo_completo <- lm(medv ~ ., data = dados)

# MELHORANDO MODELO ============================================================
step_forward <- step(modelo_vazio,
                     scope = formula(modelo_completo),
                     direction = 'forward',
                     trace = 0)

step_backward <- step(modelo_completo,
                      direction = 'backward',
                      trace = 0)

step_both <- step(modelo_completo,
                  direction = 'both',
                  trace = 0)

# VERIFICANDO MELHOR MODELO ====================================================
formula(step_forward)
formula(step_backward)
formula(step_both)

summary(step_both)

# PREDIÇÃO MANUAL ==============================================================
b0<-step_both$coefficients[1]
b1<-step_both$coefficients[2]
b2<-step_both$coefficients[3]
b3<-step_both$coefficients[4]
b4<-step_both$coefficients[5]
b5<-step_both$coefficients[6]
b6<-step_both$coefficients[7]
b7<-step_both$coefficients[8]
b8<-step_both$coefficients[9]
b9<-step_both$coefficients[10]
b10<-step_both$coefficients[11]
b11<-step_both$coefficients[12]
b12<-step_both$coefficients[13]
b13<-step_both$coefficients[14]
b14<-step_both$coefficients[15]
b15<-step_both$coefficients[16]
b16<-step_both$coefficients[17]
b17<-step_both$coefficients[18]

predicao_2 <- b0 +
  b1*dados$crim +
  b2*dados$zn +
  b3*(dados$chas==1) +
  b4*dados$nox +
  b5*dados$rm +
  b6*dados$dis +
  b7*(dados$rad==2) +
  b8*(dados$rad==3) +
  b9*(dados$rad==4) +
  b10*(dados$rad==5) +
  b11*(dados$rad==6) +
  b12*(dados$rad==7) +
  b13*(dados$rad==8) +
  b14*(dados$rad==24) +
  b15*dados$tax +
  b16*dados$ptratio +
  b17*dados$lstat

# PREDIÇÃO =====================================================================
predicao_1 <- predict(step_both, newdata = dados)
predicao_1

predicaodados <- data.frame(mpg = dados$medv,
                            predicao1 = predicao_1,
                            predicao2 = predicao_2)
predicaodados
