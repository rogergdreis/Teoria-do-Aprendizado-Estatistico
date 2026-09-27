# Verificar cada variável que aparece como categórica e transformar em factor; 
# construir um modelo adequado; 
# melhorar o modelo se possível;
# realizar a predição de duas maneiras distintas.

# ==============================================================================
# Exercício 2 – Boston Housing (Preço de imóveis)
# ==============================================================================

# O Boston Housing reúne dados de 506 bairros de Boston. As variáveis incluem:

# - crim: taxa de criminalidade.
# - nox: concentração de óxidos nítricos (poluição).
# - rm: número médio de cômodos por residência.
# - age: proporção de unidades ocupadas construídas antes de 1940.
# - tax: taxa de imposto sobre imóveis.
# - ptratio: razão aluno-professor por município.
# - lstat: percentual de população de baixa renda.
# - medv: valor mediano das casas (em milhares de dólares).

# Construa um modelo de regressão para prever o preço das casas (medv).

# LIMPEZA ======================================================================
rm(list=ls())

# BIBLIOTECAS ==================================================================
library(readr)

# CARREGAR OS DADOS ============================================================
PATH <- "/home/roger/Documentos/Teoria do Aprendizado Estatistico/Avaliação P1"
setwd(PATH)

dados <- read_csv("boston_housing.csv", locale = locale(encoding = "LATIN1"))
dados <- as.data.frame(dados)
names(dados)
str(dados)

dados$chas <- as.factor(dados$chas)
dados$rad <- as.factor(dados$rad)

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

predicao_2 <- b0 +
  b1*dados$zn +
  b2*(dados$chas==1) +
  b3*dados$nox +
  b4*dados$rm +
  b5*dados$dis +
  b6*dados$tax +
  b7*dados$lstat

# PREDIÇÃO =====================================================================
predicao_1 <- predict(step_both, newdata = dados)
predicao_1

predicaodados <- data.frame(mpg = dados$medv,
                            predicao1 = predicao_1,
                            predicao2 = predicao_2)
predicaodados
