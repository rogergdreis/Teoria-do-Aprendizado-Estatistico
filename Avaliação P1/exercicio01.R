# Verificar cada variável que aparece como categórica e transformar em factor; 
# construir um modelo adequado; 
# melhorar o modelo se possível;
# realizar a predição de duas maneiras distintas.

# ==============================================================================
# Exercício 1 – Auto MPG (Consumo de combustível)
# ==============================================================================
# O conjunto Auto MPG contém informações de cerca de 400 automóveis
# fabricados entre 1970 e 1982. As variáveis incluem:
  
# - mpg: consumo de combustível (milhas por galão).
# - cylinders: número de cilindros do motor.
# - displacement: cilindrada em polegadas cúbicas.
# - horsepower: potência do motor.
# - weight: peso do veículo em libras.
# - acceleration: tempo para ir de 0 a 60 mph.
# - model_year: ano do modelo.
# - origin: origem do veículo (1=EUA, 2=Europa, 3=Japão).

# Estime um modelo de regressão linear multivariada para explicar o
# consumo de combustível (mpg).
# (antes de gerar o modelo se pergunte qual variável seria interessante
# retirar do modelo)

# LIMPEZA ======================================================================
rm(list=ls())

# BIBLIOTECAS ==================================================================
library(readr)

# CARREGAR OS DADOS ============================================================
PATH <- "/home/roger/Documentos/Teoria do Aprendizado Estatistico/Avaliação P1"
setwd(PATH)

dados <- read_csv("autos.csv", locale = locale(encoding = "LATIN1"))
dados <- as.data.frame(dados)
names(dados)
str(dados)

dados$origin <- as.factor(dados$origin)

# MODELOS ======================================================================
modelo_vazio <- lm(mpg ~ 1, data = dados)
modelo_completo <- lm(mpg ~ . -origin -name, data = dados)

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

summary(step_forward)

# PREDIÇÃO MANUAL ==============================================================
b0<-step_forward$coefficients[1]
b1<-step_forward$coefficients[2]
b2<-step_forward$coefficients[3]

predicao_2 <- b0 +
  b1*dados$weight +
  b2*dados$horsepower

predicao_2

# PREDIÇÃO =====================================================================
predicao_1 <- predict(step_forward, newdata = dados)
predicao_1

predicaodados <- data.frame(mpg = dados$mpg,
                              predicao1 = predicao_1,
                              predicao2 = predicao_2)
predicaodados
