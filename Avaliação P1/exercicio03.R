# Verificar cada variável que aparece como categórica e transformar em factor; 
# construir um modelo adequado; 
# melhorar o modelo se possível;
# realizar a predição de duas maneiras distintas.

# ==============================================================================
# Exercício 3 – Heart Disease (Doença cardíaca)
# ==============================================================================

# O Heart Disease contém dados clínicos de mais de 300 pacientes avaliados
# para risco cardíaco. Algumas variáveis são:

# - age: idade do paciente.
# - sex: sexo (0=feminino, 1=masculino).
# - cp: tipo de dor no peito (0–3).
# - trestbps: pressão arterial em repouso.
# - chol: nível de colesterol sérico.
# - thalach: frequência cardíaca máxima atingida.
# - exang: angina induzida por exercício (0/1).
# - oldpeak: depressão do segmento ST.
# - ca: número de vasos principais coloridos por fluoroscopia.
# - target: presença de doença cardíaca (0=ausente, 1=presente).

# Ajuste um modelo de regressão logística binária para prever a
# probabilidade de doença cardíaca. variável dependente: target.

# LIMPEZA ======================================================================
rm(list=ls())

# BIBLIOTECAS ==================================================================
library(readr)

# CARREGAR OS DADOS ============================================================
PATH <- "/home/roger/Documentos/Teoria do Aprendizado Estatistico/Avaliação P1"
setwd(PATH)

dados <- read_csv("heart_disease_uci.csv", locale = locale(encoding = "LATIN1"))
dados <- as.data.frame(dados)
names(dados)
str(dados)

dados$sex <- as.factor(dados$sex)
dados$cp <- as.factor(dados$cp)
dados$fbs <- as.factor(dados$fbs)
dados$restecg <- as.factor(dados$restecg)
dados$exang <- as.factor(dados$exang)
dados$slope <- as.factor(dados$slope)
dados$ca <- as.factor(dados$ca)
dados$thal <- as.factor(dados$thal)
dados$target <- as.factor(dados$target)

str(dados)

# SEPARANDO EM TREINO E TESTE ==================================================
set.seed(123) # reprodutibilidade
particao <- sample(seq_len(nrow(dados)), size = 0.7 * nrow(dados))
dados_treino <- dados[particao, ]
dados_teste <- dados[-particao, ]
prop.table(table(dados_treino$target))
prop.table(table(dados_teste$target))

# GERANDO MODELO ===============================================================
modelo <- glm(target~.,
                data = dados_treino,
                family = binomial())
formula(modelo)
summary(modelo)

# PREDIÇÃO =====================================================================
prob_modelo <- predict(modelo,
                         newdata = dados_teste,
                         type = "response")

prob_modelo
