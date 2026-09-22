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
library(funModeling)

# CARREGAR OS DADOS ============================================================
# URL oficial do dataset de Cleveland
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

# Nomes das 14 variáveis originais
nomes_colunas <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", 
                   "thalach", "exang", "oldpeak", "slope", "ca", "thal", "target")

# Importa os dados tratando os valores "?" como NA
dados <- read.csv(url, header = FALSE, col.names = nomes_colunas, na.strings = "?")

# Confere se a variável target está lá
names(dados)
str(dados)

# Gerando aleatório
nrow(dados_d)
seq_len(nrow(dados_d))

# Separando em treino e teste
set.seed(123) # reprodutibilidade
particao_d <- sample(seq_len(nrow(dados_d)), size = 0.7 * nrow(dados_d))
dados_treino_d <- dados_d[particao_d, ]
dados_teste_d <- dados_d[-particao_d, ]
prop.table(table(dados_treino_d$Purchase))
prop.table(table(dados_teste_d$Purchase))

# Gerando o modelo
modelo_d <- glm(Purchase~.,
                data = dados_treino_d,
                family = binomial())
formula(modelo_d)
summary(modelo_d)

# Predição
prob_modelo_d <- predict(modelo_d,
                         newdata = dados_teste_d,
                         type = "response")

prob_modelo_d
