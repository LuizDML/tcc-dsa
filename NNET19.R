# Carregar pacotes necessários
library(neuralnet)
library(dplyr)

# Função de normalização
normalizar <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Manter apenas as colunas relevantes para o modelo
esc2019_deep <- escolas2019[, -c(1,2,3,4,5,7,8,9,11,12,13,15,16,19,20,21,22,23,24,25,26,27,28,29,31,32)]

# Normalizar as variáveis numéricas
esc2019_deep <- esc2019_deep %>%
  mutate(across(c(ALE, MD_ANOS_C, MD_IDADE, P_POSGRAD, PEI, PERC_AUSENCIA, PERC_FIXOS_TEMP, MD_AT19, M_ATING_AA), normalizar))

# Dividir os dados em treino (80%) e teste (20%)
set.seed(123)
rm(train_index)
rm(train_data)
rm(test_data)
train_index <- sample(1:nrow(esc2019_deep), 0.8 * nrow(esc2019_deep))
train_data <- esc2019_deep[train_index, ]
test_data <- esc2019_deep[-train_index, ]

# Preparar a fórmula para o modelo neural
formula_nn <- as.formula(paste("M_ATING ~", paste(names(train_data)[names(train_data) != "M_ATING"], collapse = " + ")))

# Treinar o modelo com o neuralnet
nn_model <- neuralnet(formula_nn,
                      data = train_data,
                      hidden = c(12, 32),  # Definindo camadas ocultas (12 neurônios na primeira e 32 na segunda)
                      linear.output = FALSE,
                      act.fct = "logistic")

# nn_model <- neuralnet(formula_nn, 
#                       data = train_data, 
#                       hidden = c(8, 16),  # Número reduzido de neurônios
#                       linear.output = FALSE, 
#                       act.fct = "logistic", 
#                       stepmax = 1e5,  # Aumenta o número de iterações
#                       rep = 5)  # Tentar o modelo múltiplas vezes para melhorar a convergência

# Visualizar o modelo
plot(nn_model)

# Fazer previsões no conjunto de teste
nn_predictions <- compute(nn_model, test_data[, -which(names(test_data) == "M_ATING")])$net.result

# Converter previsões para 0 ou 1 com um limite de 0.5
nn_predictions_bin <- ifelse(nn_predictions > 0.5, 1, 0)

# Gerar a matriz de confusão
matriz_conf19 <- table(PREVISÃO = nn_predictions_bin, REAL = test_data$M_ATING)
print(matriz_conf19)

# Avaliar o desempenho do modelo
accuracy <- sum(diag(matriz_conf18)) / sum(matriz_conf18)
cat("Acurácia do modelo:", accuracy)

