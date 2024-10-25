# Normalizando os dados
normalizar <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# manter só as colunas estritamente relevantes para o modelo
esc2022_deep <- escolas2022[, -c(1,2,3,4,5,7,8,9,11,12,13,15,16,19,20,21,22,23,24,25,26,27,28,29,31,32)]  # 2 DE, 29 P_MD

esc2022_deep <- esc2022_deep %>%
  #mutate(DE = as.numeric(as.factor(DE))) #%>% # Converte DE para numérico se necessário 
  mutate(across(c( ALE, MD_ANOS_C, P_POSGRAD, PEI, PERC_AUSENCIA, PERC_FIXOS_TEMP, MD_AT22), normalizar))

#esc2022_deep <- dummy_cols(esc2022_deep, select_columns = "DE", remove_first_dummy = TRUE, remove_selected_columns = TRUE)

set.seed(123)  # Para reprodutibilidade
rm(train_index)
rm(train_data)
rm(test_data)
train_index <- sample(1:nrow(esc2022_deep), 0.8 * nrow(esc2022_deep))
train_data <- esc2022_deep[train_index, ]
test_data <- esc2022_deep[-train_index, ]

# modelo de rede 
model <- keras_model_sequential() %>%
  layer_dense(units = 12, activation = 'relu', input_shape = c(ncol(train_data) - 1)) %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')  # Saída binária para prever 0 ou 1 - se atingiu a meta ou não

model %>% compile(
  loss = 'binary_crossentropy',  # Porque estamos prevendo uma classe binária
  optimizer = 'adam',
  metrics = c('accuracy')
)

# treino do modelo
history <- model %>% fit(
  as.matrix(train_data[, -which(names(train_data) == "M_ATING")]),  # Previsores
  as.matrix(train_data$M_ATING),  # Variável alvo
  epochs = 100,
  batch_size = 64,
  validation_split = 0.2
)

# avaliar desempenho do modelo
model %>% evaluate(as.matrix(test_data[, -which(names(test_data) == "M_ATING")]), 
                   as.matrix(test_data$M_ATING))

# tf 2.5
# predictions <- model %>% predict_classes(as.matrix(test_data[, -which(names(test_data) == "M_ATING")]))

# tf 2.6
predictions <- model %>% predict(as.matrix(test_data[, -which(names(test_data) == "M_ATING")])) %>% `>`(0.5) %>% k_cast("int32")
# foi necessário mudar o cutoff para obter a matriz de confusão

# Comparar com os valores reais
real <- test_data$M_ATING

# Convertendo para vetor, se necessário
predictions <- as.vector(predictions)

# Gerando a matriz de confusão
matriz_conf22 <- table(PREVISÃO = predictions, REAL = real)
print(matriz_conf22)

# Carregar o pacote pROC
library(pROC)

# Calcular as previsões probabilísticas
pred_probs <- model %>% predict(as.matrix(test_data[, -which(names(test_data) == "M_ATING")]))


# Calcular a curva ROC e a AUC
roc_curve <- roc(real, pred_probs)
auc_value <- auc(roc_curve)

# Extraindo TP, TN, FP, FN
TN <- matriz_conf22[1, 1]  # Verdadeiro Negativo
FP <- matriz_conf22[1, 2]  # Falso Positivo
FN <- matriz_conf22[2, 1]  # Falso Negativo
TP <- matriz_conf22[2, 2]  # Verdadeiro Positivo

# Calculando métricas
acuracia <- (TP + TN) / (TP + TN + FP + FN)
sensitividade <- TP / (TP + FN)
especificidade <- TN / (TN + FP)
precisao <- TP / (TP + FP)
f1_score <- 2 * (precisao * sensitividade) / (precisao + sensitividade)

# Criando um dataframe com os resultados
res_nn22 <- data.frame(
  Acuracia = acuracia,
  Sensitividade = sensitividade,
  Especificidade = especificidade,
  F1_Score = f1_score,
  AUC = auc_value
)

print(res_nn22)

# Obter os pesos da primeira camada densa
pesos <- model %>% get_weights()

# A primeira camada contém os pesos para cada uma das features
pesos[[1]]  # Pesos entre as features de entrada e a primeira camada densa