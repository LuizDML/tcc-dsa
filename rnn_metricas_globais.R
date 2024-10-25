esc_all_rnn <- escolas_all[, -c(1,2,3,4,5,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26)]
esc_all_rnn <- esc_all_rnn %>% drop_na()
summary(esc_all_rnn)

# Normalizando os dados
normalizar <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

esc_all_rnn <- esc_all_rnn %>%
  mutate(across(c( ALE, INDICE_EVOLUC, IND_INGR, IND_REGU, IND_ABAN, IND_INTE, MD_ATG), normalizar))

# dados de treino e teste
set.seed(123)  # Para reprodutibilidade
rm(train_index_all)
rm(train_data_all)
rm(test_data_all)
train_index_all <- sample(1:nrow(esc_all_rnn), 0.8 * nrow(esc_all_rnn))
train_data_all <- esc_all_rnn[train_index_all, ]
test_data_all <- esc_all_rnn[-train_index_all, ]

# Modificação da última camada para classificação multiclasse
model_all <- keras_model_sequential() %>%
  layer_dense(units = 12, activation = 'relu', input_shape = c(ncol(train_data_all) - 1)) %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dense(units = 5, activation = 'softmax')  # 5 saídas, uma para cada categoria

# Atualizar a função de perda para 'categorical_crossentropy'
model_all %>% compile(
  loss = 'categorical_crossentropy',  # Perda para classificação multiclasse
  optimizer = 'adam',
  metrics = c('accuracy')
)

# Treino do modelo, convertendo a variável target para uma codificação one-hot
train_labels <- to_categorical(train_data_all$QT_M_ATING, num_classes = 5)
test_labels <- to_categorical(test_data_all$QT_M_ATING, num_classes = 5)

history <- model_all %>% fit(
  as.matrix(train_data_all[, -which(names(train_data_all) == "QT_M_ATING")]),  # Previsores
  train_labels,  # Variável alvo com one-hot encoding
  epochs = 100,
  batch_size = 64,
  validation_split = 0.2
)

# Avaliar o desempenho do modelo
model_all %>% evaluate(as.matrix(test_data_all[, -which(names(test_data_all) == "QT_M_ATING")]), 
                       test_labels)


# Fazer predições no conjunto de teste
pred_probs <- model_all %>% predict(as.matrix(test_data_all[, -which(names(test_data_all) == "QT_M_ATING")]))

# Convertendo as probabilidades em classes preditas (pegando a classe com maior probabilidade)
pred_classes <- apply(pred_probs, 1, which.max) - 1  # Ajustando para o range 0-4

# Comparando com os valores reais
real_classes <- test_data_all$QT_M_ATING

# Verificando os comprimentos
length(pred_classes)  # Deve ser igual ao número de linhas no conjunto de teste
length(real_classes)   # Deve ser igual ao número de linhas no conjunto de teste

# Gerando a matriz de confusão
matriz_confusao <- table(PREVISÃO = pred_classes, REAL = real_classes)
print(matriz_confusao)

###############################################


# Instalar e carregar o pacote 'caret', se ainda não tiver
# install.packages("caret")
library(caret)

# Plotando a matriz de confusão
confusionMatrix(factor(pred_classes), factor(real_classes))  # Usando 'caret'

# Instalar e carregar o pacote 'pROC', se ainda não tiver
# install.packages("pROC")
library(pROC)

# Calcular a curva ROC para cada classe
multi_roc <- multiclass.roc(real_classes, pred_probs)

# Plotando as curvas ROC para todas as classes
plot.roc(multi_roc)

library(ggplot2)

# Criar um dataframe com os resultados
df_results <- data.frame(
  Real = factor(real_classes),
  Predito = factor(pred_classes)
)

# Gráfico de barras
ggplot(df_results, aes(x = Real, fill = Predito)) +
  geom_bar(position = "dodge") +
  labs(title = "Comparação das Classes Reais e Preditas", x = "Classe Real", y = "Contagem") +
  theme_minimal()
###############################################

# tf 2.5
# predictions <- model %>% predict_classes(as.matrix(test_data[, -which(names(test_data) == "M_ATING")]))

# tf 2.6
predictions <- model_all %>% predict(as.matrix(test_data_all[, -which(names(test_data_all) == "QT_M_ATING")])) %>% `>`(0.5) %>% k_cast("int32")
# foi necessário mudar o cutoff para obter a matriz de confusão

# Comparar com os valores reais
real <- test_data_all$QT_M_ATING

# Convertendo para vetor, se necessário
predictions <- as.vector(predictions)

# Gerando a matriz de confusão
matriz_conf_rnn_all <- table(PREVISÃO = predictions, REAL = real)
print(matriz_conf_rnn_all)

# Carregar o pacote pROC
library(pROC)

# Calcular as previsões probabilísticas
pred_probs <- model_all %>% predict(as.matrix(test_data_all[, -which(names(test_data_all) == "QT_M_ATING")]))


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
res_nn_all <- data.frame(
  Acuracia = acuracia,
  Sensitividade = sensitividade,
  Especificidade = especificidade,
  F1_Score = f1_score,
  AUC = auc_value
)

print(res_nn_all)

# Obter os pesos da primeira camada densa
pesos <- model_all %>% get_weights()

# A primeira camada contém os pesos para cada uma das features
pesos[[1]]  # Pesos entre as features de entrada e a primeira camada densa