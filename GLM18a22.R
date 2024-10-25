# Filtrando apenas as colunas relevantes
esc_all_glm_a <- escolas2018[, -c(1,2,3,4,5,7,8,9,11,12,13,15,16,18,19,20,21,22,23,24,25,26,27,28,31,32)] 
esc_all_glm_b <- escolas2019[, -c(1,2,3,4,5,7,8,9,11,12,13,15,16,18,19,20,21,22,23,24,25,26,27,28,31,32)] 
esc_all_glm_c <- escolas2021[, -c(1,2,3,4,5,7,9,10,12,13,15,16,17,18,19,20,21,22,23,24,25,28,29)] 
esc_all_glm_d <- escolas2022[, -c(1,2,3,4,5,7,8,9,11,12,13,15,16,18,19,20,21,22,23,24,25,26,27,28,31,32)]

esc_all_glm_a <- rename(esc_all_glm_a, MD_AT = MD_AT18)
esc_all_glm_b <- rename(esc_all_glm_b, MD_AT = MD_AT19)
esc_all_glm_c <- rename(esc_all_glm_c, MD_AT = MD_AT21)
esc_all_glm_d <- rename(esc_all_glm_d, MD_AT = MD_AT22)

esc_all_glm <- rbind(esc_all_glm_a, esc_all_glm_b, esc_all_glm_c, esc_all_glm_d)

# Verificar a distribuição da variável alvo
table(esc_all_glm$M_ATING)

# Separação entre treino e teste (80% treino, 20% teste)
set.seed(123)  # Para reprodutibilidade
rm(train_indices)
rm(train_data)
rm(test_data)
train_indices <- sample(1:nrow(esc_all_glm), 0.8 * nrow(esc_all_glm))
train_data <- esc_all_glm[train_indices, ]
test_data <- esc_all_glm[-train_indices, ]

# Ajustar o modelo de regressão logística nos dados de treino
modelo_18a22 <- glm(M_ATING ~ ., 
                   data = train_data, 
                   family = binomial)

# Resumo dos parâmetros do modelo
summary(modelo_18a22)

# Refinamento do modelo com stepwise
modelo_18a22s <- step(modelo_18a22)
summary(modelo_18a22s)

# Adicionando os valores previstos de probabilidade na base de teste
test_data$phat <- predict(modelo_18a22s, newdata = test_data, type = "response")

# Classificar como 0 ou 1 com um cutoff de 0.5
test_data$pred_class <- ifelse(test_data$phat > 0.5, 1, 0)

# Matriz de confusão para o conjunto de teste
library(caret)
conf_matrix <- confusionMatrix(as.factor(test_data$pred_class), as.factor(test_data$M_ATING))
print(conf_matrix)

# Extraindo TP, TN, FP, FN
TN <- conf_matrix$table[1, 1]  # Verdadeiro Negativo
FP <- conf_matrix$table[1, 2]  # Falso Positivo
FN <- conf_matrix$table[2, 1]  # Falso Negativo
TP <- conf_matrix$table[2, 2]  # Verdadeiro Positivo

# Calculando métricas
precisao <- TP / (TP + FP)
f1_score <- 2 * (precisao * sensitividade) / (precisao + sensitividade)

# Curva ROC e AUC
library(ROCR)
predictions <- prediction(test_data$phat, test_data$M_ATING)
perf <- performance(predictions, "tpr", "fpr")

# # Plotar a curva ROC
# plot(perf, colorize = TRUE)
# auc <- performance(predictions, measure = "auc")
# auc_value <- auc@y.values[[1]]
# print(paste("AUC:", auc_value))

# Carregar o pacote pROC
library(pROC)

# Gerar a curva ROC diretamente com pROC
ROC <- roc(response = test_data$M_ATING, 
           predictor = test_data$phat)

auc_value <- auc(ROC)

# Métricas de desempenho
data.frame(
  Acurácia = conf_matrix$overall['Accuracy'],
  Sensitividade = conf_matrix$byClass['Sensitivity'],
  Especificidade = conf_matrix$byClass['Specificity'],
  #Precisão = precisao,
  F1Score = f1_score,
  AUC = auc_value
) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center", full_width = F, font_size = 20)

# Plotagem da curva ROC
ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),  # Linha diagonal de referência
               color = "grey40", size = 0.2) +
  geom_line(aes(x = 1 - ROC$specificities, y = ROC$sensitivities),  # Plotar sensitividade vs 1 - especificidade
            color = "darkorchid", size = 2) +
  labs(x = "1 - Especificidade", 
       y = "Sensitividade",
       title = paste("Área abaixo da curva (AUC):", round(ROC$auc, 4))) +
  theme_bw()

