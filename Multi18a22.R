# Instalar e carregar o pacote nnet
# install.packages("nnet")
library(nnet)

esc_all_mul <- escolas_all[, -c(1,2,3,4,5,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26)]
esc_all_mul <- esc_all_mul %>% drop_na()
summary(esc_all_mul)

# Normalizando os dados
normalizar <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

esc_all_mul <- esc_all_mul %>%
  mutate(across(c( ALE, INDICE_EVOLUC, IND_INGR, IND_REGU, IND_ABAN, IND_INTE, MD_ATG), normalizar))

# dados de treino e teste
set.seed(123)  # Para reprodutibilidade
rm(train_index_all_m)
rm(train_data_all_m)
rm(test_data_all_m)
train_index_all_m <- sample(1:nrow(esc_all_mul), 0.8 * nrow(esc_all_mul))
train_data_all_m <- esc_all_mul[train_index_all_m, ]
test_data_all_m <- esc_all_mul[-train_index_all_m, ]

# Ajustar o modelo de regressão logística multinomial
modelo_multinom <- multinom(QT_M_ATING ~ ., data = train_data_all_m)

# Obter os coeficientes
coeficientes <- summary(modelo_multinom)$coefficients

# Obter os erros-padrão
erros_padrao <- summary(modelo_multinom)$standard.errors

# Prever as classes no conjunto de teste
pred_classes_multinom <- predict(modelo_multinom, newdata = test_data_all_m)

# Comparar as previsões com os valores reais
real_classes_multinom <- test_data_all_m$QT_M_ATING

# Gerar a matriz de confusão
matriz_confusao_multinom <- table(PREVISÃO = pred_classes_multinom, REAL = real_classes_multinom)
print(matriz_confusao_multinom)

# Utilizar caret para calcular métricas detalhadas
library(caret)
confusionMatrix(factor(pred_classes_multinom), factor(real_classes_multinom))

# Criar um dataframe com os resultados
df_results_multinom <- data.frame(
  Real = factor(real_classes_multinom),
  Predito = factor(pred_classes_multinom)
)

# Gráfico de barras
ggplot(df_results_multinom, aes(x = Real, fill = Predito)) +
  geom_bar(position = "dodge") +
  labs(title = "Comparação das Classes Reais e Preditas (Multinomial)", x = "Classe Real", y = "Contagem") +
  theme_minimal()

# Previsão das probabilidades
pred_probs_multinom <- predict(modelo_multinom, newdata = test_data_all_m, type = "probs")

# Calcular a curva ROC para cada classe (usando pROC)
multi_roc_multinom <- multiclass.roc(real_classes_multinom, pred_probs_multinom)
plot.roc(multi_roc_multinom)

# Calcular os valores z
z_values <- coeficientes / erros_padrao

# Calcular os p-valores (distribuição normal padrão)
p_values <- 2 * (1 - pnorm(abs(z_values)))

# Exibir os coeficientes e os p-valores correspondentes
resultados <- data.frame(Coefficientes = coeficientes, Z_Values = z_values, P_Values = p_values)
print(resultados)
