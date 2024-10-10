# Dataframe A
dfA <- data.frame(
  Nome = c("Luiz", "Bruna"),
  `2019` = c("TI", "RH"),
  stringsAsFactors = FALSE
)

# Dataframe B
dfB <- data.frame(
  Nome = c("Patricia", "Luiz", "Bruna"),
  `2020` = c("RH", "TI", "TI"),
  stringsAsFactors = FALSE
)

# Unindo os dataframes A e B pelo campo "Nome"
dfC <- merge(dfA, dfB, by = "Nome", all = TRUE)

# Exibindo o resultado
print(dfC)



# Criar uma lista de dataframes simulando anos diferentes
# Suponha que cada dataframe tenha colunas "Nome" e um ano específico

#######################

# Criar dataframes de exemplo
set.seed(123)  # Para garantir a reprodutibilidade

# Simulação de dataframes para diferentes anos
df_list <- lapply(1970:2020, function(year) {
  data.frame(
    Nome = c("Luiz", "Bruna", "Patricia"),
    Valor = rnorm(3),
    Ano = year,
    stringsAsFactors = FALSE
  )
})

# Função para merge recursivo
merge_all <- function(df_list, by = "Nome") {
  if (length(df_list) == 1) {
    return(df_list[[1]])
  } else {
    merged <- df_list[[1]]
    for (i in 2:length(df_list)) {
      merged <- merge(merged, df_list[[i]], by = by, all = TRUE)
    }
    return(merged)
  }
}

# Aplicar a função de merge para unir todos os dataframes da lista
df_merged <- merge_all(df_list, by = "Nome")

# Exibir o resultado
print(df_merged)

#############################

# Usar lapply para iterar sobre os nomes dos dataframes
lista_base_formac <- lapply(lista_base_formac, function(nome) {
  
  # Obter o dataframe pelo nome
  df <- get(nome)
  
  # Transformar as colunas específicas em fatores
  df$CARGO_C <- factor(df$CARGO)
  df$QUADRO_C <- factor(df$QUADRO)
  df$CATEG_E <- factor(df$CATEGORIA)
  df$FORMACAO <- factor(df$FORMACAO)
  
  # Retorna o dataframe modificado
  return(df)
})

# Verificar a estrutura do primeiro dataframe da lista após a transformação
print(str(lista_base_formac[[1]]))

##############################

# Exemplo de lista de dataframes
lista_dataframes <- list(
  df1 = data.frame(
    Nome = c("Luiz", "Bruna"),
    Idade = c(25, 30),
    Departamento = c("TI", "RH")
  ),
  df2 = data.frame(
    Nome = c("Patricia", "João"),
    Idade = c(28, 35),
    Departamento = c("Financeiro", "Vendas")
  )
)

# Lista de colunas que você deseja manter
colunas_a_manter <- c("Nome", "Idade")

# Usar lapply para remover as colunas específicas
lista_dataframes_sem_colunas <- lapply(lista_dataframes, function(df) {
  subset(df, select = colunas_a_manter)
})

# Verificar o resultado
print(lista_dataframes_sem_colunas)

##############################

# Exemplo de lista de dataframes
lista_dataframes <- list(
  df1 = data.frame(
    Nome = c("Luiz", "Bruna"),
    Idade = c(25, 30),
    Departamento = c("TI", "RH")
  ),
  df2 = data.frame(
    Nome = c("Patricia", "João"),
    Idade = c(28, 35),
    Departamento = c("Financeiro", "Vendas")
  )
)

# Lista de colunas que você deseja remover
colunas_a_remover <- c("Departamento")

# Usar lapply para remover as colunas específicas
lista_dataframes_sem_colunas <- lapply(lista_dataframes, function(df) {
  df[, !names(df) %in% colunas_a_remover]
})

# Verificar o resultado
print(lista_dataframes_sem_colunas)

################################

# Selecionar apenas as colunas CATEG_E e FORMACAO do segundo dataset
base_formac1219 <- base_formac1219 %>%
  select(ID_INTERNO, Cat1219 = CATEG_E, For1219 = FORMACAO)

base_formac1220 <- base_formac1220 %>%
  select(ID_INTERNO, Cat1220 = CATEG_E, For1220 = FORMACAO)

base_formac1221 <- base_formac1221 %>%
  select(ID_INTERNO, Cat1221 = CATEG_E, For1221 = FORMACAO)

base_formac1222 <- base_formac1222 %>%
  select(ID_INTERNO, Cat1222 = CATEG_E, For1222 = FORMACAO)

base_formac1223 <- base_formac1223 %>%
  select(ID_INTERNO, Cat1223 = CATEG_E, For1223 = FORMACAO)

#####################################

library(dplyr)

# Exemplo de dataframes
df1 <- data.frame(
  id_interno = 1:5,
  nome = c("A", "B", "C", "D", "E"),
  valor = c(10, 20, 30, 40, 50)
)

df2 <- data.frame(
  id_interno = 3:7,
  nome = c("F", "G", "H", "I", "J"),
  valor = c(60, 70, 80, 90, 100)
)

# Realizar o merge (join) com sufixos para colunas com nomes duplicados
merged_df <- df1 %>% 
  left_join(df2, by = "id_interno", suffix = c("_df1", "_df2"))

# Visualizar o dataframe resultante
head(merged_df)

########################################

library(dplyr)

# Exemplo de dataframes
df1 <- data.frame(
  id_interno = 1:5,
  nome = c("A", NA, "C", NA, "E"),
  valor = c(10, 20, 30, 40, 50)
)

df2 <- data.frame(
  id_interno = 3:7,
  nome = c("F", "G", "H", "I", "J"),
  valor = c(60, 70, 80, 90, 100)
)

# Realizar o merge (join) mantendo todas as colunas
merged_df <- df1 %>% 
  full_join(df2, by = "id_interno", suffix = c("_df1", "_df2"))

# Usar coalesce para combinar as colunas 'nome' e 'valor', mantendo dados de df1 quando disponíveis
final_df <- merged_df %>%
  mutate(
    nome = coalesce(nome_df1, nome_df2),
    valor = coalesce(valor_df1, valor_df2)
  ) %>%
  select(id_interno, nome, valor)  # Manter apenas as colunas necessárias

# Visualizar o dataframe resultante
head(final_df)


####################################################

library(dplyr)

# Exemplo de dataframes
base_formac1218a <- data.frame(id_interno = 1:5, nome = c("A", NA, "C", NA, "E"), valor = c(10, 20, 30, 40, 50))
base_formac1219a <- data.frame(id_interno = 3:7, nome = c("F", "G", "H", "I", "J"), valor = c(60, 70, 80, 90, 100))
base_formac1220a <- data.frame(id_interno = 1:5, nome = c("K", "L", "M", "N", "O"), valor = c(110, 120, 130, 140, 150))
base_formac1221a <- data.frame(id_interno = 1:5, nome = c("P", "Q", "R", "S", "T"), valor = c(210, 220, 230, 240, 250))
base_formac1222a <- data.frame(id_interno = 1:5, nome = c("U", "V", "W", "X", "Y"), valor = c(310, 320, 330, 340, 350))
base_formac1223a <- data.frame(id_interno = 1:5, nome = c("Z", "A1", "B1", "C1", "D1"), valor = c(410, 420, 430, 440, 450))

base_formac18a23 <- list(base_formac1218a, base_formac1219a, base_formac1220a, base_formac1221a, base_formac1222a, base_formac1223a)

# Função para merge recursivo
merge_all_bf <- function(df_list, by = "id_interno") {
  if (length(df_list) == 1) {
    return(df_list[[1]])
  } else {
    merged <- df_list[[1]]
    for (i in 2:length(df_list)) {
      sufixo_atual <- paste0("_df", i - 1) # Define sufixos dinamicamente
      merged <- merge(merged, df_list[[i]], by = by, all = TRUE, suffixes = c("", sufixo_atual))
      # Usar coalesce para combinar as colunas 'nome' e 'valor', mantendo dados de df1 quando disponíveis
      merged <- merged %>%
        mutate(
          nome = coalesce(merged[[paste0("nome", sufixo_atual)]], merged[["nome"]]),
          valor = coalesce(merged[[paste0("valor", sufixo_atual)]], merged[["valor"]])
        ) %>%
        select(-matches(paste0("nome", sufixo_atual)), -matches(paste0("valor", sufixo_atual))) # Remove colunas adicionais
    }
    return(merged)
  }
}

# Aplicar a função de merge para unir todos os dataframes da lista
base_formac <- merge_all_bf(base_formac18a23, by = "id_interno")

# Exibir o resultado
head(base_formac)