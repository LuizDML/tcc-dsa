getwd() 
setwd("D:\\R\\MBA\\TCC\\Educacao")
idesp07a17ai <- read.csv("NotasIDESP\\IDESP_Escolas_2007_2019_AI.csv", header=TRUE, sep = ";", stringsAsFactors=TRUE)
idesp07a17af <- read.csv("NotasIDESP\\IDESP_Escolas_2007_2019_AF.csv", header=TRUE, sep = ";", stringsAsFactors=TRUE)
idesp07a17em <- read.csv("NotasIDESP\\IDESP_Escolas_2007_2019_EM.csv", header=TRUE, sep = ";", stringsAsFactors=TRUE)

idesp18 <- read.csv("NotasIDESP\\IDESP_ESCOLA_2018.csv", header=TRUE, sep = ";", stringsAsFactors=TRUE)
idesp19 <- read.csv("NotasIDESP\\IDESP_ESCOLA_2019.csv", header=TRUE, sep = ",", stringsAsFactors=TRUE)
idesp21 <- read.csv("NotasIDESP\\IDESP_ESCOLA_2021.csv", header=TRUE, sep = ";", stringsAsFactors=TRUE)
idesp22 <- read.xlsx("NotasIDESP\\IDESP_ESCOLA_2022.xlsx", 1, header=TRUE)

str(idesp07a17ai)
str(idesp07a17af)
str(idesp07a17em)
str(idesp18)
str(idesp19)
str(idesp21)
str(idesp22)


idesp07a17ai <- idesp07a17ai %>%
  mutate(across(starts_with("X20"), ~ {
    valor_str <- as.character(.)
    valor <- as.numeric(gsub(",", "", valor_str))
    casas_decimais <- nchar(sub(".*,", "", valor_str))  # Conta o número de casas após a vírgula
    
    # Aplica a condição para dividir conforme o número de casas decimais
    ifelse(grepl(",", valor_str),  # Verifica se o valor original contém vírgula
           ifelse(casas_decimais == 1, valor / 10,
                  ifelse(casas_decimais == 2, valor / 100, valor)),
           valor)  # Mantém o valor original se não houver vírgula
  }))

idesp07a17af <- idesp07a17af %>%
  mutate(across(starts_with("X20"), ~ {
    valor_str <- as.character(.)
    valor <- as.numeric(gsub(",", "", valor_str))
    casas_decimais <- nchar(sub(".*,", "", valor_str))  # Conta o número de casas após a vírgula
    
    # Aplica a condição para dividir conforme o número de casas decimais
    ifelse(grepl(",", valor_str),  # Verifica se o valor original contém vírgula
           ifelse(casas_decimais == 1, valor / 10,
                  ifelse(casas_decimais == 2, valor / 100, valor)),
           valor)  # Mantém o valor original se não houver vírgula
  }))

idesp07a17em <- idesp07a17em %>%
  mutate(across(starts_with("X20"), ~ {
    valor_str <- as.character(.)
    valor <- as.numeric(gsub(",", "", valor_str))
    casas_decimais <- nchar(sub(".*,", "", valor_str))  # Conta o número de casas após a vírgula
    
    # Aplica a condição para dividir conforme o número de casas decimais
    ifelse(grepl(",", valor_str),  # Verifica se o valor original contém vírgula
           ifelse(casas_decimais == 1, valor / 10,
                  ifelse(casas_decimais == 2, valor / 100, valor)),
           valor)  # Mantém o valor original se não houver vírgula
  }))

idesp07a17ai <- idesp07a17ai %>%
  group_by(NIVEL.ENSINO) %>%
  summarize('2007' = mean(X2007, na.rm=TRUE), 
            '2008' = mean(X2008, na.rm=TRUE), 
            '2009' = mean(X2009, na.rm=TRUE), 
            '2010' = mean(X2010, na.rm=TRUE), 
            '2011' = mean(X2011, na.rm=TRUE), 
            '2012' = mean(X2012, na.rm=TRUE), 
            '2013' = mean(X2013, na.rm=TRUE), 
            '2014' = mean(X2014, na.rm=TRUE), 
            '2015' = mean(X2015, na.rm=TRUE), 
            '2016' = mean(X2016, na.rm=TRUE), 
            '2017' = mean(X2017, na.rm=TRUE) )

idesp07a17af <- idesp07a17af %>%
  group_by(NIVEL.ENSINO) %>%
  summarize('2007' = mean(X2007, na.rm=TRUE), 
            '2008' = mean(X2008, na.rm=TRUE), 
            '2009' = mean(X2009, na.rm=TRUE), 
            '2010' = mean(X2010, na.rm=TRUE), 
            '2011' = mean(X2011, na.rm=TRUE), 
            '2012' = mean(X2012, na.rm=TRUE), 
            '2013' = mean(X2013, na.rm=TRUE), 
            '2014' = mean(X2014, na.rm=TRUE), 
            '2015' = mean(X2015, na.rm=TRUE), 
            '2016' = mean(X2016, na.rm=TRUE), 
            '2017' = mean(X2017, na.rm=TRUE) )

idesp07a17em <- idesp07a17em %>%
  group_by(NIVEL.ENSINO) %>%
  summarize('2007' = mean(X2007, na.rm=TRUE), 
            '2008' = mean(X2008, na.rm=TRUE), 
            '2009' = mean(X2009, na.rm=TRUE), 
            '2010' = mean(X2010, na.rm=TRUE), 
            '2011' = mean(X2011, na.rm=TRUE), 
            '2012' = mean(X2012, na.rm=TRUE), 
            '2013' = mean(X2013, na.rm=TRUE), 
            '2014' = mean(X2014, na.rm=TRUE), 
            '2015' = mean(X2015, na.rm=TRUE), 
            '2016' = mean(X2016, na.rm=TRUE), 
            '2017' = mean(X2017, na.rm=TRUE) )

idesp21 <- idesp21 %>%
  mutate(ANOS_INICIAIS = as.numeric(gsub(",", "", as.character(idesp21$ANOS_INICIAIS)))/100,
         ANOS_FINAIS = as.numeric(gsub(",", "", as.character(idesp21$ANOS_FINAIS)))/100,
         ENSINO_MÉDIO = as.numeric(gsub(",", "", as.character(idesp21$ENSINO_MÉDIO)))/100)

idesp18g <- idesp18 %>%
  group_by(ANO_LETIVO) %>%
  summarize(ANOS_INICIAIS = mean(ANOS_INICIAIS, na.rm=TRUE), 
         ANOS_FINAIS = mean(ANOS_FINAIS, na.rm=TRUE),
         ENSINO_MEDIO = mean(ENSINO_MEDIO, na.rm=TRUE))

idesp19 <- idesp19 %>% 
  mutate(across(c(ANOS_INICIAIS, ANOS_FINAIS, ENSINO_MÉDIO), ~ na_if(., 0.00)))

idesp19g <- idesp19 %>%
  group_by(ANO_LETIVO) %>%
  summarize(ANOS_INICIAIS = mean(ANOS_INICIAIS, na.rm=TRUE), 
            ANOS_FINAIS = mean(ANOS_FINAIS, na.rm=TRUE),
            ENSINO_MEDIO = mean(ENSINO_MÉDIO, na.rm=TRUE))

idesp21g <- idesp21 %>%
  group_by(ANO_LETIVO) %>%
  summarize(ANOS_INICIAIS = mean(ANOS_INICIAIS, na.rm=TRUE), 
            ANOS_FINAIS = mean(ANOS_FINAIS, na.rm=TRUE),
            ENSINO_MEDIO = mean(ENSINO_MÉDIO, na.rm=TRUE))

idesp22g <- idesp22 %>%
  group_by(ANO) %>%
  summarize(ANOS_INICIAIS = mean(ANOS_INICIAIS, na.rm=TRUE), 
            ANOS_FINAIS = mean(ANOS_FINAIS, na.rm=TRUE),
            ENSINO_MEDIO = mean(ENSINO_MEDIO, na.rm=TRUE))

idesp_history <- rbind(idesp07a17ai, idesp07a17af, idesp07a17em)
idesp_history <- idesp_history[!(idesp_history$NIVEL.ENSINO==""), ]

idesp22g <- rename(idesp22g, ANO_LETIVO = ANO)
idesp_temp <- rbind(idesp18g, idesp19g, idesp21g, idesp22g)

idesp_temp <- as.data.frame(t(idesp_temp))
idesp_temp <- rename(idesp_temp, '2018' = V1, '2019' = V2, '2021' = V3, '2022' = V4)
idesp_temp <- idesp_temp[2:4,]

idesp_history <-cbind(idesp_history,idesp_temp)

idesp_history

write.csv(idesp_history, "Graficos/idesp_history.csv", row.names = FALSE)