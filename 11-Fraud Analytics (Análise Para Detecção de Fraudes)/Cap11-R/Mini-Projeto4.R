# Mini-Projeto 4 - Detecção de Fraudes em Transações de Vendas Online

# Leia os manuais em pdf no Capítulo 11 do curso de Business Analytics com a descrição do projeto,
# definição do problema e fonte de dados.

# Defina sua pasta de trabalho
getwd()
setwd("~/Users/pedropedrosa/Documents/Data Analytics/11-Fraud Analytics (Análise Para Detecção de Fraudes)/Cap11-R")

# Pacotes
library(ggplot2)
library(caret)
library(dplyr)
library(tibble)
library(tidyverse)
library(data.table)

# https://cran.r-project.org/web/packages/arsenal/vignettes/comparedf.html
library(arsenal)

# Carregando os Dados
treino_id = read.csv(file = "dados/train_identity.csv", na.strings = "")
treino_transacoes = read.csv(file = "dados/train_transaction.csv", na.strings = "")
teste_id = read.csv(file = "dados/test_identity.csv", na.strings = "")
teste_transacoes = read.csv("dados/test_transaction.csv", na.strings = "")

# Exploração Inicial dos Dados Históricos de Transações Online

# Dados de treino
dim(treino_id)
dim(treino_transacoes)

str(treino_id)
str(treino_transacoes)

View(treino_id)
View(treino_transacoes)

# Dados de teste
dim(teste_id)
dim(teste_transacoes)

str(teste_id)
str(teste_transacoes)

View(teste_id)
View(teste_transacoes)

# Engenharia de Atributos

# Nomes das colunas
names(treino_transacoes)
names(teste_transacoes)

# Ajustando a variável target em treino e teste
treino_transacoes[,"isFraud"] = factor(treino_transacoes[,"isFraud"])
teste_transacoes$isFraud <- NA

# Conferimos as dimensões
dim(treino_transacoes)
dim(teste_transacoes)

# Merge dos dataframnes para facilitar o trabalho de limpeza
?merge
dados_treino = merge(x = treino_id, y = treino_transacoes, by = "TransactionID")
dados_teste = merge(x = teste_id, y = teste_transacoes, by = "TransactionID")

# Dimensões
dim(dados_treino)
dim(dados_teste)

# Criamos uma coluna para identificar mais tarde se o registro é de treino ou teste
dados_treino$label = "treino"
dados_teste$label = "teste"

# Dimensões
dim(dados_treino)
dim(dados_teste)

# Vamos gerar um dataset completo com as duas amostras de dados (treino e teste)
dados_full <- rbind(dados_treino, dados_teste)

# TAREFA DE TROUBLESHOOTING

# Nomes das colunas
names(dados_treino)
names(dados_teste)

# Vamos organizar os nomes das colunas
dados_treino <- dados_treino %>%
  select(-label, -isFraud, everything())

dados_teste <- dados_teste %>%
  select(-label, -isFraud, everything())

# Nomes das colunas
names(dados_treino)
names(dados_teste)

# Dimensões
dim(dados_treino)
dim(dados_teste)

# Vamos gerar o dataset como as duas amostras
dados_full <- rbind(dados_treino, dados_teste)

# Vamos comparar os dataframes
?comparedf
comparedf(dados_treino, dados_teste)

# Nomes das colunas
names(dados_treino)
names(dados_teste)

# Lista de dataframes
dfs <- c("dados_treino", "dados_teste")

# Loop por todas as colunas dos dataframes para ajustar os nomes das colunas
for (eachdf in dfs) {
  df.tmp <- get(eachdf) 
  for (eachcol in 1:length(df.tmp)){
    colnames(df.tmp)[eachcol] <- str_trim(str_to_lower(str_replace_all(colnames(df.tmp)[eachcol], "_", ".")))
  }
  assign(eachdf, df.tmp) 
}

# Nomes das colunas
names(dados_treino)
names(dados_teste)

# Vamos gerar o dataset como as duas amostras
dados_full <- rbind(dados_treino, dados_teste)
dim(dados_full)
str(dados_full)

# Tratamento de Valores Ausentes

# Estratégia 1 - Remover variáveis cujo percentual de valor ausente for superior a 50%
# Estratégia 2 - Para as variáveis remanescentes, atribuir o valor "Desconhecido" se for variável categórica
# Estratégia 3 - Para as variáveis remanescentes, atribuir a média se for variável quantitativa

# Aplicando a Estratégia 1

# Calculando o percentual de valores ausentes por coluna
percentual_valores_ausentes = (colSums(is.na(dados_full)) / nrow(dados_full)) * 100

# Dataframe com o resultado anterior para criar o plot
df_percent_NA = data.frame(colnames(dados_full), percentual_valores_ausentes)
colnames(df_percent_NA) <- c("Variavel", "Percentual_Ausente")
df_percent_NA = df_percent_NA[order(df_percent_NA$Percentual_Ausente, decreasing = TRUE), ]

# Visualiza
View(df_percent_NA)

# Plot
plot(df_percent_NA$Percentual_Ausente, 
     ylab = "% de Valores Ausentes", 
     main = "Percentual de Valores Ausentes")

# Vamos remover as colunas com mais de 50% de valores ausentes
dim(dados_full)
dados_full <- dados_full[percentual_valores_ausentes < 50]
dim(dados_full)

# Aplicando as Estratégias 2 e 3

# Colunas ainda com valores ausentes
outrasNAcol <- (dados_full)[!colSums(is.na(dados_full))==0]
outrasNAcol <- colnames(outrasNAcol)

# Vamos colocar o valor "Desconhecido" onde estiver NA se for variável qualitativa
# Para variáveis quantitativas substituímos NA pela méda
for(f in outrasNAcol){
  
  if(any(is.na(dados_full[[f]]))){
    
    if(is.factor(dados_full[,f])){
      
      dados_full[,f] <- as.character(dados_full[,f])
      
      # Estratégia 2
      dados_full[,f][is.na(dados_full[,f])] <- "Desconhecido"
      dados_full[,f] <- factor(dados_full[,f])
      
    }
    else{
      
      # Estratégia 3
      dados_full[is.na(dados_full[,f]),f] <- mean(dados_full[,f], na.rm = TRUE)
    }
  }
}

# Verifica o dataframe
str(dados_full)
names(dados_full)
dim(dados_full)
sum(is.na(dados_full))

# Pré-Processamento das Variáveis Categóricas

# Convertemos as variáveis categóoricas para o tipo fator
str(dados_full)
dados_full[,"card1"] = factor(dados_full[,"card1"])
dados_full[,"card2"] = factor(dados_full[,"card2"])
dados_full[,"card3"] = factor(dados_full[,"card3"])
dados_full[,"card4"] = factor(dados_full[,"card4"])
dados_full[,"card5"] = factor(dados_full[,"card5"])
dados_full[,"card6"] = factor(dados_full[,"card6"])
dados_full[,"addr1"] = factor(dados_full[,"addr1"])
dados_full[,"addr2"] = factor(dados_full[,"addr2"])
dados_full[,"p.emaildomain"] = factor(dados_full[,"p.emaildomain"])
dados_full[,"r.emaildomain"] = factor(dados_full[,"r.emaildomain"])
dados_full[,"devicetype"] = factor(dados_full[,"devicetype"])
dados_full[,"deviceinfo"] = factor(dados_full[,"deviceinfo"])
dados_full[,"id.12"] = factor(dados_full[,"id.12"]) 
dados_full[,"id.13"] = factor(dados_full[,"id.13"]) 
dados_full[,"id.14"] = factor(dados_full[,"id.14"]) 
dados_full[,"id.15"] = factor(dados_full[,"id.15"])
dados_full[,"id.16"] = factor(dados_full[,"id.16"]) 
dados_full[,"id.17"] = factor(dados_full[,"id.17"])
dados_full[,"id.19"] = factor(dados_full[,"id.19"])
dados_full[,"id.20"] = factor(dados_full[,"id.20"]) 
dados_full[,"id.28"] = factor(dados_full[,"id.28"]) 
dados_full[,"id.29"] = factor(dados_full[,"id.29"]) 
dados_full[,"id.30"] = factor(dados_full[,"id.30"]) 
dados_full[,"id.31"] = factor(dados_full[,"id.31"]) 
dados_full[,"id.32"] = factor(dados_full[,"id.32"]) 
dados_full[,"id.33"] = factor(dados_full[,"id.33"])
dados_full[,"id.34"] = factor(dados_full[,"id.34"])
dados_full[,"id.35"] = factor(dados_full[,"id.35"]) 
dados_full[,"id.36"] = factor(dados_full[,"id.36"]) 
dados_full[,"id.37"] = factor(dados_full[,"id.37"]) 
dados_full[,"id.38"] = factor(dados_full[,"id.38"]) 

# Em variáveis do tipo texto vamos aplicar limpeza ao texto para poder separar as categorias

# Variável deviceinfo
View(table(dados_full$deviceinfo))
names_deviceinfo <- dados_full$deviceinfo
dados_full$deviceinfo <- factor(gsub("([A-Za-z]+).*",  "\\1", names_deviceinfo, ignore.case = FALSE))
View(table(dados_full$deviceinfo))

# Variável id.30
View(table(dados_full$id.30))
names_id.30 <- dados_full$id.30
dados_full$id.30 <- factor(gsub("([A-Za-z]+).*",  "\\1", names_id.30, ignore.case = FALSE))
View(table(dados_full$id.30))

# Variável card4
View(table(dados_full$card4))
dados_full$card4 = recode_factor(dados_full$card4, 
                                 'american express' = "OTHER",  
                                 'discover' = "OTHER", 
                                 'visa' = "visa",   
                                 'mastercard' = "mastercard",   
                                 .default = "OTHER")
View(table(dados_full$card4))

# Variável card6
View(table(dados_full$card6))
dados_full$card6 = recode_factor(dados_full$card6,
                                 'credit' = "credit", 
                                 'debit' = "debit",
                                 .default = "OTHER")
View(table(dados_full$card6))

# Ajusta a variável alvo removendo o nível onde a categoria for "Desconhecido" 
View(table(dados_full$isfraud))
dados_full$isfraud = factor(x = dados_full$isfraud, exclude = "Desconhecido")
View(table(dados_full$isfraud))

# Dimensões
dim(dados_full)
colnames(dados_full)

# Divisão dos Dados em Treino e Teste
dados_treino_final = subset(dados_full, label == "treino")
dados_teste_final = subset(dados_full, label == "teste")

# Dimensões
dim(dados_treino_final)
dim(dados_teste_final)

# Colunas
names(dados_treino_final)
names(dados_teste_final)

# Análise Exploratória
ggplot(dados_treino_final,
       aes(x = factor(devicetype), fill = isfraud)) +
  geom_bar() + 
  scale_fill_brewer(palette = "Dark2")

ggplot(dados_treino_final, 
       aes(x = factor(productcd), fill = isfraud)) +
  geom_bar(position = "fill") + 
  scale_fill_brewer(palette = "Set2")

ggplot(dados_treino_final,
       aes(x = factor(p.emaildomain), fill = isfraud)) +
  geom_bar() + 
  coord_flip() + 
  scale_fill_brewer(palette = "Set1")

# Checando outliers na quantidade das transações
ggplot(dados_treino_final,
       aes(factor(isfraud), transactionamt)) +
  geom_boxplot()

# Proporção por classe
table(dados_treino_final$isfraud)

# Preparação dos Dados Para Modelagem

# Divisão em dados de treino e teste
set.seed(100)
?createDataPartition
indice <- createDataPartition(dados_treino_final$isfraud, p = .7, list = F) 
df_treino <- dados_treino_final[indice, ]
df_valid <- dados_treino_final[-indice, ]

# Dimensões
dim(df_treino)
dim(df_valid)

# Remove a coluna de label e amostra os dados
set.seed(100)
names(df_treino)
df_treino_final = select(df_treino, -395)
dim(df_treino_final)
names(df_treino_final)
df_treino_final_sample <- sample(df_treino_final, replace = FALSE, prob = NULL)
dim(df_treino_final_sample)
names(df_treino_final_sample)

# Modelagem

# Modelo de Regressão Logística
?glm
modelo_v1 <- glm(formula = isfraud ~ productcd + card4 + card6 + devicetype + id.30,
                 data = df_treino_final_sample,
                 family = "binomial")

summary(modelo_v1)

# Avaliação do Modelo
previsoes <- predict(modelo_v1, newdata = df_valid, type = "response")
View(previsoes)

# Cutoff
y_pred_num <- ifelse(previsoes > 0.5, 1, 0)

# Previsões de classe
y_pred <- factor(y_pred_num, levels = c(0, 1))

# Valor real de y
y_act <- df_valid$isfraud
y_act <- factor(y_act, levels=c(0, 1))

# Matriz de Confusão e Acurácia
confusionMatrix(y_act, y_pred)

# Previsões com Novos Dados
previsoes_novos_dados = predict(modelo_v1, newdata = dados_teste_final, type = "response")
previsoes <- data.frame(TransactionID = dados_teste_final$transactionid , fraud = previsoes_novos_dados)
View(previsoes)
write.csv(previsoes , file = 'dados/previsoes.csv' , row.names = FALSE )

# Fim

