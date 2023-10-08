# Mini-Projeto 2 - Análise de Risco em Operações Financeiras

# Leia os manuais em pdf no Capítulo 10 do curso de Business Analytics com a definição do problema e outros detalhes.
# Você vai precisar do dicionário de dados, também disponível no no Capítulo 10, para compreender os nomes das variáveis.
# Execute o script linha a linha e leia os comentários.

# TARGET_FLAG é a nossa variável alvo.

# Modelagem

# Pacotes
library(ggplot2)
library(readr)
library(plyr)
library(car)
library(fBasics)
library(gmodels)
library(MASS)
library(gridExtra)
library(pROC)

# Carrega os dados
dados <- read_csv("dados/dados_historicos.csv")

# Visaliza no formato de tabela
View(dados)

##### Análise Exploratória e Limpeza dos Dados #####

# Dimensões do dataset
dim(dados)

# Tipos das variáveis
str(dados)

# Sumário estatístico
summary(dados)

# Vamos visualizar as estatísticas das variáveis numéricas 
# Analise a tabela em detalhes, especialmente a coluna que indica valores NA
?basicStats
View(t(basicStats(dados[sapply(dados, is.numeric)])))

# Calculando a frequência das variaveis categóricas
# Analise a tabela em detalhes
char_Freq <- lapply(dados[sapply(dados, is.character)], FUN = count)
char_stats <- ldply(char_Freq, data.frame)
names(char_stats) <- c("Variavel", "Valor", "Frequencia")
View(char_stats)

# Tratamento dos valores ausentes

# Com base na análise das estatísticas anteriores vamos definir as regras do tratamento de valores ausentes.
# Vamos limpar as variáveis usando regras diferentes para cada uma.

# Se tiver valor ausente na variável idade preenche com 45 (mediana da variável idade), senão, mantém a idade atual
dados$IDADE <- ifelse(is.na(dados$IDADE), 45, dados$IDADE)

# Se tiver valor ausente na variável YOJ preenche com 11 (mediana), senão, mantém o valor atual
dados$YOJ <- ifelse(is.na(dados$YOJ), 11, dados$YOJ)

# Vamos tratar os valores ausentes da variável salário de acordo com o tipo de trabalho, usando a estratégia de imputação
dados$SALARIO <- ifelse(is.na(dados$SALARIO) & is.na(dados$JOB), 54000, 
                        ifelse(is.na(dados$SALARIO) & (dados$JOB == "Medico"), 128000, 
                               ifelse(is.na(dados$SALARIO) & (dados$JOB == "Advogado"), 88000, 
                                      ifelse(is.na(dados$SALARIO) & (dados$JOB == "Gerente"), 87000, 
                                             ifelse(is.na(dados$SALARIO) & (dados$JOB == "Profissional"), 76000,
                                                    ifelse(is.na(dados$SALARIO) & (dados$JOB == "Trabalho Administrativo"), 58000,
                                                           ifelse(is.na(dados$SALARIO) & (dados$JOB == "Trabalho Manual"), 33000,
                                                                  ifelse(is.na(dados$SALARIO) & (dados$JOB == "Do Lar"), 12000,
                                                                         ifelse(is.na(dados$SALARIO) & (dados$JOB == "Estudante"), 6300, 
                                                                                dados$SALARIO)))))))))

# Imputação da mediana para a variável HOME_VAL
dados$HOME_VAL <- ifelse(is.na(dados$HOME_VAL), 162000, dados$HOME_VAL)

# Preenchemos com 8 (mediana da variável TEMPO_VIDA_PROD) se tver valor nulo. 
# Se for valor negativo preenchemos com zero (valor negativo não faz sentido nesta variável).
dados$TEMPO_VIDA_PROD <- ifelse(is.na(dados$TEMPO_VIDA_PROD), 8, ifelse(dados$TEMPO_VIDA_PROD < 0, 0, dados$TEMPO_VIDA_PROD))

# Para tratar os valores ausentes da variável JOB, checamos o salário.
# Por exemplo: Se a variável JOB tiver valor ausente e o salário for maior que 150.000 por ano, 
# deve ser Medico (pois esse é o salário dos demais médicos no dataset) e preenchemos com essa categoria o valor ausente.
dados$JOB <- ifelse(is.na(dados$JOB) & dados$SALARIO > 150000, "Medico", 
                     ifelse(is.na(dados$JOB) & dados$SALARIO > 100000, "Advogado",
                            ifelse(is.na(dados$JOB) & dados$SALARIO > 85000, "Gerente",
                                   ifelse(is.na(dados$JOB) & dados$SALARIO > 75000, "Profissional",
                                          ifelse(is.na(dados$JOB) & dados$SALARIO > 60000, "Trabalho Administrativo",
                                                 ifelse(is.na(dados$JOB) & dados$SALARIO > 35000, "Trabalho Manual",
                                                        ifelse(is.na(dados$JOB) & dados$SALARIO >= 12000, "Do Lar",
                                                               ifelse(is.na(dados$JOB) & dados$SALARIO < 12000, "Estudante",
                                                                      dados$JOB))))))))

# Visualiza o dataset
View(dados)

# Vamos checar as estatística após o tratamento dos valores ausentes.

# Visualizando as estatísticas das variáveis numéricas
View(t(basicStats(dados[sapply(dados,is.numeric)])))

# Calculando a frequência das variaveis categóricas
char_Freq <- lapply(dados[sapply(dados,is.character)], FUN = count)
char_stats <- ldply(char_Freq, data.frame)
names(char_stats) <- c("Variavel", "Valor", "Frequencia")
View(char_stats)

# Valores ausentes tratados com sucesso!

# Vamos seguir com a exploração dos dados e construir alguns gráficos.
# Para diferentes cores digite: colors() no console e escolha a sua preferida.

# Exploração de algumas variáveis numéricas (fique à vontade para explorar outras variáveis numéricas).
# Analise e interprete os gráficos conforme você aprendeu no curso até aqui. Use o dicionário de dados se necessário.

# Análise de Risco Por Atrasos de Pagamentos
ggplot(dados, mapping = aes(x = CLM_FREQ, y = (..density..)  , fill = TARGET_FLAG)) +
  geom_histogram(colour = "tomato3") + 
  facet_grid(~TARGET_FLAG, labeller = label_both ) + 
  theme_bw() +
  labs(title = "Análise de Risco Por Atrasos de Pagamentos", x = "CLM_FREQ", y = "Density") +
  scale_y_continuous(breaks = seq(0, 10, by = 0.5)) 

# Análise de Risco Por Filhos Morando ou Não na Mesma Casa
ggplot(dados, mapping = aes(x = KIDSDRIV, y = (..density..) , fill = TARGET_FLAG)) +
  geom_histogram(colour = "magenta") + 
  facet_grid(~TARGET_FLAG, labeller = label_both ) + 
  theme_bw() +
  labs(title = "Análise de Risco Por Filhos Morando ou Não na Mesma Casa", x = "KIDSDRIV", y = "Density") +
  scale_y_continuous(breaks = seq(0, 10, by = 0.5))

# Análise de Risco Por Tempo Como Cliente
ggplot(dados, mapping = aes(x = TEMPO_CLIENTE, y = (..density..) , fill = TARGET_FLAG)) +
  geom_histogram(colour = "magenta") + 
  facet_grid(~TARGET_FLAG, labeller = label_both ) + 
  theme_bw() +
  labs(title = "Análise de Risco Por Tempo Como Cliente", x = "TEMPO_CLIENTE", y = "Density") +
  scale_y_continuous(breaks = seq(0, 10, by = 0.5))

# Análise de Risco Por Salário
ggplot(dados, mapping = aes(x = SALARIO, y = (..density..) , fill = TARGET_FLAG)) +
  geom_histogram(colour = "magenta") + 
  facet_grid(~TARGET_FLAG, labeller = label_both ) + 
  theme_bw() +
  labs(title = "Análise de Risco Por Salário", x = "SALARIO", y = "Density") +
  scale_y_continuous(breaks = seq(0, 10, by = 0.5))

# Análise de Risco Por Valor da Casa Onde Mora
ggplot(dados, mapping = aes(x = HOME_VAL, y = (..density..) , fill = TARGET_FLAG)) +
  geom_histogram(colour = "magenta") + 
  facet_grid(~TARGET_FLAG, labeller = label_both ) + 
  theme_bw() +
  labs(title = "Análise de Risco Por Valor da Casa Onde Mora", x = "HOME_VAL", y = "Density") +
  scale_y_continuous(breaks = seq(0, 10, by = 0.5))

# Exploração de algumas variáveis categóricas (fique à vontade para explorar outras variáveis categóricas)

# Análise de Risco Por Nível Educacional
ggplot(dados, mapping = aes(x = TARGET_FLAG, y = (..density..),  group = EDUCATION, fill = EDUCATION)) +
  geom_histogram(colour = "yellowgreen") + 
  facet_grid(~EDUCATION ) + 
  theme_bw() +
  labs(title = "Análise de Risco Por Nível Educacional", x = "TARGET_FLAG", y = "Density") +
  scale_y_continuous(breaks = seq(0, 100, by = 5)) + 
  scale_x_continuous(breaks = seq(0, 1, by = 0.5))

# Análise de Risco Por Tipo de Profissão
ggplot(dados, mapping = aes(x = TARGET_FLAG, y = (..density..),  group = JOB, fill = JOB)) +
  geom_histogram(colour = "violetred4") + 
  facet_grid(~JOB ) +
  theme_bw() +
  labs(title = "Análise de Risco Por Tipo de Profissão", x = "TARGET_FLAG", y = "Density") +
  scale_y_continuous(breaks = seq(0, 30, by = 5))  + 
  scale_x_continuous(breaks = seq(0, 1, by = 0.5))

# Análise de Risco Por Número de Contas
ggplot(dados, mapping = aes(x = TARGET_FLAG, y = (..density..),  group = CONTA_CLI, fill = CONTA_CLI)) +
  geom_histogram(colour = "slateblue2") + 
  facet_grid(~CONTA_CLI, labeller = label_both ) +
  theme_bw() +
  labs(title = "Análise de Risco Por Número de Contas", x = "TARGET_FLAG", y = "Density") +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) 

# Análise de Risco Por Estado Civil
ggplot(dados, mapping = aes(x = TARGET_FLAG, y = (..density..),  group = CASADO, fill = CASADO)) +
  geom_histogram(colour = "slateblue3") + 
  facet_grid(~CASADO, labeller = label_both ) +
  theme_bw() +
  labs(title = "Análise de Risco Por Estado Civil", x = "TARGET_FLAG", y = "Density") +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) 

# Análise de Risco Por Gênero
ggplot(dados, mapping = aes(x = TARGET_FLAG, y = (..density..),  group = SEX, fill = SEX)) +
  geom_histogram(colour = "slateblue4") + 
  facet_grid(~SEX, labeller = label_both ) +
  theme_bw() +
  labs(title = "Análise de Risco Por Gênero", x = "TARGET_FLAG", y = "Density") +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) 

##### Engenharia de Atributos #####

# Vamos criar algumas tabelas cruzadas e compreender como os dados estão organizados. 
# Com base nessa análise definiremos as decisões na engenharia de atributos.
# O objetivo é compreender a proporção de registros de acordo com o risco envolvido na operação financeira.

# Analise cada uma das tabelas conforme você aprendeu no curso até aqui.

# Tabela 1
CrossTable('Total' = dados$JOB, dados$TARGET_FLAG)

# Tabela 2
CrossTable(dados$KIDSDRIV, 
           dados$TARGET_FLAG, 
           prop.r = TRUE, 
           prop.c = FALSE, 
           prop.t = FALSE, 
           prop.chisq = FALSE, 
           row.labels = TRUE, 
           dnn = c("KIDSDRIV", "TARGET_FLAG"))

# Tabela 3
CrossTable(dados$HOMEKIDS, 
           dados$TARGET_FLAG, 
           prop.r = TRUE, 
           prop.c = FALSE,
           prop.t = FALSE, 
           prop.chisq = FALSE, 
           row.labels = TRUE,
           dnn = c("HOMEKIDS", "TARGET_FLAG"))

# Tabela 4
CrossTable(dados$CONTA_CLI, 
           dados$TARGET_FLAG, 
           prop.r = TRUE, 
           prop.c = FALSE,
           prop.t = FALSE, 
           prop.chisq = FALSE, 
           row.labels = TRUE,
           dnn = c("CONTA_CLI", "TARGET_FLAG"))

# Tabela 5
CrossTable(dados$CASADO, 
           dados$TARGET_FLAG, 
           prop.r = TRUE, 
           prop.c = FALSE,
           prop.t = FALSE, 
           prop.chisq = FALSE, 
           row.labels = TRUE,
           dnn = c("CASADO", "TARGET_FLAG"))

# Tabela 6
CrossTable(dados$SEX, 
           dados$TARGET_FLAG, 
           prop.r = TRUE, 
           prop.c = FALSE,
           prop.t = FALSE, 
           prop.chisq = FALSE, 
           row.labels = TRUE,
           dnn = c("SEX", "TARGET_FLAG"))

# Tabela 7
CrossTable(dados$EDUCATION, 
           dados$TARGET_FLAG, 
           prop.r = TRUE, 
           prop.c = FALSE,
           prop.t = FALSE, 
           prop.chisq = FALSE, 
           row.labels = TRUE,
           dnn = c("EDUCATION", "TARGET_FLAG"))

# Tabela 8
CrossTable(dados$JOB, 
           dados$TARGET_FLAG, 
           prop.r = TRUE, 
           prop.c = FALSE,
           prop.t = FALSE, 
           prop.chisq = FALSE, 
           row.labels = TRUE,
           dnn = c("JOB", "TARGET_FLAG"))

# Tabela 9
CrossTable(dados$USO_PROD, 
           dados$TARGET_FLAG, 
           prop.r = TRUE, 
           prop.c = FALSE,
           prop.t = FALSE, 
           prop.chisq = FALSE, 
           row.labels = TRUE,
           dnn = c("USO_PROD", "TARGET_FLAG"))

# Tabela 10
CrossTable(dados$TIPO_PROD, 
           dados$TARGET_FLAG, 
           prop.r = TRUE, 
           prop.c = FALSE,
           prop.t = FALSE, 
           prop.chisq = FALSE, 
           row.labels = TRUE,
           dnn = c("TIPO_PROD", "TARGET_FLAG"))

# Tabela 11
CrossTable(dados$PROD_ALTO_VALOR, 
           dados$TARGET_FLAG, 
           prop.r = TRUE, 
           prop.c = FALSE,
           prop.t = FALSE, 
           prop.chisq = FALSE, 
           row.labels = TRUE,
           dnn = c("PROD_ALTO_VALOR", "TARGET_FLAG"))

# Tabela 12
CrossTable(dados$TRAN_SUSP, 
           dados$TARGET_FLAG, 
           prop.r = TRUE, 
           prop.c = FALSE,
           prop.t = FALSE, 
           prop.chisq = FALSE, 
           row.labels = TRUE,
           dnn = c("TRAN_SUSP", "TARGET_FLAG"))

# Tabela 13
CrossTable(dados$URBANICITY, 
           dados$TARGET_FLAG, 
           prop.r = TRUE, 
           prop.c = FALSE,
           prop.t = FALSE, 
           prop.chisq = FALSE, 
           row.labels = TRUE,
           dnn = c("URBANICITY", "TARGET_FLAG"))

# Tabela 14
CrossTable(dados$IDADE, 
           dados$TARGET_FLAG, 
           prop.r = TRUE, 
           prop.c = FALSE,
           prop.t = FALSE, 
           prop.chisq = FALSE, 
           row.labels = TRUE,
           dnn = c("IDADE", "TARGET_FLAG"))

# Tabela 15
CrossTable(dados$YOJ, 
           dados$TARGET_FLAG, 
           prop.r = TRUE, 
           prop.c = FALSE,
           prop.t = FALSE, 
           prop.chisq = FALSE, 
           row.labels = TRUE,
           dnn = c("YOJ", "TARGET_FLAG"))

# Tabela 16
CrossTable(dados$TEMPO_VIDA_PROD, 
           dados$TARGET_FLAG, 
           prop.r = TRUE, 
           prop.c = FALSE,
           prop.t = FALSE, 
           prop.chisq = FALSE, 
           row.labels = TRUE,
           dnn = c("TEMPO_VIDA_PROD", "TARGET_FLAG"))

# Agora preparamos as variáveis categóricas, criando variáveis dummy 
# (classes das variáveis categóricas com uma representação numérica)
# Nosso objetivo é usar variáveis binárias para representar a maioria das informações.
dados$CONTA_CLI_Ind <- ifelse(dados$CONTA_CLI == 'Yes', 1, 0)
dados$CASADO_Ind <- ifelse(dados$CASADO == 'Yes', 0, 1)
dados$SEX_Ind <- ifelse(dados$SEX == 'M', 0, 1)
dados$Ed_Non_Degree_Ind <- ifelse(dados$EDUCATION == 'Ensino Medio' | dados$EDUCATION == 'Ensino Fundamental', 1, 0)
dados$USO_PROD_Ind <- ifelse(dados$USO_PROD == 'Comercial', 1, 0)
dados$Renda_Variavel_Ind <- ifelse(dados$TIPO_PROD == 'Renda Variavel', 1, 0)
dados$Poupanca_Ind <- ifelse(dados$TIPO_PROD == 'Poupanca', 1, 0)
dados$Criptomeda_Ind <- ifelse(dados$TIPO_PROD == 'Criptomeda', 1, 0)
dados$Renda_Fixa_Ind <- ifelse(dados$TIPO_PROD == 'Renda Fixa', 1, 0)
dados$Tesouro_Direto_Ind <- ifelse(dados$TIPO_PROD == 'Tesouro Direto', 1, 0)
dados$TRAN_SUSP_Ind <- ifelse(dados$TRAN_SUSP == 'Yes', 1, 0)
dados$URB_Ind <- ifelse(dados$URBANICITY == 'Urbano', 1, 0)
dados$JOB_White_Collar_Ind <- ifelse(dados$JOB == 'Trabalho Administrativo' | dados$JOB == 'Medico' | dados$JOB == 'Advogado' | dados$JOB == 'Gerente' | dados$JOB == 'Profissional', 1, 0)
dados$JOB_Blue_Collar_Ind <- ifelse(dados$JOB == 'Trabalho Manual', 1, 0)
dados$JOB_Student_Ind <- ifelse(dados$JOB == 'Estudante', 1, 0)
dados$Home_Owner_else_Renter_Ind <- ifelse(dados$HOME_VAL != 0 , 1, 0)

# Visualiza o dataset
View(dados)

# Visualiza as estatísticas
View(t(basicStats(dados[sapply(dados, is.numeric)])))

# Algumas variáveis podem ser removidas. Vamos preparar a lista com as variáveis.
names(dados)

# Cria a lista com os nomes das variáveis que podem ser removidas
lista_remover <- c('INDEX', 'TARGET_AMT', 'CONTA_CLI', 'CASADO', 'SEX', 'EDUCATION', 'USO_PROD', 'TIPO_PROD', 'TRAN_SUSP', 
                   'URBANICITY', 'JOB', 'HOME_VAL', 'VALOR_SOLICITADO', 'PROD_ALTO_VALOR', 'TEMPO_VIDA_PROD', 'TRAVTIME', 'TIF')

# Remove as variáveis do dataset
dados <- dados[,!(names(dados) %in% lista_remover )]

# Confere o resultado
names(dados)
View(dados)

# Vamos criar uma variável aleatória seguindo uma distribuição uniforme que será usada como índice para divisão treino/teste
dados$indice <- runif(n = dim(dados)[1], min = 0, max = 1)

# Se o valor do índice for menor que 0.70 colocaremos no dataset de treino, senão, dataset de teste
dados$treino <- ifelse(dados$indice < 0.70, 1, 0)

# Verificamos o resultado e a proporção 70/30 (treino/teste)
# 0 é para registro de teste
# 1 é para registro de treino
table(dados$treino)
table(dados$treino)/dim(dados)[1]

# Confere o resultado
names(dados)
View(dados)

# Salvamos o dataframe em disco
saveRDS(dados, file= "dados/dados.RData")

##### Modelagem #####

# Carrega os dados do disco
df <- readRDS(file= "dados/dados.RData")
names(df)
View(df)

# Criamos as amostras de treino e teste
df_treino <- subset(df, treino == 1)
dim(df_treino)
df_teste <- subset(df, treino == 0)
dim(df_teste)

# Modelo Versão 1 com Regressão Logística
# Observe atentamente que estamos criando 3 modelos que depois serão usados no stepAIC.
?glm

# Modelo full com todas as variáveis (exceto as variáveis de controle treino e indice)
modelo_full <- glm(TARGET_FLAG ~ . -treino -indice, family = binomial(link = "logit"), data = df_treino)
summary(modelo_full)
sort(vif(modelo_full), decreasing = TRUE)

# Modelo apenas com o intercepto
modelo_intercepto <- glm(TARGET_FLAG ~ 1, family = binomial(link = "logit"), data = df_treino)
summary(modelo_intercepto)

# Modelo com uma das variáveis
modelo_start <- glm(TARGET_FLAG ~ CLM_FREQ, family = binomial(link = "logit"), data = df_treino)
summary(modelo_start)

# Agora criamos o modelo versão 1 usando stepAIC
# O stepAIC vai eliminando as variáveis e criando diversos modelos. Ao final entrega o modelo com melhor performance.
?stepAIC

# Execute a linha de código abaixo e observe o que ocorre no console
modelo_v1 <- stepAIC(object = modelo_start, 
                     scope = list(upper = formula(modelo_full), lower = ~1), 
                     direction = c('both'))

summary(modelo_v1)
sort(vif(modelo_v1), decreasing = TRUE)

# Avaliação do Modelo Versão 1

# Definimos o cutoff para a definição de classe.
# Isso é necessário pois o resultado da previsão do modelo é em probabilidade.
# Definiremos o cutoff em 50% de probabilidade e usaremos para classificar em classe 1 ou 0.
cutoff <- 0.50

# Previsões
pred_v1_treino <- predict(modelo_v1, type = "response")
pred_v1_teste <- predict(modelo_v1, newdata = df_teste, type = "response")

# Aplica o cutoff
class_pred_v1_treino <- ifelse(pred_v1_treino > cutoff, 1, 0)
class_pred_v1_teste <- ifelse(pred_v1_teste > cutoff, 1, 0)

# Matriz de confusão
cm_v1_treino <- table(df_treino$TARGET_FLAG, class_pred_v1_treino)
cm_v1_teste <- table(df_teste$TARGET_FLAG, class_pred_v1_teste)

# Acurácia
acc_v1_treino <- sum(diag(cm_v1_treino)) / nrow(df_treino)
print(acc_v1_treino)
acc_v1_teste <- sum(diag(cm_v1_teste)) / nrow(df_teste)
print(acc_v1_teste)

# ROC
roc_v1_treino <- roc(df_treino$TARGET_FLAG, pred_v1_treino)
roc_v1_teste <- roc(df_teste$TARGET_FLAG, pred_v1_teste )

# Plot da Curva ROC
par(mfrow = c(1,2))
plot.roc(roc_v1_treino, col = "green", main = "Curva ROC com Dados de Treino")
plot.roc(roc_v1_teste, col = "green", main = "Curva ROC com Dados de Teste")

# Score AUC
auc(roc_v1_treino)
auc(roc_v1_teste)

# AIC
AIC(modelo_v1)

# Ao final vamos comparar e interpretar as versões dos modelos.

# Modelo Versão 2 com Regressão Logística
# Para essa versão usaremos as variáveis que se mostraram mais relevantes na versão 1 do modelo.
names(df)
modelo_v2 <- glm(TARGET_FLAG ~ CLM_FREQ + URB_Ind + SALARIO + USO_PROD_Ind + TRAN_SUSP_Ind + CONTA_CLI_Ind + 
                   HOMEKIDS + Ed_Non_Degree_Ind + CASADO_Ind + KIDSDRIV + Renda_Variavel_Ind + Poupanca_Ind + 
                   Criptomeda_Ind + Renda_Fixa_Ind + OLDCLAIM + SEX_Ind + Home_Owner_else_Renter_Ind,
                family = binomial(link = "logit"),
                data = df_treino)

summary(modelo_v2)
sort(vif(modelo_v2), decreasing = TRUE)

# Avaliação do Modelo Versão 2

# Definimos o cutoff
cutoff <- 0.50

# Previsões
pred_v2_treino <- predict(modelo_v2, type = "response")
pred_v2_teste <- predict(modelo_v2, newdata = df_teste, type = "response")

# Aplica o cutoff
class_pred_v2_treino <- ifelse(pred_v2_treino > cutoff, 1, 0)
class_pred_v2_teste <- ifelse(pred_v2_teste > cutoff, 1, 0)

# Matriz de confusão
cm_v2_treino <- table(df_treino$TARGET_FLAG, class_pred_v2_treino)
cm_v2_teste <- table(df_teste$TARGET_FLAG, class_pred_v2_teste)

# Acurácia
acc_v2_treino <- sum(diag(cm_v2_treino)) / nrow(df_treino)
print(acc_v2_treino)
acc_v2_teste <- sum(diag(cm_v2_teste)) / nrow(df_teste)
print(acc_v2_teste)

# ROC
roc_v2_treino <- roc(df_treino$TARGET_FLAG, pred_v2_treino)
roc_v2_teste <- roc(df_teste$TARGET_FLAG, pred_v2_teste )

# Plot da Curva ROC
par(mfrow = c(1,2))
plot.roc(roc_v2_treino, col = "magenta", main = "Curva ROC com Dados de Treino")
plot.roc(roc_v2_teste, col = "magenta", main = "Curva ROC com Dados de Teste")

# Score AUC
auc(roc_v2_treino)
auc(roc_v2_teste)

# AIC
AIC(modelo_v2)


# Versão 3 do Modelo com Regressão Logística
# Usaremos as variáveis mais relevantes da versão 2.
modelo_v3 <- glm(TARGET_FLAG ~ CLM_FREQ + URB_Ind + SALARIO + USO_PROD_Ind + TRAN_SUSP_Ind + 
                   Ed_Non_Degree_Ind + CASADO_Ind + KIDSDRIV + Poupanca_Ind,
                 family = binomial(link = "logit"),
                 data = df_treino )

summary(modelo_v3)
sort(vif(modelo_v3), decreasing = TRUE)


# Avaliação do Modelo Versão 3

# Definimos o cutoff
cutoff <- 0.50

# Previsões
pred_v3_treino <- predict(modelo_v3, type = "response")
pred_v3_teste <- predict(modelo_v3, newdata = df_teste, type = "response")

# Aplica o cutoff
class_pred_v3_treino <- ifelse(pred_v3_treino > cutoff, 1, 0)
class_pred_v3_teste <- ifelse(pred_v3_teste > cutoff, 1, 0)

# Matriz de confusão
cm_v3_treino <- table(df_treino$TARGET_FLAG, class_pred_v3_treino)
cm_v3_teste <- table(df_teste$TARGET_FLAG, class_pred_v3_teste)

# Acurácia
acc_v3_treino <- sum(diag(cm_v3_treino)) / nrow(df_treino)
print(acc_v3_treino)
acc_v3_teste <- sum(diag(cm_v3_teste)) / nrow(df_teste)
print(acc_v3_teste)

# ROC
roc_v3_treino <- roc(df_treino$TARGET_FLAG, pred_v3_treino)
roc_v3_teste <- roc(df_teste$TARGET_FLAG, pred_v3_teste )

# Plot da Curva ROC
par(mfrow = c(1,2))
plot.roc(roc_v3_treino, col = "blue", main = "Curva ROC com Dados de Treino")
plot.roc(roc_v3_teste, col = "blue", main = "Curva ROC com Dados de Teste")

# Score AUC
auc(roc_v3_treino)
auc(roc_v3_teste)

# AIC
AIC(modelo_v3)


# Seleção do modelo
# Vamos comparar as métricas dos modelos e escolher qual será levado para produção.

# Acurácia em teste (quanto MAIOR, melhor)
acc_v1_teste
acc_v2_teste
acc_v3_teste

# AUC em teste (quanto MAIOR, melhor)
auc(roc_v1_teste)
auc(roc_v2_teste)
auc(roc_v3_teste)

# AIC dos modelos (quanto MENOR, melhor)
AIC(modelo_v1)
AIC(modelo_v2)
AIC(modelo_v3)

# Vamos trabalhar com o Modelo Versão 1, pois foi o que apresentou melhor performance global.

# Um modelo de Machine Learning nada mais é do que o conjunto de coeficientes que 
# foram aprendidos durante o treinamento. Vamos visualizar os coeficientes da versão 1:

options(scipen = 999)
df_coef <- as.data.frame(modelo_v1$coefficients)
print(df_coef)

# Este foi o nosso resultado:

# (Intercept)                       -2.523369633534
# CLM_FREQ                           0.171030108830
# URB_Ind                            2.182256461605
# SALARIO                           -0.000007053102
# USO_PROD_Ind                       0.823279491688
# CONTA_CLI_Ind                      0.327474507031
# TRAN_SUSP_Ind                      0.879376388237
# Poupanca_Ind                      -1.197398965813
# TEMPO_CLIENTE                      0.128643649068
# Home_Owner_else_Renter_Ind        -0.313317550816
# Ed_Non_Degree_Ind                  0.484220797137
# KIDSDRIV                           0.432252022246
# CASADO_Ind                         0.472944124897
# Tesouro_Direto_Ind                -1.049482910648
# OLDCLAIM                          -0.000011406187
# Criptomeda_Ind                    -0.633869337039
# Renda_Fixa_Ind                    -0.650033714145
# SEX_Ind                           -0.240057122738
# Renda_Variavel_Ind                -0.252483050147
# IDADE                             -0.008585587562

# Levamos os valores dos coeficientes para o script de previsões.
# Perceba que alguns coeficentes são valores negativos e outros positivos.
# Como o processo de criação do modelo segue passos aleatórios os resultados podem ser diferentes a cada execução.



