# Mini-Projeto 2 - Análise de Risco em Operações Financeiras

# Neste script usamos o modelo para fazer previsões e salvar em arquivo csv.

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

# Importamos o arquivo com dados de novos clientes
novos_dados <- read_csv("dados/novos_clientes.csv")

# Visão geral dos dados
dim(novos_dados)
summary(novos_dados)
str(novos_dados)
View(novos_dados)

# Preparação dos Dados

# Toda e qualquer limpeza, transformação ou processamento que foram aplicados aos dados de treino 
# devem ser aplicados aos novos dados.

novos_dados$IDADE <- ifelse(is.na(novos_dados$IDADE), 45, novos_dados$IDADE)

novos_dados$YOJ <- ifelse(is.na(novos_dados$YOJ), 11, novos_dados$YOJ)

novos_dados$SALARIO <- ifelse(is.na(novos_dados$SALARIO) & is.na(novos_dados$JOB), 54000, 
                              ifelse(is.na(novos_dados$SALARIO) & (novos_dados$JOB == "Medico"), 128000, 
                                     ifelse(is.na(novos_dados$SALARIO) & (novos_dados$JOB == "Advogado"), 88000, 
                                            ifelse(is.na(novos_dados$SALARIO) & (novos_dados$JOB == "Gerente"), 87000, 
                                                   ifelse(is.na(novos_dados$SALARIO) & (novos_dados$JOB == "Profissional"), 76000,
                                                          ifelse(is.na(novos_dados$SALARIO) & (novos_dados$JOB == "Trabalho Administrativo"), 33000,
                                                                 ifelse(is.na(novos_dados$SALARIO) & (novos_dados$JOB == "Trabalho Manual"), 58000,
                                                                        ifelse(is.na(novos_dados$SALARIO) & (novos_dados$JOB == "Do Lar"), 12000,
                                                                               ifelse(is.na(novos_dados$SALARIO) & (novos_dados$JOB == "Estudante"), 6300,
                                                                                      novos_dados$SALARIO)))))))))

novos_dados$HOME_VAL <- ifelse(is.na(novos_dados$HOME_VAL), 162000, novos_dados$HOME_VAL)

novos_dados$TEMPO_VIDA_PROD <- ifelse(is.na(novos_dados$TEMPO_VIDA_PROD), 8,
                                      ifelse(novos_dados$TEMPO_VIDA_PROD < 0, 0,
                                             novos_dados$TEMPO_VIDA_PROD))

novos_dados$JOB <- ifelse(is.na(novos_dados$JOB) & novos_dados$SALARIO > 150000, "Medico", 
                          ifelse(is.na(novos_dados$JOB) & novos_dados$SALARIO > 100000, "Advogado",
                                 ifelse(is.na(novos_dados$JOB) & novos_dados$SALARIO > 85000, "Gerente",
                                        ifelse(is.na(novos_dados$JOB) & novos_dados$SALARIO > 75000, "Profissional",
                                               ifelse(is.na(novos_dados$JOB) & novos_dados$SALARIO > 60000, "Trabalho Administrativo",
                                                      ifelse(is.na(novos_dados$JOB) & novos_dados$SALARIO > 35000, "Trabalho Manual",
                                                             ifelse(is.na(novos_dados$JOB) & novos_dados$SALARIO >= 12000, "Do Lar",
                                                                    ifelse(is.na(novos_dados$JOB) & novos_dados$SALARIO < 12000, "Estudante",
                                                                           novos_dados$JOB))))))))

View(novos_dados)

# Criamos então as variáveis que também foram criadas para treinar o modelo.

novos_dados$CONTA_CLI_Ind <- ifelse(novos_dados$CONTA_CLI == 'Yes', 1, 0)
novos_dados$CASADO_Ind <- ifelse(novos_dados$CASADO == 'Yes', 0, 1)
novos_dados$SEX_Ind <- ifelse(novos_dados$SEX == 'M', 0, 1)
novos_dados$Ed_Non_Degree_Ind <- ifelse(novos_dados$EDUCATION == 'Ensino Medio' | novos_dados$EDUCATION == 'Ensino Fundamental', 1, 0)
novos_dados$USO_PROD_Ind <- ifelse(novos_dados$USO_PROD == 'Comercial', 1, 0)
novos_dados$Renda_Variavel_Ind <- ifelse(novos_dados$TIPO_PROD == 'Renda Variavel', 1, 0)
novos_dados$Poupanca_Ind <- ifelse(novos_dados$TIPO_PROD == 'Poupanca', 1, 0)
novos_dados$Criptomeda_Ind <- ifelse(novos_dados$TIPO_PROD == 'Criptomeda', 1, 0)
novos_dados$Renda_Fixa_Ind <- ifelse(novos_dados$TIPO_PROD == 'Renda Fixa', 1, 0)
novos_dados$Tesouro_Direto_Ind <- ifelse(novos_dados$TIPO_PROD == 'Tesouro Direto', 1, 0)
novos_dados$TRAN_SUSP_Ind <- ifelse(novos_dados$TRAN_SUSP == 'Yes', 1, 0)
novos_dados$URB_Ind <- ifelse(novos_dados$URBANICITY == 'Urbano', 1, 0)
novos_dados$JOB_White_Collar_Ind <- ifelse(novos_dados$JOB == 'Trabalho Administrativo' | novos_dados$JOB == 'Medico' | novos_dados$JOB == 'Advogado' | novos_dados$JOB == 'Gerente' | novos_dados$JOB == 'Profissional', 1, 0)
novos_dados$JOB_Blue_Collar_Ind <- ifelse(novos_dados$JOB == 'Trabalho Manual', 1, 0)
novos_dados$JOB_Student_Ind <- ifelse(novos_dados$JOB == 'Estudante', 1, 0)
novos_dados$Home_Owner_else_Renter_Ind <- ifelse(novos_dados$HOME_VAL != 0 , 1, 0)

View(novos_dados)

# Como a previsão do modelo é uma equação matemática, qualquer coluna que tenha valores NA vai gerar como previsão valor NA.
# Se tivermos valores NA nos novos dados que não apareceram nos dados de treino, o ideal é treinar o modelo com essa regra.
# Não podemos definir uma nova regra de tratamento de valores ausentes em novos dados se essa regra não fez parte do treino.

# Deploy do Modelo

# Vamos montar a equação de previsão do modelo usando novos dados e os 
# coeficientes do modelo escolhido após na fase de modelagem.

resultado <- with(novos_dados, -2.523369633534 
                  + 0.171030108830 * CLM_FREQ  
                  + 2.182256461605 * URB_Ind  
                  - 0.000007053102 * SALARIO  
                  + 0.823279491688 * USO_PROD_Ind  
                  + 0.327474507031 * CONTA_CLI_Ind  
                  + 0.879376388237 * TRAN_SUSP_Ind  
                  + 0.126402607277 * TEMPO_CLIENTE  
                  - 1.197398965813 * Poupanca_Ind  
                  + 0.128643649068 * TEMPO_CLIENTE  
                  - 0.313317550816 * Home_Owner_else_Renter_Ind  
                  + 0.484220797137 * Ed_Non_Degree_Ind  
                  + 0.432252022246 * KIDSDRIV  
                  + 0.472944124897 * CASADO_Ind  
                  - 1.049482910648 * Tesouro_Direto_Ind  
                  - 0.000011406187 * OLDCLAIM  
                  - 0.633869337039 * Criptomeda_Ind  
                  - 0.650033714145 * Renda_Fixa_Ind
                  - 0.240057122738 * SEX_Ind
                  - 0.252483050147 * Renda_Variavel_Ind
                  - 0.008585587562 * IDADE)

# Calcuamos o exponencial do resultado (cálculo matemático da regressão logística)
ODDS <- exp(resultado)

# Fazemos as previsões
Previsoes_TARGET_FLAG <- ODDS / (1 + ODDS)

# Gravamos os resultados das previsões no dataframe
novos_dados$Previsoes_TARGET_FLAG <- Previsoes_TARGET_FLAG
View(novos_dados)

# Definimos o cutoff
cutoff <- 0.50

# Gravamos as previsões conforme regra do cutoff
novos_dados$TARGET_FLAG <- ifelse(Previsoes_TARGET_FLAG > cutoff, 1, 0)

# Visualiza os dados
View(novos_dados)

# Vamos salvar as previsões em um dataframe e depois exportar para CSV.
# Incluiremos algumas colunas (você pode incluir outras se desejar)
names(novos_dados)
resultado_final <- with(novos_dados, cbind.data.frame(INDEX, IDADE, EDUCATION, JOB, SALARIO, TARGET_FLAG))

# Ajustamos os nomes das colunas
colnames(resultado_final) <- c("Id_Cliente", "Idade", "Escolaridade", "Trabalho", "Salario", "Previsao_Risco")
View(resultado_final)

# Vamos deixar mais amigável para o tomador de decisão
resultado_final$Previsao_Risco <- ifelse(resultado_final$Previsao_Risco > 0, "Alto Risco", "Baixo Risco")
View(resultado_final)

# Gravamos o arquivo em disco com as previsões
write.csv(resultado_final, "dados/previsoes.csv")

# Agora leve o dataframe com as previsões para uma ferramenta de criação de gráficos como o Power BI, crie visualizações e
# apresente o resultado ao tomador de decisão.















