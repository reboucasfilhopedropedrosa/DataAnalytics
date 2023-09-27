# Distribuições de Probabilidade Para Variáveis Discretas

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("~/Users/pedropedrosa/Documents/Data Analytics/02-Introduction to Analytics - Part 1/Cap02-Code/R")
getwd()


# Distribuição de Probabilidade Para Variável Discreta - Qual a Probabilidade de Responder Corretamente um Exame Final?


######----------------- Distribuição Binomial -----------------------######

# Na teoria da probabilidade e na estatística, a distribuição binomial com os parâmetros n e p é a distribuição discreta de 
# probabilidade do número de sucessos em uma sequência de n experimentos independentes, cada um fazendo uma pergunta sim-não 
# e cada um com seu próprio resultado com valor booleano: sucesso / sim / verdadeiro / um (com probabilidade p) ou 
# falha / não / falso / zero (com probabilidade q = 1 - p). 

# Um único experimento de sucesso / fracasso também é chamado de Trial de Bernoulli ou Experimento de Bernoulli e 
# uma sequência de resultados é chamada de Processo de Bernoulli; para uma única tentativa, ou seja, n = 1, a distribuição 
# binomial é uma Distribuição de Bernoulli.

# Portanto, a distribuição binomial é uma distribuição de probabilidade discreta, que descreve o resultado de n experimentos 
# independentes. Cada ensaio é suposto ter apenas dois resultados, seja sucesso ou fracasso.

# Em R, temos duas funções principais que nos ajudam a calcular as probabilidades:

# dbinom é uma função de massa de probabilidade da distribuição binomial, enquanto 
# pbinom é uma função de distribuição cumulativa dessa distribuição. 

# O primeiro diz a você o que é P (𝑋 = 𝑥) (probabilidade de observar valor igual a 𝑥)
# enquanto o segundo, o que é P (𝑋≤𝑥) (probabilidade de observar valor menor ou igual a 𝑥)

# Exemplo:

# Suponha que haja 12 perguntas de múltipla escolha em um exame (para ser aprovado precisa acerta 6 ou mais questões).
# Cada pergunta tem 5 respostas possíveis, e apenas 1 delas está correta. 

# Encontre a probabilidade de ter 6 respostas corretas se um aluno tentar responder a todas as perguntas aleatoriamente.

# Uma vez que apenas uma entre cinco respostas possíveis está correta, a probabilidade de responder a 
# uma pergunta corretamente por aleatoriedade é 1/5 = 0,2. Podemos encontrar a probabilidade de ter 
# exatamente 6 respostas corretas por tentativas aleatórias como segue.

# A função dbinom fornece a distribuição de densidade de probabilidade em cada ponto.
# A distribuição binomial requer dois parâmetros extras, o número de tentativas e a probabilidade de sucesso 
# de uma única tentativa.
help(Binomial)
?dbinom
dbinom(6, size = 12, prob = 0.2) 
dbinom(6, size = 12, prob = 0.2) * 100

# Vamos plotar todas as possibilidades
x <- dbinom(0:12, size = 12, p = 0.2)
barplot(x, names.arg = 0:12, space = 0)

# Qual a probabilidade de ter 5 ou menos perguntas respondidas corretamente de forma aleatória, no questionário 
# de 12 questões de múltipla escolha?

# Poderíamos resolver assim:
dbinom(0, size=12, prob=0.2) + 
dbinom(1, size=12, prob=0.2) + 
dbinom(2, size=12, prob=0.2) + 
dbinom(3, size=12, prob=0.2) + 
dbinom(4, size=12, prob=0.2) +
dbinom(5, size=12, prob=0.2) 

# Ou assim:
# A função pbinom fornece a probabilidade cumulativa de um evento. É um valor único que representa a probabilidade.
?pbinom
pbinom(5, size = 12, prob = 0.2) 

# Vamos plotar todas as possibilidades
x <- pbinom(0:12, size = 12, p = 0.2)
barplot(x, names.arg = 0:12, space = 0)


# Distribuição de Probabilidade Para Variável Discreta - Qual a Probabilidade de Ter Um Número Específico de Vendas Por Semana?


######----------------- Distribuição Poisson -----------------------######

# Na teoria das probabilidades e estatística, a Distribuição de Poisson, nomeada em homenagem ao matemático francês 
# Siméon Denis Poisson, é uma distribuição de probabilidade discreta que expressa a probabilidade de um determinado número 
# de eventos ocorrendo em um intervalo fixo de tempo ou espaço, se esses eventos ocorrerem com uma taxa média constante 
# conhecida e independentemente do tempo desde o último evento. 

# A Distribuição Poisson também pode ser usada para o número de eventos em outros intervalos especificados, como distância, 
# área ou volume.

# A Distribuição de Poisson é a distribuição de probabilidade de ocorrências de eventos independentes em um 
# intervalo fixo de tempo ou espaço.

# A função R dpois (x, lambda) é a probabilidade de x sucessos em um período em que o número esperado de eventos é lambda. 
# A função R ppois (q, lambda, lower.tail) é a probabilidade cumulativa, sendo 
# (lower.tail = TRUE para a cauda esquerda, lower.tail = FALSE para a cauda direita) menor ou igual a q sucessos.

# Qual é a probabilidade de realizar de 2 a 4 vendas em uma semana se a taxa média de vendas for de 3 por semana?

# Usando a probabilidade exata
?dpois
dpois(x = 2, lambda = 3) + dpois(x = 3, lambda = 3) + dpois(x = 4, lambda = 3)

# Usando probabilidade acumulada (por que a quarta opção é a correta?)
ppois(q = 4, lambda = 3) 
ppois(q = 4, lambda = 3, lower.tail = TRUE)
ppois(q = 4, lambda = 3, lower.tail = FALSE)
ppois(q = 4, lambda = 3, lower.tail = TRUE) - ppois(q = 1, lambda = 3, lower.tail = TRUE)

# Vamos plotar todas as possibilidades
x <- ppois(q = 0:10, lambda = 3,  lower.tail = TRUE)
barplot(x, names.arg = 0:10, space = 0)

# Qual a probabilidade de qualquer número de vendas em uma semana se a taxa média de vendas for de 3 por semana?
# Número esperado de vendas = lambda = 3

# Pacotes
library(ggplot2)

# Formata valores decimais
options(scipen = 999, digits = 2) 

# Eventos possíveis (número de vendas)
eventos <- 0:10

# Calcula as probabilidades para todos os eventos, ou seja, a distribuição de probabilidades 
# para a variável aleatória.
probs <- dpois(x = eventos, lambda = 3)

# Calcula as probabilidades acumuladas para todos os eventos.
prob_acumulada <- ppois(q = eventos, lambda = 3, lower.tail = TRUE)

# Consolidade tudo em um dataframe
df <- data.frame(eventos, probs, prob_acumulada)
df

# Plot (cuidado com a escala do plot)

# Sem probabilidade acumulada
ggplot(df, aes(x = factor(eventos), y = probs)) +
  geom_col() +
  geom_text(aes(label = round(probs,2), y = probs + 0.01), position = position_dodge(0.9), size = 3, vjust = 0) +
  labs(title = "Distribuição Poisson Para Calcular a Probabilidade de Vendas Por Semana", 
       x = "Evento (Número de Vendas)", 
       y = "Probabilidade") 

# Com probabilidade acumulada
ggplot(df, aes(x = factor(eventos), y = probs)) +
  geom_col() +
  geom_text(aes(label = round(probs,2), y = probs + 0.01), position = position_dodge(0.9), size = 3, vjust = 0) +
  labs(title = "Distribuição Poisson Para Calcular a Probabilidade de Vendas Por Semana", 
       x = "Evento (Número de Vendas)", 
       y = "Probabilidade") +
  geom_line(data = df, aes(x = eventos, y = prob_acumulada))



