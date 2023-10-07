# Análise Descritiva em R

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("~/Users/pedropedrosa/Documents/Data Analytics/02-Introduction to Analytics - Part 1/Cap02-Code/R")
getwd()

# Carregando os dados
carros <- read.csv("dados/carros.csv", stringsAsFactors = FALSE)

# Sumário da estrutura do dataset
str(carros)
names(carros) <- c('Ano', 'Modelo', 'Preco', 'Kilometragem', 'Cor', 'Transmissao')
View(carros)

##### Explorando Variáveis Numéricas ##### 

# Sumário
summary(carros$Ano)
str(carros)
carros$Ano <- as.character(carros$Ano)
summary(carros$Ano)
summary(carros[c("Preco", "Kilometragem")])

# Calculando a média
(36000 + 44000 + 56000) / 3
mean(c(36000, 44000, 56000))
mean(carros$Preco)

# Mediana
median(c(36000, 44000, 56000))
median(carros$Preco)

# Range - Min e Max
range(carros$Preco)

# Diferença do Range (Max -  Min)
diff(range(carros$Preco))

# IQR - Interquantile Range
IQR(carros$Preco)

# Quartis
quantile(carros$Preco)

# Percentil 1% e 99%
quantile(carros$Preco, probs = c(0.01, 0.99))

# Percentis com intervalo de 20
quantile(carros$Preco, seq(from = 0, to = 1, by = 0.20))

# Boxplot
boxplot(carros$Preco, main = "Boxplot Preços", ylab = "Preço (R$)")
boxplot(carros$Kilometragem, main = "Boxplot Kilometragem", ylab = "Kilometragem (Km)")

# Histograma
mean(carros$Preco)
hist(carros$Preco, main = "Histograma Preços", xlab = "Preço (R$)")

mean(carros$Kilometragem)
hist(carros$Kilometragem, main = "Histograma Kilometragem", xlab = "Kilometragem (Km)")

# Assimetria e Curtose da variável Preço
library(e1071)
skewness(carros$Preco)
kurtosis(carros$Preco)

# Variância e Desvio Padrão
mean(carros$Preco)
var(carros$Preco)
sd(carros$Preco)

mean(carros$Kilometragem)
var(carros$Kilometragem)
sd(carros$Kilometragem)

# Explorando relacionamento entre as variáveis numéricas

# Scatter Plot
plot(x = carros$Kilometragem, y = carros$Preco,
     main = "Scatterplot Kilometragem x Preço",
     xlab = "Kilometragem (Km)",
     ylab = "Preço (R$)")

# Calculando o coeficiente de correlação
cor(carros$Kilometragem, carros$Preco)

# Agregação
?aggregate

# Média de preços dos carros por ano
str(carros)
aggregate(carros$Preco ~ carros$Ano, FUN = mean, data = carros)


#####  Explorando Variáveis Categóricas #####  

# Tabela de Frequência
str(carros)
?table
table(carros$Ano)
table(carros$Modelo)
table(carros$Cor)

# Proporções da Tabela de Frequência
model_table <- table(carros$Modelo)
prop.table(model_table)

# Ajuste do resultado das proporções
color_table <- table(carros$Cor)
color_pct <- prop.table(color_table) * 100
round(color_pct, digits = 1)

# Resumo gráfico e relação entre as variáveis categóricas
library(ggplot2)

# Total de veículos por tipo de transmissão
ggplot(data = carros, aes(x = as.factor(Transmissao))) +
  geom_bar(aes(y = (..count..))) +
  labs(x = "Transmissao", y = "Contagem de Carros Por Tipo de Transmissao")

str(carros)
carros$Transmissao <- as.factor(carros$Transmissao)
str(carros)

# Proporção de veículos por tipo de transmissão e por cor
ggplot(carros, aes(x = as.factor(Transmissao))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Transmissao") +
  scale_y_continuous(labels = scales::percent, name = "Proporção") +
  facet_grid(~ Cor) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))















