# Estatística Elementar em R - Exercício

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("~/Users/pedropedrosa/Documents/Data Analytics/02-Introduction to Analytics - Part 1/Cap02-Code/R")
getwd()

# Considere os dados de exemplo a seguir.
# Calcule as estatísticas elementares e interprete o resultado da assimetria e da curtose.
# A distribuição dos dados é normal?

# Dados de exemplo
set.seed(140)
dados <- rnorm(n = 10000, mean = 55, sd = 4.5)
View(dados)
summary(dados)
mean(dados)

# Converte os dados para um dataframe para facilitar a manipulação
dataset <- data.frame(dados)

# Renomeia a coluna
colnames(dataset) <- c("Medida")
View(dataset)

# Estatísticas Elementares
mean(dataset$Medida)
median(dataset$Medida)
var(dataset$Medida)
sd(dataset$Medida)

# Moda
table(as.vector(dataset$Medida))
table(as.vector(round(dataset$Medida)))
moda <- table(as.vector(round(dataset$Medida)))
names(moda)[moda == max(moda)]

# Sumário
summary(dataset)

# Assimetria e Curtose
library(e1071)

skewness(dados)
kurtosis(dados)

# Usando a função describe
library(psych)
describe(dataset)

########## Assimetria ########## 

# Assimetria é o grau de afastamento de uma distribuição da unidade de assimetria.
# Uma Distribuição é Simétrica quando seus valores de Média, Mediana e Moda coincidem. 
# A Assimetria, dá, portanto, uma indicação da inclinação da distribuição.

# O coeficiente de assimetria (skewness) permite distinguir as distribuições assimétricas. 
# Um valor negativo indica que a cauda do lado esquerdo da função densidade de probabilidade 
# é maior que a do lado direito. Um valor positivo para a assimetria indica que a cauda do 
# lado direito é maior que a do lado esquerdo. Um valor nulo indica que os valores são 
# distribuídos de maneira relativamente iguais em ambos os lados da média, mas não implica 
# necessariamente, uma distribuição simétrica.

# A assimetria dos dados simulados é negativa e próxima de zero. 
# Isso conclui que os dados estão próximos de uma distribuição gaussiana (formato de sino), 
# mas ligeiramente inclinados para a esquerda. 


########## Curtose ########## 

# Curtose é  uma medida de dispersão que caracteriza o "achatamento" da curva da função 
# de distribuição. O que significa analisar um conjunto quanto à Curtose?

# Significa apenas verificar o “grau de achatamento da curva”. Ou seja, saber se a Curva de 
# Frequência que representa o conjunto é mais “afilada” ou mais “achatada” em relação a uma 
# Curva Padrão, chamada de Curva Normal! 

# Uma curva (um conjunto) poderá ser, quanto à sua Curtose: 

# Mesocúrtica: ou de curtose média! 
# Será essa a nossa Curva Normal. “Meso” lembra meio! Esta curva está no meio termo: 
# nem muito achatada, nem muito afilada;

# Platicúrtica: é a curva mais achatada. Seu desenho lembra o de um prato emborcado. 
# Então “prato” lembra “plati” e “plati” lembra “platicúrtica”;

# Leptocúrtica: é a curva mais afilada! 

# Quando se trata de Curtose, não há como extrairmos uma conclusão sobre qual será a situação 
# da distribuição – se mesocúrtica, platicúrtica ou leptocúrtica – apenas conhecendo os valores 
# da Média, Moda e Mediana. 

# Histograma
library(ggplot2)

# Plot
ggplot(dataset, aes(x = dataset$Medida), binwidth = 20) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Dados de Exemplo'))) + 
  ylab(expression(bold('Densidade')))




