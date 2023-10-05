# Análise de Regressão

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("~/Users/pedropedrosa/Documents/Data Analytics/03-Introduction to Analytics - Part 2/R")
getwd()

# Explorando Testes "Motor Trend Car Road"

# Neste projeto, trabalhamos para a Motor Trend (uma revista sobre a indústria automobilística) 
# Observando um conjunto de dados de uma coleção de carros (que contém 32 observações), eles estão 
# interessados em explorar a relação entre um conjunto de variáveis e a autonomia de combustível em milhas 
# por galão (mpg), que é a nossa variável target. Estão particularmente interessados nestas duas questões:

# Qual tipo de transmissão consome menos combustível, automática ou manual?
# Quão diferente é a autonomia (mpg) entre as transmissões 'Automática' e 'Manual'?


#####--------------------- Pré-processamento e Transformações de Dados -----------------------------

# Carga de dados
data(mtcars)
View(mtcars)
dim(mtcars) 

# mpg	- Milhas/galão
# cyl	- Número de cilindros
# disp	- Deslocamento
# hp	- Horsepower (cavalos de potência)
# drat	- Relação do eixo traseiro
# wt	- Peso (1000 lbs)
# qsec	- 1/4 mile time
# vs	- Motor (0 = turbo, 1 = normal)
# am	- Transmissão (0 = automática, 1 = manual)
# gear	- Número de engrenagens
# carb	- Número de carburadores


#####--------------------- Análise Exploratória -----------------------------

# Exploramos várias relações entre as variáveis de interesse e o resultado (target). Inicialmente, traçamos as 
# relações entre todas as variáveis do conjunto de dados conforme gráfico abaixo. 
# A partir da matriz de correlação notamos que variáveis como cyl, disp, hp, drat, wt, vs e am parecem ter
# alguma forte correlação com mpg. Usaremos modelos lineares para quantificar isso na próxima seção.

# Além disso, traçamos um boxplot da variável mpg quando am é 'Automático' ou 'Manual' 
# Este gráfico mostra que o mpg aumenta quando a transmissão é 'Manual'.

# Pacotes
install.packages("corrplot")
install.packages("plyr")
install.packages("printr")
install.packages("GGally")
library(corrplot)
library(plyr)
library(knitr)
library(printr)
library(GGally)
library(ggplot2)
library(MASS)

# Formatando os dados para posterior documentação
?kable
kable(head(mtcars), align = 'c')
head(mtcars)

# Tipos das variáveis e resumo estatístico
str(mtcars)
summary(mtcars)
summary(mtcars$mpg)
sum(is.na(mtcars))

# Tabela de frequência
table(mtcars$cyl)
?plyr::count
count(mtcars, 'carb')

# Gráficos

# Percentual de carros por número de cilindros
cyl_freq <- table(mtcars$cyl)
labels <- names(cyl_freq)
percent <- round(cyl_freq/sum(cyl_freq) * 100)
labels <- paste(labels, percent)
labels <- paste(labels, "%", sep = "")
length(labels)
pie(cyl_freq, labels = labels, col = rainbow(length(labels)), main = "% de Carros Por Número de Cilindros")

# Número de carros por HP
count <- table(mtcars$hp)

?barplot
barplot(count, 
        main = "Carros Por HP", 
        xlab = "HP",
        ylab = "Número de Carros")

?sort
barplot(sort(count, decreasing = TRUE), 
        main = "Carros Por HP", 
        xlab = "HP",
        ylab = "Número de Carros")

# Scatter Plot
?plot
plot(mtcars$mpg, mtcars$hp, xlab = "MPG (Autonomia)", ylab = "HP (Potência)")

# Histograma da variável alvo
hist(mtcars$mpg)
?hist
hist(mtcars$mpg, 
     breaks = 10, 
     xlab = "Milhas Por Galão", 
     main = "Histograma da Variável MPG", 
     xlim = range(10:35))

# Boxplot
?boxplot
boxplot(mtcars$mpg)

# Corrplot
?cor
m_cor <- cor(mtcars)
?corrplot
corrplot(m_cor, method = "circle")

# Pair Plot
pairs(mtcars, panel = panel.smooth, main = "Pair Graphs")

# Também vale a pena verificar como o MPG varia de acordo com a transmissão automática 
# versus a manual. Para esse efeito, criamos um gráfico de violino para MPG através de 
# transmissões automáticas e manuais. No nosso conjunto de dados, 0 representa uma 
# transmissão automática e 1 significa uma transmissão manual.
# Violin Plot
?ggplot
ggplot(mtcars, aes(y = mpg, 
                   x = factor(am, labels = c("automatic", "manual")), 
                   fill = factor(am))) +
  geom_violin(colour = "black", size = 1) +
  xlab("Transmissão") + 
  ylab("MPG")

# Podemos criar uma hipótese clara a partir dessa visualização: parece que os carros 
# automáticos têm milhas mais baixas por galão e, portanto, uma menor eficiência de 
# combustível do que os carros manuais. Mas é possível que esse padrão aparente tenha 
# acontecido por acaso - ou seja, que tenhamos escolhido um grupo de carros automáticos 
# com baixa eficiência e um grupo de carros manuais com maior eficiência. Portanto, para 
# verificar se é esse o caso, precisamos usar um teste estatístico.

# Função para diversos gráficos estatísticos em um único plot
cria_plot <- function(data, mapping, method = "loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point() + 
    geom_smooth(method = method, ...)
  p
}

# Cria o plot
?ggpairs
ggpairs(mtcars, lower = list(continuous = cria_plot))


# Teste Estatístico
# Teste t de duas amostras

# Estamos interessados em saber se uma transmissão automática ou manual é melhor para MPG.
# Então, primeiro testamos a hipótese de que carros com transmissão automática consomem 
# mais combustível do que carros com transmissão manual. Para comparar duas amostras e 
# verificar se elas têm médias diferentes, usamos o Teste T de duas amostras.

# Existem 3 tipos comuns de Teste t:

# Teste t para duas amostras independentes (ou não pareadas) - usaremos este!
# Teste t para duas amostras dependentes (ou pareadas)
# Teste t para uma amostra
?t.test
View(mtcars)
teste <- t.test(mpg ~ am, data = mtcars, var.equal = FALSE, paired = FALSE, conf.level = .95)
print(teste)

# Um dos testes mais comuns em estatística é o Teste t, usado para determinar se as médias 
# de dois grupos são iguais entre si. Este é um teste paramétrico e a suposição para o teste 
# é que ambos os grupos são amostrados de distribuições normais e com variâncias iguais. 

# Ou seja, o Teste t pode ser implementado para determinar se as amostras são diferentes. 

# Vamos testar as suposições?

# Extraindo os registros segmentados pelo tipo de transmissão
str(mtcars)
mtcars$am <- as.factor(mtcars$am)
str(mtcars)
levels(mtcars$am) <- c("Automatic", "Manual")

Automatic <- mtcars[mtcars$am == "Automatic",]
Manual <- mtcars[mtcars$am == "Manual",]
View(Automatic)
View(Manual)

# Médias e Sumário
mean(Automatic$mpg)
mean(Manual$mpg)
summary(mtcars$mpg)

# Teste de Normalidade - Shapiro Test
# Hipótese Nula (H0): Os dados são normalmente distribuídos. 
# Hipótese Alternativa (H1): Os dados não são normalmente distribuídos. 

# Se o valor-p for maior que 0.05 não rejeitamos a hipótese nula e podemos assumir a normalidade dos dados.
# Se o valor-p for menor que 0.05 rejeitamos a hipótese nula e não podemos assumir a normalidade dos dados.
?shapiro.test
mtcars$mpg[mtcars$am == "Automatic"]
shapiro.test(mtcars$mpg[mtcars$am == "Automatic"])
shapiro.test(Manual$mpg)

# Teste de Homogeneidade das Variâncias - Bartlett’s Test
# A hipótese nula (H0) para o teste é que as variâncias são iguais para todas as amostras. 
# A hipótese alternativa (H1) (a que você está testando) é que as variâncias não são iguais.

# Se o valor-p for maior que 0.05 não rejeitamos a hipótese nula e podemos assumir que as variâncias são iguais para todas as amostras.
# Se o valor-p for menor que 0.05 rejeitamos a hipótese nula e não podemos assumir que as variâncias são iguais para todas as amostras.
?bartlett.test
bartlett.test(mpg ~ am, data = mtcars)

# Obs: O leveneTest() é usado quando os dados não são normalmente distribuídos.

# O teste t de Student (ou simplesmente teste t) compara duas médias e mostra se as 
# diferenças entre essas médias são significativas. Em outras palavras, permite que você 
# avalie se essas diferenças ocorreram por um mero acaso ou não.

# A necessidade de determinar se duas médias de amostras são diferentes entre si é uma 
# situação extremamente frequente em pesquisas científicas.

# Por exemplo se um grupo experimental difere de um grupo controle, se uma amostra difere 
# da população, se um grupo difere antes e depois de um procedimento. Nessas diversas 
# situações, um método bastante comum é a comparação das médias da medida de interesse.

# A hipótese nula é que as duas médias são iguais, e a alternativa é que não são. Sabe-se 
# que, sob a hipótese nula, podemos calcular uma estatística t que seguirá uma 
# distribuição t com n1 + n2 - 2 graus de liberdade. Há também uma modificação amplamente 
# usada do teste t, conhecida como Teste t de Welch, que ajusta o número de graus de 
# liberdade quando se pensa que as variações não são iguais umas às outras. Esse é o teste usado em R.

# Como todo teste estatístico, a teste t também tem como produto a medida do valor-p. 
# Ou seja, no final das contas, teremos calculado a probabilidade da diferença encontrada 
# (entre as médias) terem sido por acaso. Se esse valor for menor que 5% (p < 0.05), 
# a tradição científica é de rejeitarmos a hipótese de que as diferenças sejam por acaso 
# (rejeitamos a hipótese nula) e alegamos termos encontrado uma diferença estatísticamente 
# significativa.

# Teste t
teste <- t.test(mpg ~ am, data = mtcars, var.equal = TRUE, paired = FALSE, conf.level = .95)
print(teste)

# Hipóteses:
# H0 (hipótese nula): a verdadeira diferença das médias é igual a 0
# H1 (hipótese alternativa): a verdadeira diferença das médias não é igual a 0

# O valor-p que mostra a probabilidade de que essa aparente diferença entre os dois grupos 
# possa aparecer por acaso é muito baixo.

# Rejeitamos a hipótese nula e portanto as médias apresentam diferenças. Conclui-se que 
# as médias de consumo de combustível entre os tipos de transmissão são diferentes e não são fruto do acaso.

# Boxplots
?boxplot
boxplot(Automatic$mpg, Manual$mpg)




