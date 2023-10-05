# Análise de Regressão

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("~setwd("~/Users/pedropedrosa/Documents/Data Analytics/03-Introduction to Analytics - Part 2/R")")
getwd()

# O Processo de Análise pode ser dividido em 3 grandes etapas:

# Etapa 1 - Análise Exploratória
# Etapa 2 - Pré-Processamento dos Dados
# Etapa 3 - Análise dos Dados e Modelagem Preditiva


# Explorando Testes "Motor Trend Car Road"

# Neste projeto, trabalhamos para a Motor Trend (uma revista sobre a indústria automobilística) 
# Observando um conjunto de dados de uma coleção de carros (que contém 32 observações), eles estão 
# interessados em explorar a relação entre um conjunto de variáveis e a autonomia de combustível em milhas 
# por galão (mpg), que é a nossa variável target. Estão particularmente interessados nestas duas questões:

# Qual tipo de transmissão consome menos combustível, automática ou manual?
# Quão diferente é a autonomia (mpg) entre as transmissões 'Automática' e 'Manual'?

# Dicionário de Dados

# mpg	- Milhas/galão
# cyl	- Número de cilindros
# disp	- Deslocamento
# hp	- Horsepower (cavalos de potência)
# drat	- Relação do eixo traseiro
# wt	- Peso (1000 lbs)
# qsec	- 1/4 mile time (velocidade de aceleração)
# vs	- Motor (0 = turbo, 1 = normal)
# am	- Transmissão (0 = automática, 1 = manual)
# gear	- Número de engrenagens
# carb	- Número de carburadores

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

# Carga de dados
data(mtcars)
View(mtcars)
dim(mtcars) 


#####--------------------- Análise de Regressão -----------------------------

# Nesta seção, vamos construir modelos de regressão linear com base nas diferentes variáveis de 
# interesse e tentar descobrir o melhor modelo de ajuste. Vamos criar um modelo base e então criar
# versões otimizadas e ao final avaliar e escolher o melhor modelo.

# Modelo de Regressão Linear Simples

# Também podemos ajustar variáveis fatoriais como regressores e criar uma análise de variância 
# como um caso especial de modelos de regressão linear. Do ponto de vista das "variáveis fictícias", 
# não há nada de especial na análise de variância (ANOVA). É apenas regressão linear no caso especial 
# de que todas as variáveis preditivas são categóricas. Nossa variável fator nesse caso é Transmission (am).

# Checando as variáveis
str(mtcars)

# Criando uma variável do tipo fator a partir da variável am
mtcars$amfactor <- factor(mtcars$am, labels = c("automatic", "manual")) 
str(mtcars)
View(mtcars)

# Criando o modelo base com a variável am (categórica) sendo usada como numérica
# Fórmula de regressão ==> y = a + bx
?lm
modelo_v1_base <- lm(mpg ~ am, data = mtcars)
summary(modelo_v1_base)

# Criando o modelo base com a variável am com seu tipo correto (categórica)
modelo_v1_base <- lm(mpg ~ amfactor, data = mtcars)
summary(modelo_v1_base)

# A inclinação (slope) de 7.24 é a mudança na média entre transmissão manual e transmissão automática. 
# O valor-p de 0,000285 para a diferença média de MPG entre transmissão manual e automática é 
# significativo. Portanto, concluímos que, de acordo com este modelo, a transmissão manual é 
# mais eficiente em termos de combustível.


# Regressão Linear Múltipla

# Queremos saber qual combinação de preditores melhor vai prever a eficiência do combustível. 
# Quais preditores aumentam nossa precisão em uma quantidade estatisticamente significativa? 
# Podemos adivinhar algumas das tendências do gráfico, mas realmente queremos realizar um teste 
# estatístico para determinar quais preditores são significativos e para determinar a fórmula ideal 
# para a previsão.

# A inclusão de variáveis que não deveríamos ter no modelo aumenta os erros padrão reais das 
# variáveis de regressão. Portanto, não queremos lançar variáveis no modelo à toa. 
# Para confirmar esse fato, você pode ver abaixo que, se incluirmos todas as variáveis, nenhuma 
# delas será um preditor significativo de MPG (a julgar pelo valor-p no nível de confiança de 95%).
str(mtcars)
modelo_v2 <- lm(mpg ~ cyl+disp+hp+drat+wt+qsec+factor(vs)+factor(am)+gear+carb, data = mtcars)
summary(modelo_v2)


# Detectando Colinearidade

# Um grande problema com a regressão multivariada é a colinearidade. Se duas ou mais variáveis 
# preditivas são altamente correlacionadas e ambas são inseridas em um modelo de regressão, 
# aumenta o verdadeiro erro padrão e você obtém estimativas muito instáveis da inclinação. 
# Podemos avaliar a colinearidade pelo Fator de Inflação de Variância (VIF). Vamos analisar os 
# fatores de inflação da variação se lançarmos todas as variáveis no modelo.

library(car)
?vif
kable(vif(modelo_v2), align = 'c')

# Valores para o VIF maior que 10 são considerados grandes. Também devemos prestar atenção aos 
# valores de VIF entre 5 e 10. Nesse ponto, podemos considerar deixar apenas uma dessas variáveis 
# no modelo.


# Método de Seleção Gradual de Atributos (Stepwise Selection Method)

# Entre os métodos disponíveis, vamos realizar a seleção gradual para nos ajudar a selecionar 
# um subconjunto de variáveis que melhor explicam o MPG. Observe que também tratamos a variável (vs) 
# como uma variável categórica.
?lm
modelo_v3 <- lm(mpg ~ cyl+disp+hp+drat+wt+qsec+factor(vs)+factor(am)+gear+carb, data = mtcars)

# O critério de informação de Akaike (AIC) é um estimador de erro de previsão e, portanto, estima a
# qualidade relativa de modelos estatísticos para um determinado conjunto de dados. 

# Dada uma coleção de modelos para os dados, a AIC estima a qualidade de cada modelo, em relação 
# a cada um dos outros modelos. Assim, o AIC fornece um meio para a seleção de modelos.

# O Coeficiente AIC é baseado na teoria da informação. Quando um modelo estatístico é usado para 
# representar o processo que gerou os dados, a representação quase nunca será exata; portanto, 
# algumas informações serão perdidas usando o modelo para representar o processo. A AIC estima 
# a quantidade relativa de informações perdidas por um determinado modelo: quanto menos informações 
# um modelo perde, maior a qualidade desse modelo.
?stepAIC
step <- stepAIC(modelo_v3, direction = "both", trace = FALSE)
summary(step)
summary(step)$coeff
summary(step)$r.squared


# Isso mostra que, além da transmissão, o peso do veículo (wt) e a velocidade de 
# aceleração (qsec) têm a maior relação com a explicação da variação em mpg. 
# O R^2 é de 85%, o que significa que o modelo explica 85% da variação 
# em mpg, indicando que é um modelo robusto e altamente preditivo.


# Ajustando o modelo final

# Agora, usando as variáveis selecionadas, podemos ajustar o modelo final.

modelo_final <- lm(mpg ~ wt+qsec+factor(am), data = mtcars)
summary(modelo_final)

# Você pode observar que todas as variáveis agora são estatisticamente 
# significativas. Este modelo explica 85% da variação em milhas por galão (mpg). 
# Agora, quando lemos o coeficiente para a variável am, dizemos que, em média, 
# os carros com transmissão manual têm 2,94 MPGs a mais de autonomia que os carros com transmissão 
# automática. No entanto, esse efeito foi muito maior do que quando não ajustamos 
# wt e o qsec.


# Diagnóstico de Regressão

# Realizaremos agora alguns diagnósticos no modelo de regressão final.

# Desta vez, analisando os fatores de inflação de variação, revelamos que os números 
# são razoáveis e não detectamos nenhum sinal de colinearidade.

# Detectando colinearidade
kable (vif(modelo_final), align = 'c')

# Resíduos versus os valores ajustados

# Ao plotar resíduos versus os valores ajustados, procuramos qualquer tipo de padrão. 
# O mesmo ocorre com os valores ajustados versus os padronizados, quando estamos plotando 
# uma função dos resíduos padronizados. O gráfico abaixo mostra que não existe um 
# padrão específico nos resíduos.
modelo_final$fitted.values
?qqPlot
qqPlot(modelo_final, main = "Normal Q-Q plot")


#####--------------------- Conclusões -----------------------------

# A partir do resumo (modelo_final) podemos concluir o seguinte:
coefficients(modelo_final)
confint(modelo_final)
summary(modelo_final)

# Milhas por galão (mpg) irá aumentar em 2.93 em carros com transmissão 'Manual' 
# em comparação com carros com transmissão 'Automatic' (ajustado por wt e qsec). 
# Conclusão para a Motor Trend Magazine é: 'Transmissão manual' é melhor para mpg.

# Milhas por galão (mpg) irá diminuir em 3.9 por cada 1000 lb de aumento em peso.
# Conclusão: Milhas por galão (mpg) diminui com aumento de peso (wt) do veículo.

# Milhas por galão (mpg) irá aumentar em um fator de 1.2 se aumentarmos em 1 ponto a aceleração.
# Conclusão: Milhas por galão (mpg) aumenta com um leve aumento da velocidade de aceleração.


.