# Distribuições de Probabilidade Para Variáveis Contínuas

# Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding

# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome
setwd("~/Users/pedropedrosa/Documents/Data Analytics/02-Introduction to Analytics - Part 1/Cap02-Code/R")
getwd()



######----------------- Distribuição Uniforme Contínua -----------------------######

# Uma distribuição uniforme (geralmente chamada de 'retangular') é aquela em que todos os valores estão 
# entre dois limites e ocorrem aproximadamente da mesma forma. Por exemplo, se você rolar um dado de seis 
# lados, você terá 1, 2, 3, 4, 5 ou 6. Se o rolar 6.000 vezes, provavelmente obterá aproximadamente 
# 1.000 de cada resultado ou algo aproximado a isso. Os resultados formariam uma distribuição uniforme de 1 a 6.

# A distribuição uniforme é definida em um intervalo [a, b]. A ideia é que qualquer número selecionado no 
# intervalo [a, b] tenha uma chance igual de ser selecionado.

# A distribuição uniforme contínua é a distribuição de probabilidade de seleção da variável aleatória 
# a partir do intervalo contínuo entre a e b. 

# Qual a probabilidade de jogar um dado de 6 faces não viciado e obter o valor 5? E o valor 2?

# Parâmetros para o experimento
num_amostras <- 6000                          
min <- 1                                  
max <- 6

# Jogando o dado
?sample
sample(min:max, size = 1, replace = TRUE)

# Armazenando 6 mil jogadas

# Cria um vetor vazio
jogadas1 <- vector("numeric") 

# Executa 6 mil jogadas e armazena no vetor
for (i in 1:num_amostras) {
  x <- sample(min:max, size = 1, replace = TRUE)
  jogadas1[i] <- x 
}
View(jogadas1)
table(jogadas1)

# Outra alternativa é o uso da função runif()

# Agora vamos gerar uma distribuição uniforme
?runif
jogadas2 <- runif(num_amostras, min = 1, max = 6 + 1)     
View(jogadas2)

# Vamos arredondar o resultado convertendo para valor inteiro
# A função as.integer() trunca o resultado, convertendo em números inteiros. 
# Adicionaremos 1 ao valor max para que o "arredomendamento" possa ser feito para o valor 6 também.
jogadas2 <- as.integer(runif(num_amostras, min = 1, max = 6 + 1))  
View(jogadas2)
table(jogadas2)

# Histograma
?hist
hist(jogadas1, main = paste(num_amostras," jogadas de um único dado"), breaks = seq(min-.5, max+.5, 1))  
hist(jogadas2, main = paste(num_amostras," jogadas de um único dado"), breaks = seq(min-.5, max+.5, 1)) 

# Qual a probabilidade de jogar um dado de 6 faces não viciado e obter o valor 5? E o valor 2?
# Já sabemos que a probabilidade de ter a face igual a 5 é igual a 1/6 = 0.1666. Mesmo caso para face 2.
# Isso é o que chamamos de PDF - Probability Density Function
?dunif
dunif(5, min = 1, max = 6 + 1) 
dunif(2, min = 1, max = 6 + 1) 

# Temos o pacote dice que permite fazer diversas simulações com dados
install.packages("dice")
library(dice)
?getEventProb

# Qual a probabilidade de jogar um dado de 6 faces não viciado e obter o valor 5?
getEventProb(nrolls = 1,
             ndicePerRoll = 1,
             nsidesPerDie = 6,
             eventList = list(5))

# Qual a probabilidade de jogar um dado de 6 faces não viciado e obter o valor 1 ou 2?
# 2/6 = 0.33333

# Isso é o que chamamos de CDF - Cumulative Density Function
# Obs: (punif) - é zero em x = 1 porque é aí que o seu intervalo começa.
?punif
punif(1, min = 1, max = 6 + 1) 
punif(2, min = 1, max = 6 + 1) 
punif(3, min = 1, max = 6 + 1) 

# Resposta com a função getEventProb()
getEventProb(nrolls = 1,
             ndicePerRoll = 1,
             nsidesPerDie = 6,
             eventList = list(1:2))


# Qual a probabilidade de jogar um dado de 6 faces 10 vezes e obter o valor 5?
# Podemos usar dbinom, pois estamos modelando a chance de sucesso/fracasso em um número n de tentativas.
?dbinom
dbinom(x = 1, size = 10, prob = 1/6)

# Qual a probabilidade de jogar um dado de 6 faces 10 vezes e obter o valor 5 menos de 3 vezes?
# Podemos usar dbinom, pois estamos modelando a chance de sucesso/fracasso em um número n de tentativas, e
# nesse caso de forma acumulada.
pbinom(3, size = 10, prob = 1/6)


######----------------- Distribuição Normal -----------------------######

# A Distribuição Normal é uma das mais importantes distribuições da estatística, conhecida também como 
# Distribuição de Gauss ou Gaussiana.

# Além de descrever uma série de fenômenos físicos e financeiros, possui grande uso na estatística inferencial. 
# É inteiramente descrita por seus parâmetros de média e desvio padrão, ou seja, conhecendo-se estes valores 
# consegue-se determinar qualquer probabilidade em uma distribuição Normal.

# Um interessante uso da Distribuição Normal é que ela serve de aproximação para o cálculo de outras 
# distribuições quando o número de observações for muito grande. 

# Essa importante propriedade provém do Teorema Central do Limite que diz que:
# "Toda soma de variáveis aleatórias independentes de média finita e variância limitada é aproximadamente Normal, 
# desde que o número de termos da soma seja suficientemente grande."

# O Teorema Central do Limite é talvez o conceito mais importante em estatística. Para qualquer distribuição com 
# média finita e desvio padrão, as amostras colhidas nessa população tenderão a uma distribuição normal em torno 
# da média da população à medida que o tamanho da amostra aumenta. Além disso, à medida que o tamanho da amostra 
# aumenta, a variação da média da amostra diminui.

# Uma distribuição é a maneira pela qual um conjunto de valores é distribuído por um possível intervalo de 
# valores. Uma maneira comum de visualizar uma distribuição é um histograma que mostra o número de elementos, 
# ou frequência, dentro dos intervalos de valores:
numero_vendas_dia = c(3, 5, 2, 3, 3, 6, 3, 10, 5, 5, 5, 7, 8, 7, 1, 5, 5, 4, 4, 7)

?hist
hist(numero_vendas_dia)

mean(numero_vendas_dia)
sd(numero_vendas_dia)

# A escala vertical de um 'histograma de frequência' mostra o número de observações em cada posição. 
# Opcionalmente, também podemos colocar rótulos numéricos em cima de cada barra, mostrando quantos indivíduos 
# ela representa.
# A contagem de posições pode somar qualquer número inteiro.
hist(numero_vendas_dia, 
     main = "Frequência do Número de Vendas Por Dia", 
     xlab = "Número de Vendas Por Dia",
     ylab = "Frequência",
     labels = TRUE)

# A escala vertical de um 'histograma de densidade' mostra as unidades que fazem o total de todas as barras ser igual a 1. 
# Isso permite mostrar a curva de densidade da população usando a mesma escala vertical.
# As densidades somam um, pois a integração do pdf gera a área da unidade.
hist(numero_vendas_dia, 
     main = "Densidade do Número de Vendas Por Dia", 
     xlab = "Número de Vendas Por Dia",
     ylab = "Densidade de Probabilidade", 
     probability = TRUE)

?lines
lines(density(numero_vendas_dia), col = "blue", lwd = 2)

?dnorm
dnorm(numero_vendas_dia, mean = 4.9, sd = 2.17)

# PDF (Probability Density Function) é usado para especificar a probabilidade da variável aleatória cair dentro de um 
# determinado intervalo de valores, em vez de assumir qualquer valor. Essa probabilidade é dada pela integral do PDF 
# dessa variável nesse intervalo - ou seja, é dada pela área sob a função de densidade, mas acima do eixo horizontal e 
# entre os valores mais altos e mais baixos do intervalo. Essa definição pode não fazer muito sentido; portanto, vamos 
# esclarecer o gráfico da função de densidade de probabilidade para uma distribuição normal.

# Embora a fórmula para a distribuição normal seja complexa, R contém várias funções que permitem a análise 
# de dados. O histograma suavizado associado à distribuição normal é conhecido popularmente como curva de sino:
x = seq(-3, 3, 0.1)
plot(x = x, y = dnorm(x), type = "l", bty = "n")

# A curva de sino é uma curva de densidade e a área sob a curva de sino entre um conjunto de valores 
# representa a porcentagem de números na distribuição entre esses valores.

# Em teoria das probabilidades e estatística, a função densidade de probabilidade, ou densidade de uma variável 
# aleatória contínua, é uma função que descreve a probabilidade relativa de uma variável aleatória tomar um valor 
# dado. A probabilidade da variável aleatória cair em uma faixa particular é dada pela integral da densidade dessa 
# variável sobre tal faixa - isto é, é dada pela área abaixo da função densidade mas acima do eixo horizontal 
# e entre o menor e o maior valor dessa faixa. A função densidade de probabilidade é não negativa sempre, e sua 
# integral sobre todo o espaço é igual a um. A função densidade pode ser obtida a partir da função distribuição 
# acumulada a partir da operação de derivação (quando esta é derivável).

# Para variáveis aleatórias contínuas, as probabilidades são representadas pelas áreas sob a curva.

# O valor médio de uma distribuição normal é a média, e a largura da curva de sino é definida pelo desvio padrão.

# Regra 68-95-99 para a Distribuição Normal

# 68,2% dos valores estão dentro de 1 desvio padrão da média
# 95,4% dos valores estão dentro de 2 desvios padrão da média
# 99,7% dos valores estão dentro de 3 desvios padrão da média

# O número de desvios padrão dos quais um valor se afasta da média é chamado de escore z. 
# O escore z da média é zero. Por exemplo, se a média de uma distribuição for 7 e o desvio padrão for 2, 
# um valor de 4 terá um escore z de -1,5.

valor_media = 7
valor_desvio = 2
x = 3:11
z = (x - valor_media) / valor_desvio
data.frame(x, z)

# Podemos generalizar o exemplo anterior do dado para o caso de amostras de tamanhos variados retiradas 
# de uma distribuição contínua que varia de 0-1. Esta simulação mostra a distribuição de amostras 
# dos tamanhos 1, 2, 4, ... 32 retiradas de uma distribuição uniforme. Observe que, para cada amostra, 
# estamos descobrindo o valor médio da amostra, e não a soma que estávamos fazendo no caso dos dados.

# Parâmetros para o experimento
num_amostras <- 10000             
min <- 0                       
max <- 1
n_vezes <- 6

# Ajustando a área de plotagem
par(mar = c(1,1,1,1))
op <- par(mfrow = c(n_vezes, 1))       
i2 <- 1                

# Aumentando o tamanho de cada amostra e criando um histograma
# Comprovando um dos fundamentos do Teorema Central do Limite
for (i in 1:n_vezes)               
  {  sample = rep(0, num_amostras)      
     k = 0             
  for (j in 1:i2)        
  {
    sample <- sample + runif(num_amostras, min, max) 
    k <- k+1  }
  x <- sample/k
  saida <- c(k, mean(x), sd(x))

  hist(x, xlim = range(0,1), prob = T, main = paste( "Amostras de tamanho",  k ), col = "blue")
  i2 <- 2*i2
}    

# Outro exemplo do Teorema Central do Limite:

# Reset da área de plotagem (se necessário, feche o RStudio e abra novamente)
par(mfrow = c(1,1))

# Dataset de idades de segurados
idades <- read.csv("dados/idades.csv")
head(idades)
tail(idades)

# Histograma
hist(idades$idade, right = FALSE)

# Vamos melhorar este histograma
hist(idades$idade, 
     right = FALSE, 
     breaks = seq(0,102,2), 
     col = "blue", 
     las = 1, 
     xlab = "Idade do Segurado (anos)", 
     ylab = "Frequência", 
     main = "")

# Vamos coletar várias amostras de dados
n <- 4
resultados <- vector()

for(i in 1:10000) {
  valores_idades <- sample(idades$idade, size = n, replace = FALSE)
  resultados[i] <- mean(valores_idades)
}

# Histograma do resultado
hist(resultados, 
     right = FALSE, 
     breaks = 50, 
     col = "firebrick", 
     las = 1, 
     xlab = "Média de Idade do Segurado", 
     ylab = "Frequency", 
     main = "")

# A influência do desvio padrão 

# Massa de dados
x = seq(-8, 8, length = 500)

# Desvio padrão igual a 1
y1 = dnorm(x, mean = 0, sd = 1)
plot(x, y1, type="l", lwd=2, col="red")

# Desvio padrão igual a 2
y2 = dnorm(x, mean = 0, sd = 2)
lines(x, y2, type="l", lwd=2, col="blue")


# Vamos calcular probabilidades para variável aleatória.

# Suponha que as pontuações dos exames de vestibular se enquadrem em uma distribuição normal. 
# Além disso, a nota média do teste é 72 e o desvio padrão é 9.3. 
# O exame vai de 0 a 100 pontos possíveis.

# Qual é a probabilidade de alunos conseguirem exatamente 85 pontos no exame?
# Qual é a probabilidade de alunos conseguirem menos de 70 pontos no exame?
# Qual é a probabilidade de alunos conseguirem mais de 90 pontos no exame?

# Reset da área de plotagem (se necessário, feche o RStudio e abra novamente)
par(mfrow = c(1,1))

# Gerando a massa de dados com notas aleatórias de 100 alunos

# Média e desvio
media <- 72
desvio <- 9.3

# Sequência de valores para a massa de dados
?rnorm
notas <- rnorm(100, mean = media, sd = desvio)
min(notas)
max(notas)

# Plot
hist(notas, main = "Notas Para o Exame Vestibular", xlab = "Notas", col = "blue", breaks = 10)
h <- hist(notas, main = "Notas Para o Exame Vestibular", xlab = "Notas", col = "blue", breaks = 10)
text(h$mids, h$counts, labels = h$counts, adj = c(0.5, -0.5))

# Curva PDF
probabilidades_notas = dnorm(notas, mean = media, sd = desvio) 
plot(x = notas, y = probabilidades_notas)

# Curva CDF
probabilidades_notas_cumul = pnorm(notas, mean = media, sd = desvio) 
plot(x = notas, y = probabilidades_notas_cumul)


# Qual é a probabilidade de alunos conseguirem exatamente 85 pontos no exame?
?dnorm
dnorm(85, mean = media, sd = desvio) 
dnorm(85, mean = media, sd = desvio) * 100


# Qual é a probabilidade de alunos conseguirem menos de 70 pontos no exame?
# Aplicamos a função pnorm da distribuição normal com média 72 e desvio padrão 9.3. Uma vez que 
# estamos procurando o percentual de alunos com pontuação inferior a 70, estamos interessados na cauda 
# inferior da distribuição normal.
?pnorm
pnorm(70, mean = media, sd = desvio, lower.tail = TRUE) 
pnorm(70, mean = media, sd = desvio, lower.tail = TRUE) * 100


# Qual é a probabilidade de alunos conseguirem mais de 90 pontos no exame?
# Aplicamos a função pnorm da distribuição normal com média 72 e desvio padrão 9.3. Uma vez que 
# estamos procurando o percentual de alunos com pontuação superior a 90, estamos interessados na cauda 
# superior da distribuição normal.
?pnorm
pnorm(90, mean = media, sd = desvio, lower.tail = FALSE) 
pnorm(90, mean = media, sd = desvio, lower.tail = FALSE) * 100


# As pontuações de QI das crianças são normalmente distribuídas com um média de 100 e desvio padrão de 15.  
# Que proporção de crianças deverá ter um QI entre 80 e 120? Crie um plot para demonstrar seu resultado.

# Reset da área de plotagem
par(mfrow = c(1,1))

# Variáveis
media = 100; desvio = 15
limite_inferior = 80; limite_superior = 120

# Cria uma distribuição de dados
x <- seq(-4, 4, length = 100) * desvio + media

# Calcula a PDF
hx <- dnorm(x, media, desvio)

# Plot
plot(x, hx, type = "n", xlab = "Valores de QI", ylab = "", main = "", axes = FALSE)

# Define os valores entre os limites
i <- x >= limite_inferior & x <= limite_superior

# Adiciona uma linha com valores de x e as probabilidades
lines(x, hx)

# Cria o polygon
?polygon
polygon(c(limite_inferior, x[i], limite_superior), c(0, hx[i] ,0), col = "red")

# Calcula as probabilidades acumuladas entre os limites
area <- pnorm(limite_superior, media, desvio) - pnorm(limite_inferior, media, desvio)
area

# Prepara o título para o gráfico
resultado <- paste("P(", limite_inferior, "< QI <", limite_superior,") =", signif(area, digits = 3))
mtext(resultado, 3)
axis(1, at = seq(40, 160, 20), pos=0)


# Teste de Normalidade

# Gerando 2 datasets
# O primeiro segue uma distribuição normal e o segundo segue uma distribuição uniforme
df1 = rnorm(100)
df2 = runif(100)

hist(df1)
hist(df2)

# Plot das densidades
plot(density(df1))
plot(density(df2))

# Shapiro Test
# Hipótese Nula (H0): Os dados são normalmente distribuídos. 
# Se o valor-p for maior que 0.05 não rejeitamos a hipótese nula e podemos assumir a normalidade dos dados.
# Se o valor-p for menor que 0.05 rejeitamos a hipótese nula e não podemos assumir a normalidade dos dados.
?shapiro.test
shapiro.test(df1)
shapiro.test(df2)

# Teste Visual Usando Normal Q-Q Plot
?qqnorm
?qqline
qqnorm(df1);qqline(df1, col = 2)
qqnorm(df2);qqline(df2, col = 2)

# Testando a variável que criamos anteriormente para o QI
x <- rnorm(100, mean = 100, sd = 15)
qqnorm(x);qqline(x,  col = 2)
shapiro.test(x)


######----------------- Distribuição Exponencial -----------------------######

# A distribuição exponencial descreve o tempo de chegada de uma sequência de eventos independentes, 
# aleatoriamente recorrentes.

# Suponha que o tempo médio de checkout de um caixa de supermercado seja de 3 minutos. 
# Encontre a probabilidade de uma compra de cliente ser concluída pelo caixa em menos de 2 minutos.

# A taxa de processamento de saída é igual a 1 dividido pelo tempo médio de conclusão do checkout. 
# Daí a taxa de processamento é 1/3 checkouts por minuto. 
# Aplicamos então a função pexp da distribuição exponencial com taxa = 1/3.

# A probabilidade de terminar um checkout em menos de dois minutos pelo caixa é de 48,7%
?pexp
pexp(2, rate = 1/3) 









