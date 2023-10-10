# Lab 2 - Inteligência Artificial em Macroeconomia Para Prever a Taxa de Inflação

# Como solução para este Lab usaremos um modelo de IA com arquitetura LSTM (Long Short-Term Memory)
# https://www.deeplearningbook.com.br/arquitetura-de-redes-neurais-long-short-term-memory/

# pip install tensorflow

# Imports
import pandas as pd
import numpy as np
import tensorflow as tf
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import mean_squared_error
import matplotlib.pyplot as plt

print("\nCarregando os Dados.")

# Carrega os dados
df_dsa = pd.read_csv('dataset.csv')

# Define nossa série temporal
inflacao = df_dsa['inflacao'].values.reshape(-1, 1)

# Função para criar o dataset para o modelo LSTM com look_back
# O parâmetro look_back define o número de passos no tempo que o modelo usará para fazer cada previsão
def func_cria_dataset(data, look_back = 1):

    # Inicializa duas listas vazias, X e Y, que conterão as sequências de entrada e saída, respectivamente
    X, Y = [], []
    
    # Itera pelo conjunto de dados menos o valor de look_back. Isso é feito para evitar índices fora dos limites do array
    for i in range(len(data) - look_back):
        
        # Coleta uma sequência de dados de tamanho look_back começando no índice i
        a = data[i:(i + look_back), 0]
        
        # Adiciona a sequência à lista X
        X.append(a)
        
        # Adiciona o valor imediatamente após a sequência de look_back à lista Y. Esse será nosso valor de saída (target).
        Y.append(data[i + look_back, 0])
    
    # Converte X e Y para arrays numpy para compatibilidade com a maioria das bibliotecas de aprendizado de máquina
    return np.array(X), np.array(Y)

# Dividimos os dados em treinamento e teste (respeitando a ordem cronológica dos dados)
indice = int(len(inflacao) * 0.8)
dados_treino, dados_teste = inflacao[0:indice, :], inflacao[indice:len(inflacao), :]

# Normaliza os dados (requerimento para redes neurais)
scaler = MinMaxScaler(feature_range = (0, 1))

# Treina e aplica o scaler em treino e somente aplica em teste
dados_treino_norm = scaler.fit_transform(dados_treino)
dados_teste_norm = scaler.transform(dados_teste)

# Criamos os datasets para o modelo LSTM
look_back = 3
X_treino, y_treino = func_cria_dataset(dados_treino_norm, look_back)
X_teste, y_teste = func_cria_dataset(dados_teste_norm, look_back)

# Reshape dos dados para [samples, time steps, features]. Isso é um requerimento do modelo LSTM
X_treino = np.reshape(X_treino, (X_treino.shape[0], X_treino.shape[1], 1))
X_teste = np.reshape(X_teste, (X_teste.shape[0], X_teste.shape[1], 1))

# Construindo o modelo LSTM
modelo = tf.keras.models.Sequential([tf.keras.layers.LSTM(50, input_shape = (look_back, 1)),
                                     tf.keras.layers.Dense(1)])

# Compila o modelo
modelo.compile(optimizer = 'adam', loss = 'mean_squared_error')

print("\nTreinamento do Modelo.\n")

# Treinamento do modelo
modelo.fit(X_treino, y_treino, epochs = 50, batch_size = 1, verbose = 1)

# Fazendo previsões com o modelo
previsao_treino = modelo.predict(X_treino)
previsao_teste = modelo.predict(X_teste)

# Transformar de volta para a escala original para calcular o erro
previsao_treino = scaler.inverse_transform(previsao_treino)
y_treino_rescaled = scaler.inverse_transform([y_treino])
previsao_teste = scaler.inverse_transform(previsao_teste)
y_teste_rescaled = scaler.inverse_transform([y_teste])

# Calcular o RMSE
train_score = np.sqrt(mean_squared_error(y_treino_rescaled[0], previsao_treino[:, 0]))
print(f"\nRMSE em Treino: {train_score:.2f}")

test_score = np.sqrt(mean_squared_error(y_teste_rescaled[0], previsao_teste[:, 0]))
print(f"RMSE em Teste: {test_score:.2f}")

# Ajustar os índices para dados originais e previsões a fim de entregar o resultado em formato de plot

# Cria um índice para os dados de treino originais, começando em 'look_back' e terminando em 'look_back + len(y_treino_rescaled[0])'.
# Este índice será utilizado para associar cada ponto de dado de treino ao seu ano correspondente no DataFrame 'df_dsa'.
original_train_data_index = df_dsa['ano'][look_back:look_back + len(y_treino_rescaled[0])]

# Cria um índice para os dados de teste originais.
# Começa a partir do final dos dados de treino padronizados e vai até o final dos dados de teste padronizados.
# O '2 * look_back' é utilizado para ajustar o índice corretamente.
original_test_data_index = df_dsa['ano'][len(y_treino_rescaled[0]) + 2 * look_back:len(y_treino_rescaled[0]) + 2 * look_back + len(y_teste_rescaled[0])]

# Cria um índice para os valores previstos em treino, começando em 'look_back' e terminando em 'look_back + len(previsao_treino)'.
# Este índice será utilizado para associar cada ponto previsto no conjunto de treino ao seu ano correspondente no DataFrame 'df_dsa'.
predicted_train_data_index = df_dsa['ano'][look_back:look_back + len(previsao_treino)]

# Cria um índice para os valores previstos em teste.
predicted_test_data_index = df_dsa['ano'][len(y_treino_rescaled[0]) + 2 * look_back:len(y_treino_rescaled[0]) + 2 * look_back+len(previsao_teste)]

# Plotar os resultados
plt.figure(figsize = (15,6))

plt.plot(original_train_data_index, y_treino_rescaled[0], label = "Dados de Treino Originais", color = "blue")

plt.plot(predicted_train_data_index, previsao_treino[:, 0], label = "Previsões em Treino", color = "green")

plt.plot(original_test_data_index, y_teste_rescaled[0], label = "Dados de Teste Originais", color = "black")

for i, value in enumerate(y_teste_rescaled[0]):
    plt.annotate(f"{value:.2f}", (original_test_data_index.iloc[i], value), fontsize = 7, ha = 'center', va = 'bottom')

plt.plot(predicted_test_data_index, previsao_teste[:, 0], label = "Previsões em Teste", color = "red")

for i, value in enumerate(previsao_teste[:, 0]):
    plt.annotate(f"{value:.2f}", (predicted_test_data_index.iloc[i], value), fontsize = 7, ha = 'center', va = 'bottom')

plt.title("Inflação Real vs. Inflação Prevista com IA")
plt.legend()
plt.show()

# Previsões com o Modelo

# Usamos as últimas 3 entradas da série original de teste para fazer a próxima previsão
last_data = dados_teste_norm[-look_back:]
last_data = np.reshape(last_data, (1, look_back, 1))

# Lista
lista_previsoes = []

# Loop de Previsão para prever 2 anos (2024 e 2025)
for _ in range(2):  

    # Previsão com o modelo (usamos os dados normalizados)
    prediction = modelo.predict(last_data)

    # Adiciona a previsão à lista de previsões
    lista_previsoes.append(prediction[0, 0])

    # Atualiza os dados para incluir a nova previsão e remover o valor mais antigo
    last_data = np.roll(last_data, shift = -1)
    last_data[0, look_back - 1, 0] = prediction

# Transformar de volta para a escala original
lista_previsoes_rescaled = scaler.inverse_transform(np.array(lista_previsoes).reshape(-1, 1))

print(f"\nPrevisão da Inflação Para 2024: {lista_previsoes_rescaled[0, 0]:.2f}")
print(f"Previsão da Inflação Para 2025: {lista_previsoes_rescaled[1, 0]:.2f}")

print("\nLab Concluído. Obrigado, DSA!")







