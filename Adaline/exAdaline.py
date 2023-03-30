import numpy as np
import pandas as pd
import random as rndm

def adaline(Xin:np.array,Yin: np.array,nhiper:float,tol:float,nepocas:int):

    # Xin -> é o conjunto de dados de entrada para treinamento
    # Yin -> é o conjunto de saídas já definido - "controle"
    # nhiper -> hiperparametro passo do ajuste -> ENTENDER MELHOR
    # tol -> tolerancia do erro -> usada para parar o treinamento
    # nepocas -> numero de epocas de treino

    N = Xin.shape[0]
    n = Xin.shape[1]

    # acrescenta a coluna relativa a x^0
    Xin = np.c_[Xin,np.ones(N)]

    # Inicializando os pesos aleatoriamente

    w = []

    for i_w in range(0,n+1):
        w.append(rndm.uniform(0,1)-0.5) # gera w transposto

    w = np.asmatrix(w).T

    erroEpoca = tol+100
    epocaAtual = 0
    epocas={}
    indexList = list(np.arange(0,N,1))

    while ((erroEpoca>tol) and (epocaAtual<nepocas)):
        indexList = rndm.sample(indexList,N)
        erroQuad = 0
        for index in indexList: # percorre todos os dados
            yhat = np.dot(Xin[index],w) #1x2 * 2x1 = 1x1
            erro = Yin[index] - yhat
            w = w + nhiper*erro*Xin[index]
            erroQuad = erroQuad + erro**2

        epocaAtual = epocaAtual +1
        erroEpoca = erroQuad/N
        epocas[epocaAtual] = erroEpoca

        return [w,epocas]


if __name__=='__main__':

    ### ------- LEITURA DOS DADOS DE ENTRADA ----------- ###

    # path = 'ex3_Adaline/'
    files = ['Ex1_t', 'Ex1_x', 'Ex1_y']
    amostras = []

    for f in files:
        data = pd.read_table(f)
        for i in range(data.shape[0]):
            amostras.append(float(data.iloc[i, 0].split(' ')[-1]))
        if f.split('_')[-1] == 't':
            t = np.asmatrix(amostras).T
            amostras = []
        elif f.split('_')[-1] == 'x':
            x = np.asmatrix(amostras).T
            amostras = []
        else:
            y = np.asmatrix(amostras).T
            amostras = []

    [wr,epochs] = adaline(x,y,0.1,0.01,50)






