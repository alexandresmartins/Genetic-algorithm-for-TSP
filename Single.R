#Limpa o Environment do RStudio
rm(list = ls())

#Criação da classe indivíduo
setClass(
  "Individuo",
  slots = c(
    noscidade = "numeric",
    distCidades = "list",
    distt = "numeric",
    notaAvaliacao = "numeric",
    geracao = "numeric",
    cromossomo = "character"
  ),
  #Inicialização da Classe indivíduo
  prototype = list(
    noscidade = 0,
    distt = 0,
    distCidades = list(),
    notaAvaliacao = 0,
    geracao = 0 
  )
)

#Função que gera os cromossomos de cada indivíduo de acordo com o nó de cada cidade
gerarCromossomo = function(tamanhoCromossomo){
  cromossomo = sample(x = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", 
                            "16", "17", "18", "19", "20", "21", "22", "23", 
                            "24", "25", "26", "27", "28", "29"),size = 29, replace = FALSE)
  cromossomo[30] = cromossomo[1]
  return(cromossomo)
}

#Função fitness
avaliacao = function(individuo){
  distt = 0
  for(i in 1:29){
    for(j in 1:29){
      if(individuo@cromossomo[i]==j){
        for(k in 1:29){
          if(individuo@cromossomo[i+1] == k)
            distt = distt + individuo@distCidades[[j]][k]
        }
      }
    }
  }
  individuo@notaAvaliacao = (1/distt)*10000
  individuo@distt = distt
  return(individuo)
}

#Função de crossover
crossover = function(individuoA, individuoB){
  corte = 14
  
    aux2 = 0
    aux5 = 0
    aux2 = c(individuoA@cromossomo[1:14])
    aux5 = c(individuoB@cromossomo[1:14])
    
    #Impede que haja valores repetidos no filho1
    for (j in 15:29) {
      aux = 0 
      for(j2 in 1:14){
        if (individuoB@cromossomo[j] == individuoA@cromossomo[j2]){
          aux = aux + 1
        }
      }
      if(aux == 0){
        aux2 = c(aux2, individuoB@cromossomo[j])
      }
    }
    
    #Completa os valores faltantes no filho1
    for (j3 in 1:29){
      aux3 = 0
      for(j4 in 1:length(aux2)){
        if(j3 == aux2[j4]){
          aux3 = aux3 + 1
        }
      }
      if (aux3 == 0){
        aux2 = c(aux2, j3)
      }
    }
    
    #Impede que haja valores repetidos no filho2
    for (j in 15:29) {
      aux4 = 0 
      for(j2 in 1:14){
        if (individuoA@cromossomo[j] == individuoB@cromossomo[j2]){
          aux4 = aux4 + 1
        }
      }
      if(aux4 == 0){
        aux5 = c(aux5, individuoA@cromossomo[j])
      }
    }
    
    #Completa os valores faltantes no filho2
    for (j3 in 1:29){
      aux6 = 0
      for(j4 in 1:length(aux5)){
        if(j3 == aux5[j4]){
          aux6 = aux6 + 1
        }
      }
      if (aux6 == 0){
        aux5 = c(aux5, j3)
      }
    }
    
    filho1 = c(aux2, individuoA@cromossomo[length(individuoA@cromossomo)])
    filho2 = c(aux5, individuoB@cromossomo[length(individuoB@cromossomo)])
  
  
  filhos = list(
    new("Individuo", 
        noscidade = individuoA@noscidade,
        distCidades = individuoA@distCidades,
        geracao = individuoA@geracao + 1),
    new("Individuo", noscidade = individuoB@noscidade,
        distCidades = individuoB@distCidades,
        geracao = individuoB@geracao + 1)
  )
  
  filhos[[1]]@cromossomo = filho1
  filhos[[2]]@cromossomo = filho2
  return(filhos)
}

#Função de mutação
mutacao = function(individuo, taxaMutacao) {

    if(runif(n = 1, min = 0, max = 1) < taxaMutacao){
      aux = individuo@cromossomo[17]
      individuo@cromossomo[17] = individuo@cromossomo[22]
      individuo@cromossomo[22] = aux
      aux = individuo@cromossomo[18]
      individuo@cromossomo[18] = individuo@cromossomo[21]
      individuo@cromossomo[21] = aux
      aux = individuo@cromossomo[19]
      individuo@cromossomo[19] = individuo@cromossomo[20]
      individuo@cromossomo[20] = aux
    }

  return(individuo)
}

#Criação da classe algoritmo genético
setClass(
  "algoritmoGenetico",
  slots = c(
    tamanhoPopulacao = "numeric",
    populacao = "list",
    geracao = "numeric",
    melhorSolucao = "Individuo",
    listaSolucoes = "list"
  )
)

#Função para inicializar a população
inicializaPopulacao = function(algoritmoGenetico, noscidade, distCidades){
  for(i in 1:algoritmoGenetico@tamanhoPopulacao){
    algoritmoGenetico@populacao[[i]] = new("Individuo", noscidade = noscidade, distCidades = distCidades)
    algoritmoGenetico@populacao[[i]]@cromossomo = gerarCromossomo(length(listaCidades))
  }
  return(algoritmoGenetico)
}

#Função para ordernar a população de acordo com a melhor solução
ordenaPopulacao = function(populacao){
  populacaoOrdenada = c()
  notasAvaliacao = c()
  for(individuo in populacao){
    notasAvaliacao = c(notasAvaliacao, individuo@notaAvaliacao)
  }
  listaPosicao = order(notasAvaliacao, decreasing = TRUE)
  for(i in 1:length(listaPosicao)){
    populacaoOrdenada = c(populacaoOrdenada, populacao[[listaPosicao[i]]])
  }
  return(populacaoOrdenada)
}

#Encontra o melhor indivíduo
melhorIndividuo = function(algoritmoGenetico, individuo){
  
  if(individuo@notaAvaliacao > algoritmoGenetico@melhorSolucao@notaAvaliacao){
    algoritmoGenetico@melhorSolucao = individuo
  }
  
  algoritmoGenetico@listaSolucoes = c(algoritmoGenetico@listaSolucoes, (algoritmoGenetico@melhorSolucao@notaAvaliacao))
  
  return(algoritmoGenetico)
}

#Soma das notas de cada avaliação
somaAvaliacoes = function(algoritmoGenetico){
  soma = 0
  for(individuo in algoritmoGenetico@populacao){
    soma = soma + individuo@notaAvaliacao
  }
  return(soma)
}

#Seleciona o indivíduo pai 
selecionaPai = function(algoritmoGenetico, somaAvaliacoes){
  pai = 0
  valorSorteado = runif(1, 0, 1) * somaAvaliacoes
  soma = 0
  i = 1
  while (i < length(algoritmoGenetico@populacao) && soma<valorSorteado){
    soma = soma + algoritmoGenetico@populacao[[1]]@notaAvaliacao
    pai = pai + 1
    i = i + 1
  }
  return(pai)
}

#Visualiza o melhor indivíduo de cada geração
VisualizaGeracao = function(algoritmoGenetico){
  melhor = algoritmoGenetico@populacao[[1]]
  cat("\nG: ", melhor@geracao, "Nota: ", melhor@notaAvaliacao,
      " Distância total:", (1/melhor@notaAvaliacao)*10000, " Cromossomo: ", melhor@cromossomo)
}

#Função principal que realiza o algoritmo genético
resolver = function(algoritmoGenetico, taxaMutacao, numeroGeracoes, noscidade, distCidades){
  ag = algoritmoGenetico
  ag = inicializaPopulacao(algoritmoGenetico = ag, noscidade = noscidade, 
                           distCidades = distCidades)
  for(i in 1:ag@tamanhoPopulacao){
    ag@populacao[[i]] = avaliacao(ag@populacao[[i]])
  }
  ag@populacao = ordenaPopulacao(ag@populacao)
  ag@melhorSolucao = ag@populacao[[1]]
  VisualizaGeracao(algoritmoGenetico = ag)
  
  for(geracao in 1:numeroGeracoes){
    soma = somaAvaliacoes(algoritmoGenetico = ag)
    novaPopulacao = c()
    for(individuosGerados in 1:ag@tamanhoPopulacao/2){
      pai1 = selecionaPai(algoritmoGenetico = ag, somaAvaliacoes = soma)
      pai2 = selecionaPai(algoritmoGenetico = ag, somaAvaliacoes = soma)
      
      filhos = crossover(individuoA = ag@populacao[[pai1]], individuoB = ag@populacao[[pai2]])
      novaPopulacao = c(novaPopulacao, mutacao(individuo = filhos[[1]], taxaMutacao = taxaMutacao))
      novaPopulacao = c(novaPopulacao, mutacao(individuo = filhos[[2]], taxaMutacao = taxaMutacao))
    }
    ag@populacao = novaPopulacao
    for(i in 1:ag@tamanhoPopulacao){
      ag@populacao[[i]] = avaliacao(ag@populacao[[i]])
    }
    ag@populacao = ordenaPopulacao(ag@populacao)
    VisualizaGeracao(algoritmoGenetico = ag)
    ag = melhorIndividuo(algoritmoGenetico = ag, individuo = ag@populacao[[1]])
  }
  
  cat("\nMelhor solução - G:", ag@melhorSolucao@geracao, " Valor: ", ag@melhorSolucao@notaAvaliacao,
      "Distância total: ", (1/ag@melhorSolucao@notaAvaliacao)*10000, "Cromossomo: ", ag@melhorSolucao@cromossomo)
  return(ag)
}

#Lista das distâncias entre 29 cidades do arquivo bays29 da TSPlib
distCidades = c(0,107,241,190,124,80,316,76,152,157,283,133,113,297,228,129,348,276,188,150,65,341,184,67,221,169,108,45,167,
                107,0,148,137,88,127,336,183,134,95,254,180,101,234,175,176,265,199,182,67,42,278,271,146,251,105,191,139,79,
                241,148,0,374,171,259,509,317,217,232,491,312,280,391,412,349,422,356,355,204,182,435,417,292,424,116,337,273,77,
                190,137,374,0,202,234,222,192,248,42,117,287,79,107,38,121,152,86,68,70,137,151,239,135,137,242,165,228,205,
                124,88,171,202,0,61,392,202,46,160,319,112,163,322,240,232,314,287,238,155,65,366,300,175,307,57,220,121,97,
                80,127,259,234,61,0,386,141,72,167,351,55,157,331,272,226,362,296,232,164,85,375,249,147,301,118,188,60,185,
                316,336,509,222,392,386,0,233,438,254,202,439,235,254,210,187,313,266,154,282,321,298,168,249,95,437,190,314,435,
                76,183,317,192,202,141,233,0,213,188,272,193,131,302,233,98,344,289,177,216,141,346,108,57,190,245,43,81,243,
                152,134,217,248,46,72,438,213,0,206,365,89,209,368,286,278,360,333,284,201,111,412,321,221,353,72,266,132,111,
                157,95,232,42,160,167,254,188,206,0,159,220,57,149,80,132,193,127,100,28,95,193,241,131,169,200,161,189,163,
                283,254,491,117,319,351,202,272,365,159,0,404,176,106,79,161,165,141,95,187,254,103,279,215,117,359,216,308,322,
                133,180,312,287,112,55,439,193,89,220,404,0,210,384,325,279,415,349,285,217,138,428,310,200,354,169,241,112,238,
                113,101,280,79,163,157,235,131,209,57,176,210,0,186,117,75,231,165,81,85,92,230,184,74,150,208,104,158,206,
                297,234,391,107,322,331,254,302,368,149,106,384,186,0,69,191,59,35,125,167,255,44,309,245,169,327,246,335,288,
                228,175,412,38,240,272,210,233,286,80,79,325,117,69,0,122,122,56,56,108,175,113,240,176,125,280,177,266,243,
                129,176,349,121,232,226,187,98,278,132,161,279,75,191,122,0,244,178,66,160,161,235,118,62,92,277,55,155,275,
                348,265,422,152,314,362,313,344,360,193,165,415,231,59,122,244,0,66,178,198,286,77,362,287,228,358,299,380,319,
                276,199,356,86,287,296,266,289,333,127,141,349,165,35,56,178,66,0,112,132,220,79,296,232,181,292,233,314,253,
                188,182,355,68,238,232,154,177,284,100,95,285,81,125,56,66,178,112,0,128,167,169,179,120,69,283,121,213,281,
                150,67,204,70,155,164,282,216,201,28,187,217,85,167,108,160,198,132,128,0,88,211,269,159,197,172,189,182,135,
                65,42,182,137,65,85,321,141,111,95,254,138,92,255,175,161,286,220,167,88,0,299,229,104,236,110,149,97,108,
                341,278,435,151,366,375,298,346,412,193,103,428,230,44,113,235,77,79,169,211,299,0,353,289,213,371,290,379,332,
                184,271,417,239,300,249,168,108,321,241,279,310,184,309,240,118,362,296,179,269,229,353,0,121,162,345,80,189,342,
                67,146,292,135,175,147,249,57,221,131,215,200,74,245,176,62,287,232,120,159,104,289,121,0,154,220,41,93,218,
                221,251,424,137,307,301,95,190,353,169,117,354,150,169,125,92,228,181,69,197,236,213,162,154,0,352,147,247,350,
                169,105,116,242,57,118,437,245,72,200,359,169,208,327,280,277,358,292,283,172,110,371,345,220,352,0,265,178,39,
                108,191,337,165,220,188,190,43,266,161,216,241,104,246,177,55,299,233,121,189,149,290,80,41,147,265,0,124,263,
                45,139,273,228,121,60,314,81,132,189,308,112,158,335,266,155,380,314,213,182,97,379,189,93,247,178,124,0,199,
                167,79,77,205,97,185,435,243,111,163,322,238,206,288,243,275,319,253,281,135,108,332,342,218,350,39,263,199,0)

#Cria uma lista de lista com as distâncias de cada cidade 
listAll=list()
aux = 1
aux2 = 29
for(i in 1:29){
  list = distCidades[aux:aux2]
  listAll[[i]] = list
  aux = aux + 29
  aux2 = aux2 + 29
}

#Define os nós de cada uma das 29 cidades
noscidade = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
              26, 27, 28, 29)

tamanho = 50 #Tamanho da população
probabilidadeMutacao = 0.05 #Probabilidade de mutação
numeroGeracoes = 49 #Número de gerações (Para 50 gerações é necessário setar 49 pois a primeira se iniciar em 0)

#Chamada para execução do algoritmo genético
ag = new("algoritmoGenetico", tamanhoPopulacao = tamanho)
ag = resolver(algoritmoGenetico = ag, taxaMutacao = probabilidadeMutacao, numeroGeracoes = numeroGeracoes,
              noscidade = noscidade, distCidades=listAll)

#Plota um gráfico com a melhor solução de cada geração
plot(
  x = 1:numeroGeracoes,
  y = ag@listaSolucoes,
  type = "l",
  main = "Gráfico de convergência",
  col = "red",
  ylab = "Valor do fitness",
  xlab = "Número de gerações"
)