# LENDO ARQUIVO COM AMOSTRA DE DADDOS
data_frame <- read.csv2('~/Downloads/mat_estudantes.csv', sep = ";")

# TRANSFORMANDO A COLUNA FAILURES EM UMA LISTA
failures = data_frame$failures

# FUNÇÃO DE CRIAÇÃO DO HISTOGRAMAS E BLOXSPOT
falhas = function (failures) {
  
  # CRIANDO UMA MATRIZ COM SEUS VALORES.
  mat <- matrix(c(1, 2))
  print(mat)
  
  # DEFININDO O LAYOUT DO MEU HISTOGRAMA.
  layout(mat, c(1,1), c(2.5, 1))
  
  # IDENTIFICANDO O MAIOR VALOR DA LISTA, PARA ACHAR O VALOR MÁXIMO NO EIXO X.
  topox=ceiling(max(failures))

  # AQUI DEFINIMOS OS PARÂMETROS DO NOSSO GRÁFICO, POR EXEMPLO, DISTÂNCIA.
  par(mar=c(0, 5, 2, 1))
  
  # PARTE DO HISTOGRAMA PINTADO INDO DO 1 ATÉ TOPOX COM QUEBRAS DE 1
  b <- hist (failures, breaks=c(0, seq(1, topox, 1)), include.lowest = TRUE, right = FALSE, plot= FALSE)
  
  # AQUI DEFINIMOS O VALOR MÁXIMO PARA O EIXO Y DE ACORDO COM O MÁXIMO ENCONTRADO NO PRIMEIRO HISTOGRAMA
  topoy = max(c(b$counts))
  
  # CALCULANDO A PORCENTAGEM DE FREQUÊNCIA PARA CADA ITEM NA LISTA
  porcent=round((c(b$counts/length(failures))*100), 2)

  # JUNTANDO INFORMAÇÕES E PLOTANDO O HISTOGRAMA DAS FALHAS.
  hist (failures,
        breaks=c(0, seq(1,topox, 1)),
        include.lowest = TRUE,
        right = FALSE,
        xlim = c(0, topox),
        ylab = "Frequência",
        main = "Falhas",
        col = "red",
        axes = FALSE,
        density = 20)
  
  # AXIS SÃO AS GRADES ENVOLTA DO GRÁFICO, BAISCAMENTE OS EIXOS, AQUI DEFINIMOS UMA ESCALA DE 0.5 PARA O EIXO X E 50 PARA O EIXO Y
  axis(1, at=seq(0,topox,by=0.5))
  axis(2, at=seq(0,topoy,by=50))
  
  # DEFININDO VALORES INICIAIS DA POSIÇÃO DAS PORCENTAGENS
  j <- 0.5
  k <- 100
  
  # FOR PARA PERCORRER TODAS AS PORCENTAGENS E PRINTAR NA TELA
  for (i in 1:3)
  {
    
    # J E K SÃO AS COORDENADAS DE POSICIONAMENTO DO TEXTO HÁ SER IMPRESSO.
    text(j, k, paste(porcent[i], "%"))
    j<-j+1
  }
  
  par(mar=c(0, 5, 0, 1))
  
  # DEFININDO NOSSO BLOXPLOT E SUAS CONFIGURAÇÕES.
  c <-boxplot(failures, horizontal = TRUE,
              outline = TRUE,
              xlim =c(0, 2),
              ylim = c(0, topoy),
              col = "red",
              axes=FALSE)
  
}

# CHAMANDO A FUNÇÃO 
falhas(failures)
