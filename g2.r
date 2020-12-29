# LENDO ARQUIVO COM AMOSTRA DE DADDOS
data_frame <- read.csv2('~/Documentos/summary-measures-two-dimensional-analysis/mat_estudantes.csv', sep = ";")
g2 = data_frame$G2
function_g2 = function (g2) {
  
  # CRIANDO UMA MATRIZ COM SEUS VALORES.
  mat <- matrix(c(1, 2))
  print(mat)
  
  # DEFININDO O LAYOUT DO MEU HISTOGRAMA.
  layout(mat, c(1,1), c(2.5, 1))
  
  # IDENTIFICANDO O MAIOR VALOR DA LISTA "a".
  topox=ceiling(max(g2))
  print(topox)
  
  # aQUI DEFINIMOS OS PARÂMETROS DO NOSSO GRÁFICO, POR EXEMPLO, DISTÂNCIA.
  par(mar=c(0, 5, 2, 1))
  
  # PARTE DO HISTOGRAMA PINTADO INDO DO 1 ATÉ TOPOX COM QUEBRAS DE 2
  b <- hist (g2, breaks=c(0, seq(1, topox, 1)), include.lowest = TRUE, right = FALSE, plot= FALSE)
  
  # AQUI IDENTIFICAMOS O VALOR MÁXIMO DENTRO DO NOSSO PRIMEIRO ESQUEMA DE HISTOGRAMA
  topoy = max(c(b$counts))
  
  #cALCULANDO A PORCENTAGEM DE FREQUÊNCIA
  porcent=round((c(b$counts/length(g2))*100), 1)
  print(porcent)
  
  # JUNTANDO INFORMAÇÕES E PLOTANDO O HISTOGRAMA DAS ABSTENCES
  hist (g2,
        breaks=c(0, seq(1,topox, 1)),
        include.lowest = TRUE,
        right = FALSE,
        xlim = c(0, topox),
        ylab = "Frequência",
        main = "G2",
        col = "yellow",
        axes = FALSE,
        density = 20)
  
  # DEFININDO O PADRÃO DE PREENCHIMENTO DOS EIXOS NO GRÁFICO
  axis(1, at=seq(0,topox,by=1))
  axis(2, at=seq(0,topoy,by=1))
  
  
  # DEFININDO VALORES INICIAIS DA POSIÇÃO DAS PORCENTAGENS
  j <- 0.5
  k <- 20
  # FOR PARA PERCORRER TODAS AS PORCENTAGENS E PRINTAR NA TELA
  for (i in 1:19)
  {
    
    # J E K SÃO AS COORDENADAS DE POSICIONAMENTO DO TEXTO HÁ SER IMPRESSO.
    if (porcent[i] != 0 & !is.na(porcent[i]))
      text(j, porcent[i]+10, paste(porcent[i]))
    j<-j+1
  }
  
  par(mar=c(0, 5, 0, 1))
  
  # DEFININDO NOSSO BLOXPLOT E SUAS CONFIGURAÇÕES.
  c <-boxplot(g2, horizontal = TRUE,
              outline = FALSE,
              xlim =c(0, 2),
              ylim = c(0, topox),
              col = "yellow",
              axes=FALSE)
  
}

function_g2(g2)
