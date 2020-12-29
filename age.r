# LENDO ARQUIVO COM AMOSTRA DE DADDOS
data_frame <- read.csv2('~/Downloads/mat_estudantes.csv', sep = ";")

# TRANSFORMANDO A COLUNA AGE EM UMA LISTA
age = data_frame$age


# FUNÇÃO QUE MONTA OS GRÁFICOS DA IDADE.
idade = function (age) {
  
    # CRIANDO UMA MATRIZ COM SEUS VALORES.
    mat <- matrix(c(1, 2))
    print(mat)
    
    # DEFININDO O LAYOUT DO MEU HISTOGRAMA.
    layout(mat, c(1,1), c(2.5, 1))
    
    # IDENTIFICANDO O MAIOR VALOR DA LISTA, PARA ACHAR O VALOR MÁXIMO NO EIXO X.
    topox=ceiling(max(age))
    print(topox)
    
    # AQUI DEFINIMOS OS PARÂMETROS DO NOSSO GRÁFICO, POR EXEMPLO, DISTÂNCIA.
    par(mar=c(0, 5, 2, 0))
    
    # PARTE DO HISTOGRAMA PINTADO INDO DO 1 ATÉ TOPOX COM QUEBRAS DE 1
    b <- hist (age, breaks=c(0, seq(1, topox, 1)), include.lowest = TRUE, right = FALSE, plot= FALSE)
    
    # AQUI DEFINIMOS O VALOR MÁXIMO PARA O EIXO Y DE ACORDO COM O MÁXIMO ENCONTRADO NO PRIMEIRO HISTOGRAMA
    topoy = max(c(b$counts))
    
    # CALCULANDO A PORCENTAGEM DE FREQUÊNCIA PARA CADA ITEM NA LISTA
    porcent=round((c(b$counts/length(age))*100), 2)
    
    # JUNTANDO INFORMAÇÕES E PLOTANDO O HISTOGRAMA DAS FALHAS.
    hist (age,
          breaks=c(0, seq(1,topox, 1)),
          include.lowest = TRUE,
          right = FALSE,
          xlim = c(0, topox),
          ylim = c(0, topoy),
          xlab = "Frequência",
          ylab = "Frequência",
          col = "orange",
          main = "Idade",
          axes = FALSE,
          density = 20)
    
    # DEFININDO O PADRÃO DE PREENCHIMENTO DOS EIXOS NO GRÁFICO
    axis(1, at=seq(0,topox,by=1))
    axis(2, at=seq(0,topoy,by=4))
    
    # DEFININDO VALORES INICIAIS DA POSIÇÃO DAS PORCENTAGENS
    j <- 0.6
    k <- 6
    
    # FOR PARA PERCORRER TODAS AS PORCENTAGENS E PRINTAR NA TELA
    for (i in 1:22)
    {
      # J E K SÃO AS COORDENADAS DE POSICIONAMENTO DO TEXTO HÁ SER IMPRESSO. MENOS AS IGUAIS A 0
      if (porcent[i] != 0 & !is.na(porcent[i]))
      text(j, k, paste(porcent[i], "%"))
      j<-j+1
    }
    
    par(mar=c(1, 4, 3, 0))
    
    # DEFININDO NOSSO BLOXPLOT E SUAS CONFIGURAÇÕES.
    c <-boxplot(age, horizontal = TRUE,
                outline = FALSE,
                xlim =c(0, 2),
                ylim = c(0, 22),
                col = "orange",
                axes=FALSE)
  
}

# HISTOGRAMA E BLOXSPOT DA VARIÁVEL AGE
idade(age)

