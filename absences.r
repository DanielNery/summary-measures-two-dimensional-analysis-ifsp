# LENDO ARQUIVO COM AMOSTRA DE DADDOS
data_frame <- read.csv2('~/Documentos/mat_estudantes.csv', sep = ";")
absences = data_frame$absences

ausencias = function (absences) {

# CRIANDO UMA MATRIZ COM SEUS VALORES.
mat <- matrix(c(1, 2))
print(mat)

# DEFININDO O LAYOUT DO MEU HISTOGRAMA.
layout(mat, c(1,1), c(2.5, 1))

# IDENTIFICANDO O MAIOR VALOR DA LISTA "a".
topox=ceiling(max(absences))
print(topox)

# aQUI DEFINIMOS OS PARÂMETROS DO NOSSO GRÁFICO, POR EXEMPLO, DISTÂNCIA.
par(mar=c(0, 5, 2, 1))

# PARTE DO HISTOGRAMA PINTADO INDO DO 1 ATÉ TOPOX COM QUEBRAS DE 2
b <- hist (absences, breaks=c(0, seq(1, topox, 2)), include.lowest = TRUE, right = FALSE, plot= FALSE)

# AQUI IDENTIFICAMOS O VALOR MÁXIMO DENTRO DO NOSSO PRIMEIRO ESQUEMA DE HISTOGRAMA
topoy = max(c(b$counts))

#cALCULANDO A PORCENTAGEM DE FREQUÊNCIA
porcent=round((c(b$counts/length(absences))*100), 2)
print(porcent)

# JUNTANDO INFORMAÇÕES E PLOTANDO O HISTOGRAMA DAS ABSTENCES
hist (absences,
      breaks=c(0, seq(1,topox, 1)),
      include.lowest = TRUE,
      right = FALSE,
      xlim = c(0, topox),
      ylab = "Frequência",
      main = "Abstenções",
      col = "green",
      axes = FALSE,
      density = 20)

# DEFININDO O PADRÃO DE PREENCHIMENTO DOS EIXOS NO GRÁFICO
axis(1, at=seq(0,topox,by=5))
axis(2, at=seq(0,topoy,by=15))


par(mar=c(0, 5, 0, 1))

# DEFININDO NOSSO BLOXPLOT E SUAS CONFIGURAÇÕES.
c <-boxplot(absences, horizontal = TRUE,
            outline = FALSE,
            xlim =c(0, 2),
            ylim = c(0, topox),
            col = "green",
            axes=FALSE)

}

ausencias(absences)
