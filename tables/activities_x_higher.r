library(gmodels) 
library(readxl)

data_frame <- read.csv2('~/Documentos/summary-measures-two-dimensional-analysis/mat_estudantes.csv', sep = ";") 

CrossTable(data_frame$activities, data_frame$higher, prop.r=FALSE, prop.c =FALSE, prop.t =FALSE, prop.chisq=FALSE) 