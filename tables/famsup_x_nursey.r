library(gmodels) 
library(readxl)

data_frame <- read.csv2('~/Documentos/summary-measures-two-dimensional-analysis/mat_estudantes.csv', sep = ";") 

CrossTable(data_frame$famsup, data_frame$nursery, prop.r=FALSE, prop.c =FALSE, prop.t =TRUE, prop.chisq=TRUE) 