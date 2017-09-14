#libraries
library(readr)

#funcoes
valormax_nome <- function(nome) {
  colnames(nome)[apply(nome,1,which.max)[1]]
}
valormin_nome <- function(nome) {
  colnames(nome)[apply(nome,1,which.min)[1]]
}

#variaveis
Analise <- list()
Tendencias <- list()
PicoPreco = list()
MenorPreco = list()

#Importa a base de dados exportada do Excel em CSV
Precos_Mensais_IPA <- read_delim("Precos_Mensais_IPA.csv", ";", escape_double = FALSE, trim_ws = TRUE)

#Pega os dados originais em CSV importados no R, classifica por tempo e roda a decomposicao (stl) dos precos de cada produto, gravando numa lista
serie_dados = ts(Precos_Mensais_IPA, frequency = 12, start=c(2007,1), end=c(2016,6))
lista_dados <- serie_dados
for(i in 1:ncol(lista_dados)){
  #convert column's data into timeseries data
  data_dados = ts(lista_dados[,i],frequency=12,start=c(2007,1), end=c(2016,6))
  #calculate 'stl' and store it in a list
  Analise[colnames(lista_dados)[i]] <- stl(data_dados,s.window="periodic")
}
Datas <- list(month.abb, unique(floor(time(Analise$Coentro))))
lista_dados <- serie_dados

#Extrai apenas a coluna de tendencias [,1] para uma nova lista por produto
for(i in 1:ncol(lista_dados)){ 
  x = gettext(names(Analise[i]))
  data_dados = ts(lista_dados[,i],frequency=12,start=c(2007,1), end=c(2016,6))
  Tendencias[[x]] <- as.data.frame(t(matrix((Analise[[i]][,1]),12,dimnames=Datas)))
}

#Pega a lista de Analises e cria um arquivo com os dados para cada produto
for (i in 1:ncol(Precos_Mensais_IPA)){
  x = gettext(names(Analise[i]))
  data = as.data.frame(t(matrix((Analise[[x]][,1]),12,dimnames=Datas)))
  name <- paste("tendencias_",x,sep="")
  assign(name,data)
}

#Cria a lista MenorPreco com o mes onde os produtos estao no seu maior valor do ano
for (i in 1:ncol(Precos_Mensais_IPA)){
  x = gettext(names(Analise[i]))
  nome <- paste("tendencias_",x,sep="")
  mespicopreco <- valormax_nome (eval(parse(text = paste(nome))))
  PicoPreco[i] <- paste(x,";",mespicopreco)
}

#Cria a lista MenorPreco com o mes onde os produtos estao no seu menor valor do ano
for (i in 1:ncol(Precos_Mensais_IPA)){
  x = gettext(names(Analise[i]))
  nome <- paste("tendencias_",x,sep="")
  mespicopreco <- valormin_nome (eval(parse(text = paste(nome))))
  MenorPreco[i] <- paste(x,";",mespicopreco)
}

#Cria lista para o mes atual com pico e menores precos
MaiorPrecoSetembro <- grep(" Sep", PicoPreco, perl=TRUE, value=TRUE)
MenorPrecoSetembro <- grep(" Sep", MenorPreco, perl=TRUE, value=TRUE)

#Exporta as tabelas de dados acima para um arquivo CSV delimitado por ";"
write.table(MaiorPrecoSetembro, file="PicoPreco_Sep.csv",sep=";",row.names=F)
write.table(MenorPrecoSetembro, file="MenorPreco_Sep.csv",sep=";",row.names=F)