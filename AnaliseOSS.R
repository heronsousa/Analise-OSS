#Analise das respostas dos Formul?rios OSS

library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyverse)


options(scipen=999)
options(digits = 10)

#Define o diret?rio onde est?o os arquivos
setwd("C:/Users/heronrs/Documents/Respostas OSS")

#Vetor com os nomes arquivos
lista_arq = list.files()

#Cria data frame com os dados das respostas recebidas
registros = data.frame()

for(i in lista_arq){
  
  arquivo <- read_excel(i, col_names=c('Nome_da_Unidade_de_Sa?de', 'CNPJ_da_OSS', '2016', '2017', '2018', 'Latitude', 'Longitude'),
                        col_types=c('guess', 'guess', 'numeric', 'numeric', 'numeric', 'guess', 'guess'), skip = 1)
  
  #Define ? qual estado e municipio pertence cada registro, de acordo com o nome do arquivo
  #Nome do arquivo
  nome_arq <- sub("\\..*", "", i)
  gestao <- substr(nome_arq, 0, 1)
  #Retira do nome do arquivo a sigla do estado
  estado <- substr(nome_arq, 2, 3)
  #Retira do nome do arquivo o municipio
  municipio <- substr(nome_arq, 5, nchar(nome_arq))
  
  #Identificando ? qual gest?o pertence cada registro
  if(gestao==1){
    arquivo$Gestao <- rep('Estadual', nrow(arquivo))
  }
  else{
    arquivo$Gestao <- rep('Municipal', nrow(arquivo))
  }
  
  #Cria uma nova coluna "Estado", replicando sua sigla pelo numero de registros que h? no arquivo
  arquivo$Estado <- rep(estado, nrow(arquivo))
  #Cria uma nova coluna "Municipio", replicando seu nome pelo numero de registros que h? no arquivo
  arquivo$Municipio <- rep(municipio, nrow(arquivo))
  
  registros <- rbind.data.frame(registros,arquivo)
  rm(arquivo)
}

rm(i)
rm(lista_arq)
rm(gestao)
rm(nome_arq)
rm(estado)
rm(municipio)


