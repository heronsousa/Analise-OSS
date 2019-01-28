#Analise das respostas dos Formulários OSS

library(readxl)
library(dplyr)
library(reshape2)
library(ggplot2)


options(scipen=999)
options(digits = 10)

#Define o diretório onde estão os arquivos
setwd("~/Área de Trabalho/CGU/Respostas OSS")

#Vetor com os nomes arquivos
lista_arq = list.files()

#Cria data frame com os dados das respostas recebidas
registros = data.frame()

for(i in lista_arq){
  
  arquivo <- read_excel(i, col_names=c('Nome_da_Unidade_de_Saúde', 'CNPJ_da_OSS', '2016', '2017', '2018', 'Latitude', 'Longitude'),
                        col_types=c('guess', 'guess', 'numeric', 'numeric', 'numeric', 'guess', 'guess'), skip = 1)
  
  #Define à qual estado e municipio pertence cada registro, de acordo com o nome do arquivo
  #Nome do arquivo
  nome_arq <- sub("\\..*", "", i)
  gestao <- substr(nome_arq, 0, 1)
  #Retira do nome do arquivo a sigla do estado
  estado <- substr(nome_arq, 2, 3)
  #Retira do nome do arquivo o municipio
  municipio <- substr(nome_arq, 5, nchar(nome_arq))
  
  #Identificando à qual gestão pertence cada registro
  if(gestao==1){
    arquivo$Gestao <- rep('Estadual', nrow(arquivo))
  }
  else{
    arquivo$Gestao <- rep('Municipal', nrow(arquivo))
  }
  
  #Cria uma nova coluna "Estado", replicando sua sigla pelo numero de registros que há no arquivo
  arquivo$Estado <- rep(estado, nrow(arquivo))
  #Cria uma nova coluna "Municipio", replicando seu nome pelo numero de registros que há no arquivo
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


#Transforma a coluna Estado em factor
registros$Estado <- factor(registros$Estado)

#Reformatando as colunas dos anos com os valores usando a função 'melt' do pacote 'reshape2'
dados = melt(registros, id=c('Gestao', 'Estado', 'Municipio', 'Nome_da_Unidade_de_Saúde', 'CNPJ_da_OSS', 'Latitude', 'Longitude'), variable.name="Ano", value.name = "Valor")


#Contagem de OSS em cada gestao
count_gestao <- registros %>% count(Gestao)

#Plotando quantidade de OSS por gestão
ggplot(data=count_gestao, aes(x=Gestao, y=n)) +
  geom_point(size=3) +
  geom_segment(aes(x=Gestao, xend=Gestao, y=0, yend=n)) +
  geom_text(aes(label=format(round(as.numeric(n), 2))), position=position_dodge(width=0.9), vjust=-0.7) +
  labs(title="Quantidade de OSS por gestão", y=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw()

rm(count_gestao)

#Separando os dados em estadual e municipal
estadual <- registros[which(registros$Gestao == 'Estadual'),]
municipal <- registros[which(registros$Gestao == 'Municipal'),]

#Contagem de OSS em cada estado e municipio
count_estado <- estadual %>% count(Estado)
count_municipio <- municipal %>% count(Municipio)

rm(estadual)
rm(municipal)
rm(registros)

#Plotando quantidade de OSS em cada estado
ggplot(data=count_estado, aes(x=Estado, y=n)) +
  geom_point(size=3) +
  geom_segment(aes(x=Estado, xend=Estado, y=0, yend=n)) +
  geom_text(aes(label=format(round(as.numeric(n), 2))), position=position_dodge(width=0.9), vjust=-0.7) +
  labs(title="Quantidade de OSS por estado (Gest?o Estadual)", y=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw()

#Plotando quantidade de OSS por municipio
ggplot(data=count_municipio, aes(x=Municipio, y=n)) +
  geom_point(size=3) +
  geom_segment(aes(x=Municipio, xend=Municipio, y=0, yend=n)) +
  geom_text(aes(label=format(round(as.numeric(n), 2))), position=position_dodge(width=0.9), vjust=-0.7) +
  labs(title="Quantidade de OSS por municipio (Gest?o municipal)", y=NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme_bw()

rm(count_estado)
rm(count_municipio)
rm(dados)















