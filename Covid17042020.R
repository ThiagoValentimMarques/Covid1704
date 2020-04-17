#################################################################################
#                                                                               #
# Covid-19: Por que alguns países estão flexibilizando as medidas protetivas?   #
#                                                                               #
# Autor: Thiago Valentim                                     Data: 17/04/2020   #
#                                                                               #
#################################################################################

#-------------------------------- Códigos em R ---------------------------------#

#########################################################
#---------- Pacotes necessários para a análise ---------#
#########################################################

library(tidyverse)
library(ggrepel)

#########################################################
#------------------ URL dos dados ----------------------#
#########################################################

# Casos confirmados
url1 <- "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv" 

# Óbitos 
url2 <- "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_deaths_global.csv&filename=time_series_covid19_deaths_global.csv"

#########################################################
#-------------- Preparação dos dados -------------------#
#########################################################

casos <- read.csv(url1,header=TRUE)
View(casos)
obitos <- read.csv(url2,header=TRUE)

# Países que desejo fazer a análise
paises <- c("Brazil","Denmark","Austria","Korea, South","New Zealand",
            "Singapore") 

# Nomemclatura que serão exibidas nas análises
sel <- c("Brasil","Dinamarca","Áustria","Coréia do Sul","Nova Zelândia",
         "Singapura")

# População dos respectivos países
pop <- c(209300000,5806000,8859000,51640000,4886000,5639000)

# Testes para 1 milhão de pessoas
teste1M <- c(296,14223,18078,10509,14549,12423) #Fonte: worldometers

##############################################################################
################ Início da rotina para os casos ##############################
##############################################################################

casos <- casos %>%
  filter(Country.Region %in% paises)

n<-dim(casos[,-c(1,2,3,4)])[2]

matriz<-matrix("NA",ncol=length(paises),nrow=n)
matriz2<-matrix("NA",ncol=length(paises),nrow=n)
matriz3<-matrix("NA",ncol=length(paises),nrow=n)
j<-0
for(i in paises){
  valor <- as.vector(apply(casos[casos$Country.Region==i,-c(1,2,3,4)],2,sum))
  if(names(table(valor))[1]=="0"){
    cont<-table(valor)[1]
    valor<-valor[-c(1:cont)]
    last_point <- rep(NA_character_,length(valor)-1)
    last_point[length(valor)]<-sel[j+1]
    valor[(length(valor)+1):(length(valor)+cont)]<-rep("NA",cont)
    last_point[(length(last_point)+1):(length(last_point)+
                                         cont)]<-rep(NA_character_,cont)
    diario<-NULL
    diario[1]<-as.numeric(valor[1])
    for(k in 2:length(valor)){
      diario[k] <- as.numeric(valor[k])-as.numeric(valor[k-1])
    }
  }else{
    last_point <- rep(NA_character_,length(valor)-1)
    last_point[length(valor)]<-sel[j+1]
    diario<-NULL
    diario[1]<-as.numeric(valor[1])
    for(k in 2:length(valor)){
      diario[k] <- as.numeric(valor[k])-as.numeric(valor[k-1])
    }
  }
  j<-j+1
  matriz[,j]<-valor
  matriz2[,j]<-last_point
  matriz3[,j]<-diario
} 

point<-as.vector(matriz2)
casos <- as.vector(as.numeric(matriz))
diario <- as.vector(as.numeric(matriz3))
logcasos <- log10(casos)
propcasos100k <- 100000*casos/rep(pop,each=n)
propdia1m <- 1000000*diario/rep(pop,each=n)
data <- seq(as.Date("2020/02/26"), by = "day", length.out = n)
data <- rep(data,length(paises))
data <- substr(data,6,10)
País <- rep(sel,each=n)
dia <- rep(1:dim(matriz)[1],length(paises))
corona <- data.frame(data,dia,País,casos,logcasos,propcasos100k,point,
                     diario,propdia1m)
corona <- as.tibble(corona)

##############################################################################
################# Final da rotina para os casos ##############################
##############################################################################

#### Gráfico 1

ggplot(corona,aes(x=dia,y=propcasos100k,group=País,colour=País))+
  geom_line(size=1.2)+
  ylab("Casos registrados para cada 100 mil habitantes")+
  xlab("Dias a partir do primeiro caso")+ 
  labs(title="Acumulado de casos registrados por Covid-19",
caption="Fonte: Johns Hopkins School of Public Health    Autor: Thiago Valentim",
       fill="País")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  coord_cartesian(xlim = c(0,dim(matriz)[1]+10))+
  geom_label_repel(aes(label = toupper(substr(point,1,3))),
                   nudge_x = 1,
                   na.rm = TRUE,
                   size=2.3,segment.colour = "transparent")

##############################################################################
################ Início da rotina para os óbitos #############################
##############################################################################

obitos <- obitos %>%
  filter(Country.Region %in% paises)

n<-dim(obitos[,-c(1,2,3,4)])[2]

matriz<-matrix("NA",ncol=length(paises),nrow=n)
matriz2<-matrix("NA",ncol=length(paises),nrow=n)
matriz3<-matrix("NA",ncol=length(paises),nrow=n)
j<-0
for(i in paises){
  valor <- as.vector(apply(obitos[obitos$Country.Region==i,-c(1,2,3,4)],2,sum))
  if(names(table(valor))[1]=="0"){
    cont<-table(valor)[1]
    valor<-valor[-c(1:cont)]
    last_point <- rep(NA_character_,length(valor)-1)
    last_point[length(valor)]<-sel[j+1]
    valor[(length(valor)+1):(length(valor)+cont)]<-rep("NA",cont)
    last_point[(length(last_point)+1):(length(last_point)+
                                         cont)]<-rep(NA_character_,cont)
    diario<-NULL
    diario[1]<-as.numeric(valor[1])
    for(k in 2:length(valor)){
      diario[k] <- as.numeric(valor[k])-as.numeric(valor[k-1])
    }
  }else{
    last_point <- rep(NA_character_,length(valor)-1)
    last_point[length(valor)]<-sel[j+1]
    diario<-NULL
    diario[1]<-as.numeric(valor[1])
    for(k in 2:length(valor)){
      diario[k] <- as.numeric(valor[k])-as.numeric(valor[k-1])
    }
  }
  j<-j+1
  matriz[,j]<-valor
  matriz2[,j]<-last_point
  matriz3[,j]<-diario
} 

point<-as.vector(matriz2)
obitos <- as.vector(as.numeric(matriz))
diario <- as.vector(as.numeric(matriz3))
logobitos <- log10(obitos)
propobt100k <- 100000*obitos/rep(pop,each=n)
propdiaobt1m <- 1000000*diario/rep(pop,each=n)
data <- seq(as.Date("2020/02/26"), by = "day", length.out = n)
data <- rep(data,length(paises))
data <- substr(data,6,10)
País <- rep(sel,each=n)
dia <- rep(1:dim(matriz)[1],length(paises))
cor_obt <- data.frame(data,dia,País,obitos,logobitos,propobt100k,point,
                     diario,propdiaobt1m)
cor_obt <- as.tibble(cor_obt)

##############################################################################
################  Final da rotina para os óbitos  ############################
##############################################################################

##### Gráfico 2

ggplot(cor_obt,aes(x=dia,y=propobt100k,group=País,colour=País))+geom_line(size=1.2)+
ylab("Óbitos para cada 100 mil habitantes")+xlab("Dias a partir do primeiro óbito")+ 
labs(title="Evolução da Covid-19 a partir do primeiro óbito",
       caption="Fonte: Johns Hopkins School of Public Health    Autor: Thiago Valentim",
       fill="País")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  coord_cartesian(xlim = c(0,dim(matriz)[1]-25))+
  geom_label_repel(aes(label = toupper(substr(point,1,3))),
                  nudge_x = 1,
                 na.rm = TRUE,
                size=2.5,segment.colour = "transparent")

##### Gráfico 3

ggplot(cor_obt,aes(x=dia,y=propdiaobt1m,group=País,colour=País))+geom_line(size=1.0)+
  ylab("Óbitos para 1 milhão de habitantes")+
  xlab("Dias a partir do primeiro óbito")+ 
  labs(title="Registros diários de óbitos considerando a população dos países",
       caption="Fonte: Johns Hopkins School of Public Health    Autor: Thiago Valentim",
       fill="País")+facet_wrap(~País)+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=0,hjust=0.5),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))+
  coord_cartesian(xlim = c(0,dim(matriz)[1]-25))

####################################################################
#--------------------- Letalidade ---------------------------------#
####################################################################

# Países que desejo fazer a análise
paises <- c("Brazil","Denmark","Austria","Korea, South","New Zealand",
            "Singapore") 

# Nomemclatura que serão exibidas nas análises
sel <- c("Brasil","Dinamarca","Áustria","Coréia do Sul","Nova Zelândia",
         "Singapura")

letalidade <- NULL
for(i in sel){
  a<-max(corona[corona$País==i,]$casos,na.rm=TRUE)
  b<- max(cor_obt[cor_obt$País==i,]$obitos,na.rm=TRUE)
  letalidade[i]<-round(100*b/a,2)
}

let <- as.vector(letalidade)
letalidade <- data.frame(let,sel,teste1M)

######## Gráfico 4

p1<-ggplot(letalidade, aes(x=reorder(sel,desc(let)), y=let,fill=sel)) + geom_col()+
  ylab("Letalidade (%)")+xlab("Países")+ 
  labs(title="Latalidade da Covid-19",
       caption= " ",
       fill="Países")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

p2<-ggplot(letalidade, aes(x=reorder(sel,desc(teste1M)), y=teste1M,fill=sel)) + geom_col()+
  ylab("Testes por 1 mihão de pessoas")+xlab("Países")+ 
  labs(title="Testes para Covid-19",
       caption="Fonte: Johns Hopkins University e Worldometer   Autor: Thiago Valentim",
       fill="Países")+
  theme(legend.position="none", 
        axis.text.y = element_text(vjust=0.8, size = 12),
        axis.text.x = element_text(vjust=1, size = 10,angle=60,hjust=1),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)))

grid.arrange(p1, p2, nrow = 1)