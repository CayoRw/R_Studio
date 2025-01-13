rm(list=ls()) #apaga todos os objetos do R (dados, modelos,..) 

##################### LEITURA DOS DADOS 

#MB<-read.table("C:/Users/cayor/ufsj/8_Periodo/Estatistica/Atividade_De_Software/Bussab_Comp_MB.txt", h=TRUE)

#Bussab <- read.delim("C:/Users/cayor/ufsj/8_Periodo/Estatistica/Atividade_De_Software/Bussab_Comp_MB.txt")

#Para leitura de dados no excel é necessário usar a biblioteca readxl
library(readxl)
Bussab <- read_excel("C:/Users/cayor/ufsj/8_Periodo/Estatistica/Atividade_De_Software/Bussab_Comp.MB.xlsx")

#####################  DISTRIBUIÇÃO DE FREQUÊNCIAS

#_________________
###Número de Filhos
f.NF<-table(Bussab$NF)
prop.NF<-prop.table(f.NF)
per.NF<-prop.NF*100
#OBS Se os dados utilizados forem da planilha excel é provável que a variável NF
#não esteja sendo entendida pelo R como um iteiro e sim como um caracter. Para
#verificar isso, ujtilize class(Bussab$NF). Se o output não for "integer", o 
#computo das proporções e porcentagens irá incluir as observações faltantes, o 
#que não desejamos. Para corrigir isso utilize o comando abaixo antes de obter 
#as frequências e porcentagens:
#Bussab$NF<-as.integer(Bussab$NF)

#.............Para ter a tabela construída
df.NF<-matrix(0,6,3)
rownames(df.NF)<-c('0','1','2','3','5','Total')
colnames(df.NF)<-c("ni","fi","100fi(%)")
df.NF[1:5,1]<-f.NF
df.NF[6,1]<-(length(Bussab$NF)-16)
df.NF[1:5,2]<-prop.NF
df.NF[6,2]<-1
df.NF[1:5,3]<-per.NF
df.NF[6,3]<-100
df.NF

#___________________
###Grau de instrução

fGI<-table(Bussab$GI)
prop.GI<-prop.table(fGI)
per.GI<-prop.GI*100

#.............Para ter a tabela construída
df.GI<-matrix(0,4,3)
rownames(df.GI)<-c('Fundamental','Médio','Superior','Total')
colnames(df.GI)<-c("ni","fi","100fi(%)")
df.GI[1:3,1]<-fGI
df.GI[4,1]<-(length(Bussab$GI))
df.GI[1:3,2]<-prop.GI
df.GI[4,2]<-1
df.GI[1:3,3]<-per.GI
df.GI[4,3]<-100
df.GI

#___________________
###Salário

#......Agrupando em classes
range(Bussab$Salario)
#nclass.Sturges(Bussab$Salario) #estima o número de classes pelo critério de Sturges; veja outras opções em nclass
#Vamos construir classes de amplitude 4
classe.sal<-cut(Bussab$Salario, breaks = seq(4, 24, length.out = 6), include.lowest = FALSE, right=FALSE)
classe.sal

#............Obtendo frequências
f.sal<-table(classe.sal)
f.sal
fr.sal<-prop.table(f.sal)
fr.sal
per.sal<-prop.table(f.sal)*100
per.sal


#.............Para ter a tabela construída
df.sal<-matrix(0,6,3)
colnames(df.sal)<-c("ni","fi","100fi(%)")
rownames(df.sal)<-c('4|-8','8|-12', '12|-16',
                    '16|-20', '20|-24','Total')
df.sal[1:5,1]<-f.sal
df.sal[6,1]<-(length(Bussab$Salario))
df.sal[1:5,2]<-fr.sal
df.sal[6,2]<-1
df.sal[1:5,3]<-per.sal
df.sal[6,3]<-100
df.sal


#####################  REPRESENTAÇÕES GRÁFICAS


####_______________Gráficos de coluna e pizza

#...................
###Número de Filhos 

barplot(f.NF, xlab='Número de Filhos', ylab='Frequência Absoluta',col=gray(seq(0.5,0.2,length=5)))
#barplot(df.NF[1:5,1], xlab='Número de Filhos', ylab='Frequência Absoluta',col=gray(seq(0.5,0.2,length=5)))

barplot(f.NF, horiz=TRUE,xlab='Frequência Absoluta', ylab='Número de Filhos',main='Gráfico de barras',col=heat.colors(5))
#barplot(df.NF[1:5,1], horiz=TRUE,xlab='Número de Filhos', ylab='Frequência Absoluta',main='Gráfico de barras',col=heat.colors(5))

pie(prop.NF,col=c("DeepSkyBlue","LightSalmon", "Yellow","Tomato","MediumOrchid") ,radius=2,main="Número de Filhos")
#pie(per.NF,col=c("DeepSkyBlue","LightSalmon", "Yellow","Tomato","MediumOrchid") ,radius=1.05,main="Número de Filhos")
#pie(df.NF[1:5,2],col=c("DeepSkyBlue","LightSalmon", "Yellow","Tomato","MediumOrchid") ,radius=1.05,main="Número de Filhos")
#...................

#...................
###Grau de instrução
barplot(fGI,col=c("Gold","Sienna", "DeepPink1") ,main="GRAU DE INSTRUÇÃO")
pie(fGI,col=c("Gold","Sienna", "DeepPink1") ,radius=1.05,main="GRAU DE INSTRUÇÃO")

####____________________________________________



####_______________Gráfico de Linha
#para exemplo hipotético do slide

ano<-c(1980,1981,1982,1983,1984,1985,1986)
vendas<-c(230,260,380,300,350,400,460)
plot(ano, vendas,type = "l") 
# o argumento type permite usar "p" para pontos, "c" para tracejado e "b" para pontos e linha

####____________________________________________



####_______________Ogivas 
#para exemplo hipotético do slide

lim<-c(45,55,65,75,85,95)
acum<-c(0,15,45,80,95,100)
plot(lim,acum,'l',xlab="Classes",ylab="Frequências acumuladas", main="Ogiva" )

####____________________________________________



####_______________Dot PLot
dotchart(Bussab$NF,lcolor = "white",pch=21)

####____________________________________________



####_______________Histograma 
#para variável salário dos empregados da companhia MB

hist.sal<-hist(Bussab$Salario, breaks=c(4,8,12,16,20,24), freq=FALSE,
               xlab="Classes Salariais",ylab="Densidades de Frequências",
               col=topo.colors(5), main="Histograma para Salários",
               xlim = c(0,25), ylim=c(0,0.1))
points(hist.sal$mids, hist.sal$density,"l",lwd=2)

####____________________________________________



####_______________Diagrama de dispersão
#exemplo dado em aula: peso(x) versus água(y)

Peso<-c(142, 201, 119, 102, 141, 124, 220, 154)
agua<-c(54,86,32,50,64,82,39,21)
plot(Peso, agua, ylab="Água")

####____________________________________________

#####################  MEDIDAS DE POSIÇÃO


####_______________Máximo e Mínimo  

#OBS: apenas para variáveis quantitativas
max(Bussab$Idade.anos., na.rm = TRUE)
min(Bussab$Idade.anos., na.rm = TRUE)
range(Bussab$Idade.anos., na.rm = TRUE) #mínimo e máximo

max(Bussab$Salario, na.rm = TRUE)
min(Bussab$Salario, na.rm = TRUE)
range(Bussab$Salario, na.rm = TRUE)
####____________________________________________


####_______________Moda   

names(f.NF)[which.max(f.NF)]
names(fGI)[which.max(fGI)] #gi<-table(idosos$GI)
####____________________________________________


####_______________Média  

#OBS: apenas para variáveis quantitativas
mean(Bussab$Idade.anos., na.rm = TRUE)
media.sal<-mean(Bussab$Salario, na.rm = TRUE)
####____________________________________________


####_______________Mediana

#OBS: para variáveis quantitativas
median(Bussab$Idade.anos., na.rm = TRUE)
median(Bussab$Salario, na.rm = TRUE)
####____________________________________________


####_______________Quantis   

#OBS: apenas para variáveis quantitativas
quantile(Bussab$Idade.anos., na.rm = TRUE)
quantile(Bussab$Salario, na.rm = TRUE)
quantile(Bussab$Salario, na.rm = TRUE, probs=0.95)
q1<-quantile(Bussab$Salario, na.rm = TRUE, probs=0.25)

####____________________________________________


#####################  MEDIDAS DE DISPERSÃO


####_______________Amplitude   

#OBS: apenas para variáveis quantitativas
diff(range(Bussab$Idade.anos., na.rm = TRUE))
diff(range(Bussab$Salario, na.rm = TRUE))
####____________________________________________


####_______________Variância   

#OBS: apenas para variáveis quantitativas
var(Bussab$Idade.anos., na.rm = TRUE)
var.sal<-var(Bussab$Salario, na.rm = TRUE)
####____________________________________________


####_______________Desvio padrão   

#OBS: apenas para variáveis quantitativas
sd(Bussab$Idade.anos., na.rm = TRUE)
sd.sal<-sd(Bussab$Salario, na.rm = TRUE)
####____________________________________________


####_______________Coeficiente de Variação   

#OBS: apenas para variáveis quantitativas
#CV=sd/mean
(sd.sal/media.sal)*100
####____________________________________________


####_______________Distância Interquartil  

#OBS: apenas para variáveis quantitativas

q3<-quantile(Bussab$Salario, na.rm = TRUE, probs=0.75)
q3-q1
####____________________________________________


####_______________Resumo das principais medidas de variáveis quantitativas   

summary(Bussab$Idade.anos.)
summary(Bussab$Salario)
####____________________________________________


#####################  BOX PLOT

boxplot(Bussab$Salario, varwidth = FALSE, notch=FALSE)
#varwidth = TRUE: caixas proporcionais ao tamanho dos intervalos 
#notch=TRUE: caixa "acinturada"


#_______________________________________________________________________________
#-------------------------------------------------------------------------------







