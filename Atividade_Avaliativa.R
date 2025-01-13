rm(list=ls()) #apaga todos os objetos do R (dados, modelos,..)

# LEITURA DOS DADOS 
#Para leitura de dados no excel é necessário usar a biblioteca readxl
library(readxl)
idosos <- read_excel("C:/Users/cayor/ufsj/8_Periodo/Estatistica/Atividade_De_Software/14idosos.xlsx")

#__________________________
# QUESTÃO 1
#DISTRIBUIÇÃO DE FREQUÊNCIAS
f.PAF <- table(idosos$PAF)
prop.PAF <- prop.table(f.PAF)
per.PAF <- prop.PAF * 100
df.PAF <- data.frame(Frequencia = f.PAF, Proporcao = prop.PAF, Porcentagem = per.PAF)
print("Distribuição de frequências para PAF")
print(df.PAF)

#Gráficos
barplot(f.PAF, xlab = "Prática de Atividade Física", ylab = "Frequência Absoluta",
        col = c("#003DA5", "#D0E8FF"), names.arg = c("Sim", "Não"), main = "Gráfico de Barras - PAF")

pie(f.PAF, labels = c("Sim", "Não"), col = c("#003DA5", "#D0E8FF"), 
    main = "Gráfico de Pizza - PAF", radius = 1.2)

#Moda para variável qualitativa
moda.PAF <- names(f.PAF)[which.max(f.PAF)]
cat("A moda da variável PAF é:", moda.PAF, "\n")
#__________________________________________________________________________________________
# QUESTÃO 2

#DISTRIBUIÇÃO DE FREQUÊNCIAS
range.Idade <- range(idosos$Idade, na.rm = TRUE)
breaks.Idade <- seq(range.Idade[1], range.Idade[2] + 5, by = 5)
classe.Idade <- cut(idosos$Idade, breaks = breaks.Idade, right = FALSE)
f.Idade <- table(classe.Idade)
prop.Idade <- prop.table(f.Idade)
per.Idade <- prop.Idade * 100
df.Idade <- data.frame(Classe = levels(classe.Idade), Frequencia = f.Idade, Proporcao = prop.Idade,
                       Porcentagem = per.Idade)
print("Distribuição de frequências para Idade")
print(df.Idade)

#Gráficos
hist(idosos$Idade, breaks = breaks.Idade, col = "#003DA5", xlab = "Idade", main = "Histograma - Idade",
     freq = TRUE)
boxplot(idosos$Idade, main = "Box Plot - Idade", ylab = "Idade", col = "#003DA5", border = "#002F6C")

#Medidas de posição e dispersão
media.Idade <- mean(idosos$Idade, na.rm = TRUE)
mediana.Idade <- median(idosos$Idade, na.rm = TRUE)
amplitude.Idade <- diff(range.Idade)
desvio.Idade <- sd(idosos$Idade, na.rm = TRUE)
cv.Idade <- (desvio.Idade / media.Idade) * 100

cat("Medidas de Posição e Dispersão para Idade:\n")
cat("Média:", media.Idade, "\n")
cat("Mediana:", mediana.Idade, "\n")
cat("Amplitude:", amplitude.Idade, "\n")
cat("Desvio Padrão:", desvio.Idade, "\n")
cat("Coeficiente de Variação:", cv.Idade, "%\n")

#__________________________________________________________________________________________
# QUESTÃO 3

#DISTRIBUIÇÃO DE FREQUÊNCIAS
f.Grupo <- table(idosos$Grupo)
prop.Grupo <- prop.table(f.Grupo)
per.Grupo <- prop.Grupo * 100
df.Grupo <- data.frame(Grupo = c("Instituição", "Comunidade"), Frequencia = f.Grupo,Proporcao = prop.Grupo,
                       Porcentagem = per.Grupo)
print("Distribuição de frequências para Grupo")
print(df.Grupo)

#Gráfico de barras
barplot(f.Grupo, xlab = "Grupo", ylab = "Frequência Absoluta", col = c("#003DA5", "#002F6C"),
        names.arg = c("Instituição", "Comunidade"), main = "Gráfico de Barras - Grupo")

#Moda para variável discreta
moda.Grupo <- names(f.Grupo)[which.max(f.Grupo)]
cat("A moda da variável Grupo é:", moda.Grupo, "\n")