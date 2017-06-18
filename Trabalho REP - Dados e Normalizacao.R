options(max.print=5.5E5) #Numero de linhas - retorno
options(error=recover)   #Debugar

#Importar arquivo
#library(readr)
ColVertebral <- read.table("C:/temp/column_3C.dat") 

#Data Frame
#ColVertebral
#View(ColVertebral) # Forma de Tabela
#str(ColVertebral) # Informações

#Matriz
# DH 1 - SL 3 - NO 2
MatrizColVertebral <- data.matrix(data.frame(ColVertebral, stringsAsFactors = FALSE))

# length(MatrizColVertebral) # Tamanho
# nrow(MatrizColVertebral)   # Numero de Linhas

###################################################################################################
# (a) A partir dos dados originais gere 10 data sets diferentes separando aleatoriamente os dados #
# em 60% para projeto do classificador (treinamento) e 40% para teste.                            #
###################################################################################################

# Dados de Treinamento (60%) / Teste (40%) - Aleatorios
# Data Set 1
TreinamentoAux1 = sample(1:nrow(MatrizColVertebral), nrow(MatrizColVertebral)*0.60, replace=FALSE) 
TreinamentoAux1 = sort(TreinamentoAux1) # Ordena os Indices
Treinamento1    = MatrizColVertebral[TreinamentoAux1,]  # retorna as linhas correspondendo aos indices
Teste1          = MatrizColVertebral[-TreinamentoAux1,] # pega o restante

# Data Set 2
TreinamentoAux2 = sample(1:nrow(MatrizColVertebral), nrow(MatrizColVertebral)*0.60, replace=FALSE) 
TreinamentoAux2 = sort(TreinamentoAux2) # Ordena os Indices
Treinamento2    = MatrizColVertebral[TreinamentoAux2,]  # retorna as linhas correspondendo aos indices
Teste2          = MatrizColVertebral[-TreinamentoAux2,] # pega o restante

# Data Set 3
TreinamentoAux3 = sample(1:nrow(MatrizColVertebral), nrow(MatrizColVertebral)*0.60, replace=FALSE) 
TreinamentoAux3 = sort(TreinamentoAux3) # Ordena os Indices
Treinamento3    = MatrizColVertebral[TreinamentoAux3,]  # retorna as linhas correspondendo aos indices
Teste3          = MatrizColVertebral[-TreinamentoAux3,] # pega o restante

# Data Set 4
TreinamentoAux4 = sample(1:nrow(MatrizColVertebral), nrow(MatrizColVertebral)*0.60, replace=FALSE) 
TreinamentoAux4 = sort(TreinamentoAux4) # Ordena os Indices
Treinamento4    = MatrizColVertebral[TreinamentoAux4,]  # retorna as linhas correspondendo aos indices
Teste4          = MatrizColVertebral[-TreinamentoAux4,] # pega o restante

# Data Set 5
TreinamentoAux5 = sample(1:nrow(MatrizColVertebral), nrow(MatrizColVertebral)*0.60, replace=FALSE) 
TreinamentoAux5 = sort(TreinamentoAux5) # Ordena os Indices
Treinamento5    = MatrizColVertebral[TreinamentoAux5,]  # retorna as linhas correspondendo aos indices
Teste5          = MatrizColVertebral[-TreinamentoAux5,] # pega o restante

# Data Set 6
TreinamentoAux6 = sample(1:nrow(MatrizColVertebral), nrow(MatrizColVertebral)*0.60, replace=FALSE) 
TreinamentoAux6 = sort(TreinamentoAux6) # Ordena os Indices
Treinamento6    = MatrizColVertebral[TreinamentoAux6,]  # retorna as linhas correspondendo aos indices
Teste6          = MatrizColVertebral[-TreinamentoAux6,] # pega o restante

# Data Set 7
TreinamentoAux7 = sample(1:nrow(MatrizColVertebral), nrow(MatrizColVertebral)*0.60, replace=FALSE) 
TreinamentoAux7 = sort(TreinamentoAux7) # Ordena os Indices
Treinamento7    = MatrizColVertebral[TreinamentoAux7,]  # retorna as linhas correspondendo aos indices
Teste7          = MatrizColVertebral[-TreinamentoAux7,] # pega o restante

# Data Set 8
TreinamentoAux8 = sample(1:nrow(MatrizColVertebral), nrow(MatrizColVertebral)*0.60, replace=FALSE) 
TreinamentoAux8 = sort(TreinamentoAux8) # Ordena os Indices
Treinamento8    = MatrizColVertebral[TreinamentoAux8,]  # retorna as linhas correspondendo aos indices
Teste8          = MatrizColVertebral[-TreinamentoAux8,] # pega o restante

# Data Set 9
TreinamentoAux9 = sample(1:nrow(MatrizColVertebral), nrow(MatrizColVertebral)*0.60, replace=FALSE) 
TreinamentoAux9 = sort(TreinamentoAux9) # Ordena os Indices
Treinamento9    = MatrizColVertebral[TreinamentoAux9,]  # retorna as linhas correspondendo aos indices
Teste9          = MatrizColVertebral[-TreinamentoAux9,] # pega o restante

# Data Set 10
TreinamentoAux10 = sample(1:nrow(MatrizColVertebral), nrow(MatrizColVertebral)*0.60, replace=FALSE) 
TreinamentoAux10 = sort(TreinamentoAux10) # Ordena os Indices
Treinamento10    = MatrizColVertebral[TreinamentoAux10,]  # retorna as linhas correspondendo aos indices
Teste10          = MatrizColVertebral[-TreinamentoAux10,] # pega o restante

################################
#Subset 1 (Dividindo por classe)

MatrizDH1 <- subset(Treinamento1, Treinamento1[,7] == 1)
MatrizNO1 <- subset(Treinamento1, Treinamento1[,7] == 2)
MatrizSL1 <- subset(Treinamento1, Treinamento1[,7] == 3)

#Subset 2 (Dividindo por classe)
MatrizDH2 <- subset(Treinamento2, Treinamento2[,7] == 1) 
MatrizNO2 <- subset(Treinamento2, Treinamento2[,7] == 2)
MatrizSL2 <- subset(Treinamento2, Treinamento2[,7] == 3)

#Subset 3 (Dividindo por classe)
MatrizDH3 <- subset(Treinamento3, Treinamento3[,7] == 1) 
MatrizNO3 <- subset(Treinamento3, Treinamento3[,7] == 2)
MatrizSL3 <- subset(Treinamento3, Treinamento3[,7] == 3)

#Subset 4 (Dividindo por classe)
MatrizDH4 <- subset(Treinamento4, Treinamento4[,7] == 1) 
MatrizNO4 <- subset(Treinamento4, Treinamento4[,7] == 2)
MatrizSL4 <- subset(Treinamento4, Treinamento4[,7] == 3)

#Subset 5 (Dividindo por classe)
MatrizDH5 <- subset(Treinamento5, Treinamento5[,7] == 1) 
MatrizNO5 <- subset(Treinamento5, Treinamento5[,7] == 2)
MatrizSL5 <- subset(Treinamento5, Treinamento5[,7] == 3)

#Subset 6 (Dividindo por classe)
MatrizDH6 <- subset(Treinamento6, Treinamento6[,7] == 1) 
MatrizNO6 <- subset(Treinamento6, Treinamento6[,7] == 2)
MatrizSL6 <- subset(Treinamento6, Treinamento6[,7] == 3)

#Subset 7 (Dividindo por classe)
MatrizDH7 <- subset(Treinamento7, Treinamento7[,7] == 1) 
MatrizNO7 <- subset(Treinamento7, Treinamento7[,7] == 2)
MatrizSL7 <- subset(Treinamento7, Treinamento7[,7] == 3)

#Subset 8 (Dividindo por classe)
MatrizDH8 <- subset(Treinamento8, Treinamento8[,7] == 1) 
MatrizNO8 <- subset(Treinamento8, Treinamento8[,7] == 2)
MatrizSL8 <- subset(Treinamento8, Treinamento8[,7] == 3)

#Subset 9 (Dividindo por classe)
MatrizDH9 <- subset(Treinamento9, Treinamento9[,7] == 1) 
MatrizNO9 <- subset(Treinamento9, Treinamento9[,7] == 2)
MatrizSL9 <- subset(Treinamento9, Treinamento9[,7] == 3)

#Subset 10 (Dividindo por classe)
MatrizDH10 <- subset(Treinamento10, Treinamento10[,7] == 1) 
MatrizNO10 <- subset(Treinamento10, Treinamento10[,7] == 2)
MatrizSL10 <- subset(Treinamento10, Treinamento10[,7] == 3)


####################################################################
# (b) Normalize os dados de treinamento (normalizacao estatistica) #
####################################################################

# Data Set 1 - Normalização
# -------------------------
TreiNormalizado1 <- Treinamento1[,1:7]

# Media das colunas
MD11 <- mean(TreiNormalizado1[,1])
MD12 <- mean(TreiNormalizado1[,2])
MD13 <- mean(TreiNormalizado1[,3])
MD14 <- mean(TreiNormalizado1[,4])
MD15 <- mean(TreiNormalizado1[,5])
MD16 <- mean(TreiNormalizado1[,6])

# Desvio Padrao das colunas
DP11 <- sd(TreiNormalizado1[,1])
DP12 <- sd(TreiNormalizado1[,2])
DP13 <- sd(TreiNormalizado1[,3])
DP14 <- sd(TreiNormalizado1[,4])
DP15 <- sd(TreiNormalizado1[,5])
DP16 <- sd(TreiNormalizado1[,6])

for(i in 1:nrow(TreiNormalizado1)) {
  TreiNormalizado1[i,1] <- (TreiNormalizado1[i,1] - MD11) / DP11
  TreiNormalizado1[i,2] <- (TreiNormalizado1[i,2] - MD12) / DP12
  TreiNormalizado1[i,3] <- (TreiNormalizado1[i,3] - MD13) / DP13
  TreiNormalizado1[i,4] <- (TreiNormalizado1[i,4] - MD14) / DP14
  TreiNormalizado1[i,5] <- (TreiNormalizado1[i,5] - MD15) / DP15
  TreiNormalizado1[i,6] <- (TreiNormalizado1[i,6] - MD16) / DP16
}

# Data Set 2 - Normalização
# -------------------------
TreiNormalizado2 <- Treinamento2[,1:7]

# Media das colunas
MD21 <- mean(TreiNormalizado2[,1])
MD22 <- mean(TreiNormalizado2[,2])
MD23 <- mean(TreiNormalizado2[,3])
MD24 <- mean(TreiNormalizado2[,4])
MD25 <- mean(TreiNormalizado2[,5])
MD26 <- mean(TreiNormalizado2[,6])

# Desvio Padrao das colunas
DP21 <- sd(TreiNormalizado2[,1])
DP22 <- sd(TreiNormalizado2[,2])
DP23 <- sd(TreiNormalizado2[,3])
DP24 <- sd(TreiNormalizado2[,4])
DP25 <- sd(TreiNormalizado2[,5])
DP26 <- sd(TreiNormalizado2[,6])

for(i in 1:nrow(TreiNormalizado2)) {
  TreiNormalizado2[i,1] <- (TreiNormalizado2[i,1] - MD21) / DP21
  TreiNormalizado2[i,2] <- (TreiNormalizado2[i,2] - MD22) / DP22
  TreiNormalizado2[i,3] <- (TreiNormalizado2[i,3] - MD23) / DP23
  TreiNormalizado2[i,4] <- (TreiNormalizado2[i,4] - MD24) / DP24
  TreiNormalizado2[i,5] <- (TreiNormalizado2[i,5] - MD25) / DP25
  TreiNormalizado2[i,6] <- (TreiNormalizado2[i,6] - MD26) / DP26
}

# Data Set 3 - Normalização
# -------------------------
TreiNormalizado3 <- Treinamento3[,1:7]

# Media das colunas
MD31 <- mean(TreiNormalizado3[,1])
MD32 <- mean(TreiNormalizado3[,2])
MD33 <- mean(TreiNormalizado3[,3])
MD34 <- mean(TreiNormalizado3[,4])
MD35 <- mean(TreiNormalizado3[,5])
MD36 <- mean(TreiNormalizado3[,6])

# Desvio Padrao das colunas
DP31 <- sd(TreiNormalizado3[,1])
DP32 <- sd(TreiNormalizado3[,2])
DP33 <- sd(TreiNormalizado3[,3])
DP34 <- sd(TreiNormalizado3[,4])
DP35 <- sd(TreiNormalizado3[,5])
DP36 <- sd(TreiNormalizado3[,6])

for(i in 1:nrow(TreiNormalizado3)) {
  TreiNormalizado3[i,1] <- (TreiNormalizado3[i,1] - MD31) / DP31
  TreiNormalizado3[i,2] <- (TreiNormalizado3[i,2] - MD32) / DP32
  TreiNormalizado3[i,3] <- (TreiNormalizado3[i,3] - MD33) / DP33
  TreiNormalizado3[i,4] <- (TreiNormalizado3[i,4] - MD34) / DP34
  TreiNormalizado3[i,5] <- (TreiNormalizado3[i,5] - MD35) / DP35
  TreiNormalizado3[i,6] <- (TreiNormalizado3[i,6] - MD36) / DP36
}

# Data Set 4 - Normalização
# -------------------------
TreiNormalizado4 <- Treinamento4[,1:7]

# Media das colunas
MD41 <- mean(TreiNormalizado4[,1])
MD42 <- mean(TreiNormalizado4[,2])
MD43 <- mean(TreiNormalizado4[,3])
MD44 <- mean(TreiNormalizado4[,4])
MD45 <- mean(TreiNormalizado4[,5])
MD46 <- mean(TreiNormalizado4[,6])

# Desvio Padrao das colunas
DP41 <- sd(TreiNormalizado4[,1])
DP42 <- sd(TreiNormalizado4[,2])
DP43 <- sd(TreiNormalizado4[,3])
DP44 <- sd(TreiNormalizado4[,4])
DP45 <- sd(TreiNormalizado4[,5])
DP46 <- sd(TreiNormalizado4[,6])

for(i in 1:nrow(TreiNormalizado4)) {
  TreiNormalizado4[i,1] <- (TreiNormalizado4[i,1] - MD41) / DP41
  TreiNormalizado4[i,2] <- (TreiNormalizado4[i,2] - MD42) / DP42
  TreiNormalizado4[i,3] <- (TreiNormalizado4[i,3] - MD43) / DP43
  TreiNormalizado4[i,4] <- (TreiNormalizado4[i,4] - MD44) / DP44
  TreiNormalizado4[i,5] <- (TreiNormalizado4[i,5] - MD45) / DP45
  TreiNormalizado4[i,6] <- (TreiNormalizado4[i,6] - MD46) / DP46
}

# Data Set 5 - Normalização
# -------------------------
TreiNormalizado5 <- Treinamento5[,1:7]

# Media das colunas
MD51 <- mean(TreiNormalizado5[,1])
MD52 <- mean(TreiNormalizado5[,2])
MD53 <- mean(TreiNormalizado5[,3])
MD54 <- mean(TreiNormalizado5[,4])
MD55 <- mean(TreiNormalizado5[,5])
MD56 <- mean(TreiNormalizado5[,6])

# Desvio Padrao das colunas
DP51 <- sd(TreiNormalizado5[,1])
DP52 <- sd(TreiNormalizado5[,2])
DP53 <- sd(TreiNormalizado5[,3])
DP54 <- sd(TreiNormalizado5[,4])
DP55 <- sd(TreiNormalizado5[,5])
DP56 <- sd(TreiNormalizado5[,6])

for(i in 1:nrow(TreiNormalizado5)) {
  TreiNormalizado5[i,1] <- (TreiNormalizado5[i,1] - MD51) / DP51
  TreiNormalizado5[i,2] <- (TreiNormalizado5[i,2] - MD52) / DP52
  TreiNormalizado5[i,3] <- (TreiNormalizado5[i,3] - MD53) / DP53
  TreiNormalizado5[i,4] <- (TreiNormalizado5[i,4] - MD54) / DP54
  TreiNormalizado5[i,5] <- (TreiNormalizado5[i,5] - MD55) / DP55
  TreiNormalizado5[i,6] <- (TreiNormalizado5[i,6] - MD56) / DP56
}

# Data Set 6 - Normalização
# -------------------------
TreiNormalizado6 <- Treinamento6[,1:7]

# Media das colunas
MD61 <- mean(TreiNormalizado6[,1])
MD62 <- mean(TreiNormalizado6[,2])
MD63 <- mean(TreiNormalizado6[,3])
MD64 <- mean(TreiNormalizado6[,4])
MD65 <- mean(TreiNormalizado6[,5])
MD66 <- mean(TreiNormalizado6[,6])

# Desvio Padrao das colunas
DP61 <- sd(TreiNormalizado6[,1])
DP62 <- sd(TreiNormalizado6[,2])
DP63 <- sd(TreiNormalizado6[,3])
DP64 <- sd(TreiNormalizado6[,4])
DP65 <- sd(TreiNormalizado6[,5])
DP66 <- sd(TreiNormalizado6[,6])

for(i in 1:nrow(TreiNormalizado6)) {
  TreiNormalizado6[i,1] <- (TreiNormalizado6[i,1] - MD61) / DP61
  TreiNormalizado6[i,2] <- (TreiNormalizado6[i,2] - MD62) / DP62
  TreiNormalizado6[i,3] <- (TreiNormalizado6[i,3] - MD63) / DP63
  TreiNormalizado6[i,4] <- (TreiNormalizado6[i,4] - MD64) / DP64
  TreiNormalizado6[i,5] <- (TreiNormalizado6[i,5] - MD65) / DP65
  TreiNormalizado6[i,6] <- (TreiNormalizado6[i,6] - MD66) / DP66
}

# Data Set 7 - Normalização
# -------------------------
TreiNormalizado7 <- Treinamento7[,1:7]

# Media das colunas
MD71 <- mean(TreiNormalizado7[,1])
MD72 <- mean(TreiNormalizado7[,2])
MD73 <- mean(TreiNormalizado7[,3])
MD74 <- mean(TreiNormalizado7[,4])
MD75 <- mean(TreiNormalizado7[,5])
MD76 <- mean(TreiNormalizado7[,6])

# Desvio Padrao das colunas
DP71 <- sd(TreiNormalizado7[,1])
DP72 <- sd(TreiNormalizado7[,2])
DP73 <- sd(TreiNormalizado7[,3])
DP74 <- sd(TreiNormalizado7[,4])
DP75 <- sd(TreiNormalizado7[,5])
DP76 <- sd(TreiNormalizado7[,6])

for(i in 1:nrow(TreiNormalizado7)) {
  TreiNormalizado7[i,1] <- (TreiNormalizado7[i,1] - MD71) / DP71
  TreiNormalizado7[i,2] <- (TreiNormalizado7[i,2] - MD72) / DP72
  TreiNormalizado7[i,3] <- (TreiNormalizado7[i,3] - MD73) / DP73
  TreiNormalizado7[i,4] <- (TreiNormalizado7[i,4] - MD74) / DP74
  TreiNormalizado7[i,5] <- (TreiNormalizado7[i,5] - MD75) / DP75
  TreiNormalizado7[i,6] <- (TreiNormalizado7[i,6] - MD76) / DP76
}

# Data Set 8 - Normalização
# -------------------------
TreiNormalizado8 <- Treinamento8[,1:7]

# Media das colunas
MD81 <- mean(TreiNormalizado8[,1])
MD82 <- mean(TreiNormalizado8[,2])
MD83 <- mean(TreiNormalizado8[,3])
MD84 <- mean(TreiNormalizado8[,4])
MD85 <- mean(TreiNormalizado8[,5])
MD86 <- mean(TreiNormalizado8[,6])

# Desvio Padrao das colunas
DP81 <- sd(TreiNormalizado8[,1])
DP82 <- sd(TreiNormalizado8[,2])
DP83 <- sd(TreiNormalizado8[,3])
DP84 <- sd(TreiNormalizado8[,4])
DP85 <- sd(TreiNormalizado8[,5])
DP86 <- sd(TreiNormalizado8[,6])

for(i in 1:nrow(TreiNormalizado8)) {
  TreiNormalizado8[i,1] <- (TreiNormalizado8[i,1] - MD81) / DP81
  TreiNormalizado8[i,2] <- (TreiNormalizado8[i,2] - MD82) / DP82
  TreiNormalizado8[i,3] <- (TreiNormalizado8[i,3] - MD83) / DP83
  TreiNormalizado8[i,4] <- (TreiNormalizado8[i,4] - MD84) / DP84
  TreiNormalizado8[i,5] <- (TreiNormalizado8[i,5] - MD85) / DP85
  TreiNormalizado8[i,6] <- (TreiNormalizado8[i,6] - MD86) / DP86
}

# Data Set 9 - Normalização
# -------------------------
TreiNormalizado9 <- Treinamento9[,1:7]

# Media das colunas
MD91 <- mean(TreiNormalizado9[,1])
MD92 <- mean(TreiNormalizado9[,2])
MD93 <- mean(TreiNormalizado9[,3])
MD94 <- mean(TreiNormalizado9[,4])
MD95 <- mean(TreiNormalizado9[,5])
MD96 <- mean(TreiNormalizado9[,6])

# Desvio Padrao das colunas
DP91 <- sd(TreiNormalizado9[,1])
DP92 <- sd(TreiNormalizado9[,2])
DP93 <- sd(TreiNormalizado9[,3])
DP94 <- sd(TreiNormalizado9[,4])
DP95 <- sd(TreiNormalizado9[,5])
DP96 <- sd(TreiNormalizado9[,6])

for(i in 1:nrow(TreiNormalizado9)) {
  TreiNormalizado9[i,1] <- (TreiNormalizado9[i,1] - MD91) / DP91
  TreiNormalizado9[i,2] <- (TreiNormalizado9[i,2] - MD92) / DP92
  TreiNormalizado9[i,3] <- (TreiNormalizado9[i,3] - MD93) / DP93
  TreiNormalizado9[i,4] <- (TreiNormalizado9[i,4] - MD94) / DP94
  TreiNormalizado9[i,5] <- (TreiNormalizado9[i,5] - MD95) / DP95
  TreiNormalizado9[i,6] <- (TreiNormalizado9[i,6] - MD96) / DP96
}

# Data Set 10 - Normalização
# -------------------------
TreiNormalizado10 <- Treinamento10[,1:7]

# Media das colunas
MD101 <- mean(TreiNormalizado10[,1])
MD102 <- mean(TreiNormalizado10[,2])
MD103 <- mean(TreiNormalizado10[,3])
MD104 <- mean(TreiNormalizado10[,4])
MD105 <- mean(TreiNormalizado10[,5])
MD106 <- mean(TreiNormalizado10[,6])

# Desvio Padrao das colunas
DP101 <- sd(TreiNormalizado10[,1])
DP102 <- sd(TreiNormalizado10[,2])
DP103 <- sd(TreiNormalizado10[,3])
DP104 <- sd(TreiNormalizado10[,4])
DP105 <- sd(TreiNormalizado10[,5])
DP106 <- sd(TreiNormalizado10[,6])

for(i in 1:nrow(TreiNormalizado10)) {
  TreiNormalizado10[i,1] <- (TreiNormalizado10[i,1] - MD101) / DP101
  TreiNormalizado10[i,2] <- (TreiNormalizado10[i,2] - MD102) / DP102
  TreiNormalizado10[i,3] <- (TreiNormalizado10[i,3] - MD103) / DP103
  TreiNormalizado10[i,4] <- (TreiNormalizado10[i,4] - MD104) / DP104
  TreiNormalizado10[i,5] <- (TreiNormalizado10[i,5] - MD105) / DP105
  TreiNormalizado10[i,6] <- (TreiNormalizado10[i,6] - MD106) / DP106
}

# Validar dados normalizados
TreiNormalizado1
TreiNormalizado2
TreiNormalizado3
TreiNormalizado4
TreiNormalizado5
TreiNormalizado6
TreiNormalizado7
TreiNormalizado8
TreiNormalizado9
TreiNormalizado10
#TreiNormalizado[2,2]
#TreiNormalizadoConf <- Treinamento1[,1:6]
#mean(TreiNormalizadoConf[,3])
#sd(TreiNormalizadoConf[,3])