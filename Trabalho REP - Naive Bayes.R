#################################
# (d) Classificador Naive Bayes #
#################################

# Data Set 1
############
# Treinamento Normalizado
TreiNormalizado1

#Subset 1 (Dividindo por classe)
MatrizDH1_NB <- subset(TreiNormalizado1, TreiNormalizado1[,7] == 1)
MatrizNO1_NB <- subset(TreiNormalizado1, TreiNormalizado1[,7] == 2)
MatrizSL1_NB <- subset(TreiNormalizado1, TreiNormalizado1[,7] == 3)

######################################
# Centroides - DataSet 1 (Normalizado)
MD_DH1_NB = matrix(c(mean(MatrizDH1_NB[,1]), mean(MatrizDH1_NB[,2]), mean(MatrizDH1_NB[,3]), mean(MatrizDH1_NB[,4]), mean(MatrizDH1_NB[,5]), mean(MatrizDH1_NB[,6]), 1),1,7,1)
MD_NO1_NB = matrix(c(mean(MatrizNO1_NB[,1]), mean(MatrizNO1_NB[,2]), mean(MatrizNO1_NB[,3]), mean(MatrizNO1_NB[,4]), mean(MatrizNO1_NB[,5]), mean(MatrizNO1_NB[,6]), 2),1,7,1)
MD_SL1_NB = matrix(c(mean(MatrizSL1_NB[,1]), mean(MatrizSL1_NB[,2]), mean(MatrizSL1_NB[,3]), mean(MatrizSL1_NB[,4]), mean(MatrizSL1_NB[,5]), mean(MatrizSL1_NB[,6]), 3),1,7,1)

################################################
# Matrizes de Variancias - Identidade (Data Set 1)
# DH
varMatrizDH1_NB <- cov(MatrizDH1_NB[,1:6])

for(i in 1:nrow(varMatrizDH1_NB)) {
  for(j in 1:nrow(varMatrizDH1_NB)) {
    if (i==j){
      varMatrizDH1_NB[i,j] = varMatrizDH1_NB[i,j]
    }
    else{
      varMatrizDH1_NB[i,j] = 0
    }
  }
}

# NO
varMatrizNO1_NB <- cov(MatrizNO1_NB[,1:6])

for(i in 1:nrow(varMatrizNO1_NB)) {
  for(j in 1:nrow(varMatrizNO1_NB)) {
    if (i==j){
      varMatrizNO1_NB[i,j] = varMatrizNO1_NB[i,j]
    }
    else{
      varMatrizNO1_NB[i,j] = 0
    }
  }
}

# SL
varMatrizSL1_NB <- cov(MatrizSL1_NB[,1:6])

for(i in 1:nrow(varMatrizSL1_NB)) {
  for(j in 1:nrow(varMatrizSL1_NB)) {
    if (i==j){
      varMatrizSL1_NB[i,j] = varMatrizSL1_NB[i,j]
    }
    else{
      varMatrizSL1_NB[i,j] = 0
    }
  }
}

# Matrizes de Variancias - Identidade (Data Set 1)
varMatrizDH1_NB
varMatrizNO1_NB
varMatrizSL1_NB

# Normalizacao do Teste com base na media e desvios-padrao do Treino
TesteNormalizado1 <- Teste1[,1:7]

for(i in 1:nrow(TesteNormalizado1)) {
  TesteNormalizado1[i,1] <- (TesteNormalizado1[i,1] - MD11) / DP11
  TesteNormalizado1[i,2] <- (TesteNormalizado1[i,2] - MD12) / DP12
  TesteNormalizado1[i,3] <- (TesteNormalizado1[i,3] - MD13) / DP13
  TesteNormalizado1[i,4] <- (TesteNormalizado1[i,4] - MD14) / DP14
  TesteNormalizado1[i,5] <- (TesteNormalizado1[i,5] - MD15) / DP15
  TesteNormalizado1[i,6] <- (TesteNormalizado1[i,6] - MD16) / DP16
}

# Data Set 2
############
# Treinamento Normalizado
TreiNormalizado2

#Subset 2 (Dividindo por classe)
MatrizDH2_NB <- subset(TreiNormalizado2, TreiNormalizado2[,7] == 1)
MatrizNO2_NB <- subset(TreiNormalizado2, TreiNormalizado2[,7] == 2)
MatrizSL2_NB <- subset(TreiNormalizado2, TreiNormalizado2[,7] == 3)

######################################
# Centroides - DataSet 2 (Normalizado)
MD_DH2_NB = matrix(c(mean(MatrizDH2_NB[,1]), mean(MatrizDH2_NB[,2]), mean(MatrizDH2_NB[,3]), mean(MatrizDH2_NB[,4]), mean(MatrizDH2_NB[,5]), mean(MatrizDH2_NB[,6]), 1),1,7,1)
MD_NO2_NB = matrix(c(mean(MatrizNO2_NB[,1]), mean(MatrizNO2_NB[,2]), mean(MatrizNO2_NB[,3]), mean(MatrizNO2_NB[,4]), mean(MatrizNO2_NB[,5]), mean(MatrizNO2_NB[,6]), 2),1,7,1)
MD_SL2_NB = matrix(c(mean(MatrizSL2_NB[,1]), mean(MatrizSL2_NB[,2]), mean(MatrizSL2_NB[,3]), mean(MatrizSL2_NB[,4]), mean(MatrizSL2_NB[,5]), mean(MatrizSL2_NB[,6]), 3),1,7,1)

#################################
# Matriz de Variancias - Identidade (Data Set 2)
# DH
varMatrizDH2_NB <- cov(MatrizDH2_NB[,1:6])

for(i in 1:nrow(varMatrizDH2_NB)) {
  for(j in 1:nrow(varMatrizDH2_NB)) {
    if (i==j){
      varMatrizDH2_NB[i,j] = varMatrizDH2_NB[i,j]
    }
    else{
      varMatrizDH2_NB[i,j] = 0
    }
  }
}

# NO
varMatrizNO2_NB <- cov(MatrizNO2_NB[,1:6])

for(i in 1:nrow(varMatrizNO2_NB)) {
  for(j in 1:nrow(varMatrizNO2_NB)) {
    if (i==j){
      varMatrizNO2_NB[i,j] = varMatrizNO2_NB[i,j]
    }
    else{
      varMatrizNO2_NB[i,j] = 0
    }
  }
}

# SL
varMatrizSL2_NB <- cov(MatrizSL2_NB[,1:6])

for(i in 1:nrow(varMatrizSL2_NB)) {
  for(j in 1:nrow(varMatrizSL2_NB)) {
    if (i==j){
      varMatrizSL2_NB[i,j] = varMatrizSL2_NB[i,j]
    }
    else{
      varMatrizSL2_NB[i,j] = 0
    }
  }
}

# Matriz de Variancias - Identidade (Data Set 2)
varMatrizDH2_NB
varMatrizNO2_NB
varMatrizSL2_NB

# Normalizacao do Teste com base na media e desvios-padrao do Treino
TesteNormalizado2 <- Teste2[,1:7]

for(i in 1:nrow(TesteNormalizado2)) {
  TesteNormalizado2[i,1] <- (TesteNormalizado2[i,1] - MD21) / DP21
  TesteNormalizado2[i,2] <- (TesteNormalizado2[i,2] - MD22) / DP22
  TesteNormalizado2[i,3] <- (TesteNormalizado2[i,3] - MD23) / DP23
  TesteNormalizado2[i,4] <- (TesteNormalizado2[i,4] - MD24) / DP24
  TesteNormalizado2[i,5] <- (TesteNormalizado2[i,5] - MD25) / DP25
  TesteNormalizado2[i,6] <- (TesteNormalizado2[i,6] - MD26) / DP26
}

# Data Set 3
############
# Treinamento Normalizado
TreiNormalizado3

#Subset 3 (Dividindo por classe)
MatrizDH3_NB <- subset(TreiNormalizado3, TreiNormalizado3[,7] == 1)
MatrizNO3_NB <- subset(TreiNormalizado3, TreiNormalizado3[,7] == 2)
MatrizSL3_NB <- subset(TreiNormalizado3, TreiNormalizado3[,7] == 3)

######################################
# Centroides - DataSet 3 (Normalizado)
MD_DH3_NB = matrix(c(mean(MatrizDH3_NB[,1]), mean(MatrizDH3_NB[,2]), mean(MatrizDH3_NB[,3]), mean(MatrizDH3_NB[,4]), mean(MatrizDH3_NB[,5]), mean(MatrizDH3_NB[,6]), 1),1,7,1)
MD_NO3_NB = matrix(c(mean(MatrizNO3_NB[,1]), mean(MatrizNO3_NB[,2]), mean(MatrizNO3_NB[,3]), mean(MatrizNO3_NB[,4]), mean(MatrizNO3_NB[,5]), mean(MatrizNO3_NB[,6]), 2),1,7,1)
MD_SL3_NB = matrix(c(mean(MatrizSL3_NB[,1]), mean(MatrizSL3_NB[,2]), mean(MatrizSL3_NB[,3]), mean(MatrizSL3_NB[,4]), mean(MatrizSL3_NB[,5]), mean(MatrizSL3_NB[,6]), 3),1,7,1)

#################################
# Matriz de Variancias - Identidade (Data Set 3)
# DH
varMatrizDH3_NB <- cov(MatrizDH3_NB[,1:6])

for(i in 1:nrow(varMatrizDH3_NB)) {
  for(j in 1:nrow(varMatrizDH3_NB)) {
    if (i==j){
      varMatrizDH3_NB[i,j] = varMatrizDH3_NB[i,j]
    }
    else{
      varMatrizDH3_NB[i,j] = 0
    }
  }
}

# NO
varMatrizNO3_NB <- cov(MatrizNO3_NB[,1:6])

for(i in 1:nrow(varMatrizNO3_NB)) {
  for(j in 1:nrow(varMatrizNO3_NB)) {
    if (i==j){
      varMatrizNO3_NB[i,j] = varMatrizNO3_NB[i,j]
    }
    else{
      varMatrizNO3_NB[i,j] = 0
    }
  }
}

# SL
varMatrizSL3_NB <- cov(MatrizSL3_NB[,1:6])

for(i in 1:nrow(varMatrizSL3_NB)) {
  for(j in 1:nrow(varMatrizSL3_NB)) {
    if (i==j){
      varMatrizSL3_NB[i,j] = varMatrizSL3_NB[i,j]
    }
    else{
      varMatrizSL3_NB[i,j] = 0
    }
  }
}

# Matriz de Variancias - Identidade (Data Set 3)
varMatrizDH3_NB
varMatrizNO3_NB
varMatrizSL3_NB

# Normalizacao do Teste com base na media e desvios-padrao do Treino
TesteNormalizado3 <- Teste3[,1:7]

for(i in 1:nrow(TesteNormalizado3)) {
  TesteNormalizado3[i,1] <- (TesteNormalizado3[i,1] - MD31) / DP31
  TesteNormalizado3[i,2] <- (TesteNormalizado3[i,2] - MD32) / DP32
  TesteNormalizado3[i,3] <- (TesteNormalizado3[i,3] - MD33) / DP33
  TesteNormalizado3[i,4] <- (TesteNormalizado3[i,4] - MD34) / DP34
  TesteNormalizado3[i,5] <- (TesteNormalizado3[i,5] - MD35) / DP35
  TesteNormalizado3[i,6] <- (TesteNormalizado3[i,6] - MD36) / DP36
}

# Data Set 4
############
# Treinamento Normalizado
TreiNormalizado4

#Subset 4 (Dividindo por classe)
MatrizDH4_NB <- subset(TreiNormalizado4, TreiNormalizado4[,7] == 1)
MatrizNO4_NB <- subset(TreiNormalizado4, TreiNormalizado4[,7] == 2)
MatrizSL4_NB <- subset(TreiNormalizado4, TreiNormalizado4[,7] == 3)

######################################
# Centroides - DataSet 4 (Normalizado)
MD_DH4_NB = matrix(c(mean(MatrizDH4_NB[,1]), mean(MatrizDH4_NB[,2]), mean(MatrizDH4_NB[,3]), mean(MatrizDH4_NB[,4]), mean(MatrizDH4_NB[,5]), mean(MatrizDH4_NB[,6]), 1),1,7,1)
MD_NO4_NB = matrix(c(mean(MatrizNO4_NB[,1]), mean(MatrizNO4_NB[,2]), mean(MatrizNO4_NB[,3]), mean(MatrizNO4_NB[,4]), mean(MatrizNO4_NB[,5]), mean(MatrizNO4_NB[,6]), 2),1,7,1)
MD_SL4_NB = matrix(c(mean(MatrizSL4_NB[,1]), mean(MatrizSL4_NB[,2]), mean(MatrizSL4_NB[,3]), mean(MatrizSL4_NB[,4]), mean(MatrizSL4_NB[,5]), mean(MatrizSL4_NB[,6]), 3),1,7,1)

#################################
# Matriz de Variancias - Identidade (Data Set 4)
# DH
varMatrizDH4_NB <- cov(MatrizDH4_NB[,1:6])

for(i in 1:nrow(varMatrizDH4_NB)) {
  for(j in 1:nrow(varMatrizDH4_NB)) {
    if (i==j){
      varMatrizDH4_NB[i,j] = varMatrizDH4_NB[i,j]
    }
    else{
      varMatrizDH4_NB[i,j] = 0
    }
  }
}

# NO
varMatrizNO4_NB <- cov(MatrizNO4_NB[,1:6])

for(i in 1:nrow(varMatrizNO4_NB)) {
  for(j in 1:nrow(varMatrizNO4_NB)) {
    if (i==j){
      varMatrizNO4_NB[i,j] = varMatrizNO4_NB[i,j]
    }
    else{
      varMatrizNO4_NB[i,j] = 0
    }
  }
}

# SL
varMatrizSL4_NB <- cov(MatrizSL4_NB[,1:6])

for(i in 1:nrow(varMatrizSL4_NB)) {
  for(j in 1:nrow(varMatrizSL4_NB)) {
    if (i==j){
      varMatrizSL4_NB[i,j] = varMatrizSL4_NB[i,j]
    }
    else{
      varMatrizSL4_NB[i,j] = 0
    }
  }
}

# Matriz de Variancias - Identidade (Data Set 4)
varMatrizDH4_NB
varMatrizNO4_NB
varMatrizSL4_NB

# Normalizacao do Teste com base na media e desvios-padrao do Treino
TesteNormalizado4 <- Teste4[,1:7]

for(i in 1:nrow(TesteNormalizado4)) {
  TesteNormalizado4[i,1] <- (TesteNormalizado4[i,1] - MD41) / DP41
  TesteNormalizado4[i,2] <- (TesteNormalizado4[i,2] - MD42) / DP42
  TesteNormalizado4[i,3] <- (TesteNormalizado4[i,3] - MD43) / DP43
  TesteNormalizado4[i,4] <- (TesteNormalizado4[i,4] - MD44) / DP44
  TesteNormalizado4[i,5] <- (TesteNormalizado4[i,5] - MD45) / DP45
  TesteNormalizado4[i,6] <- (TesteNormalizado4[i,6] - MD46) / DP46
}

# Data Set 5
############
# Treinamento Normalizado
TreiNormalizado5

#Subset 5 (Dividindo por classe)
MatrizDH5_NB <- subset(TreiNormalizado5, TreiNormalizado5[,7] == 1)
MatrizNO5_NB <- subset(TreiNormalizado5, TreiNormalizado5[,7] == 2)
MatrizSL5_NB <- subset(TreiNormalizado5, TreiNormalizado5[,7] == 3)

######################################
# Centroides - DataSet 5 (Normalizado)
MD_DH5_NB = matrix(c(mean(MatrizDH5_NB[,1]), mean(MatrizDH5_NB[,2]), mean(MatrizDH5_NB[,3]), mean(MatrizDH5_NB[,4]), mean(MatrizDH5_NB[,5]), mean(MatrizDH5_NB[,6]), 1),1,7,1)
MD_NO5_NB = matrix(c(mean(MatrizNO5_NB[,1]), mean(MatrizNO5_NB[,2]), mean(MatrizNO5_NB[,3]), mean(MatrizNO5_NB[,4]), mean(MatrizNO5_NB[,5]), mean(MatrizNO5_NB[,6]), 2),1,7,1)
MD_SL5_NB = matrix(c(mean(MatrizSL5_NB[,1]), mean(MatrizSL5_NB[,2]), mean(MatrizSL5_NB[,3]), mean(MatrizSL5_NB[,4]), mean(MatrizSL5_NB[,5]), mean(MatrizSL5_NB[,6]), 3),1,7,1)

#################################
# Matriz de Variancias - Identidade (Data Set 5)
# DH
varMatrizDH5_NB <- cov(MatrizDH5_NB[,1:6])

for(i in 1:nrow(varMatrizDH5_NB)) {
  for(j in 1:nrow(varMatrizDH5_NB)) {
    if (i==j){
      varMatrizDH5_NB[i,j] = varMatrizDH5_NB[i,j]
    }
    else{
      varMatrizDH5_NB[i,j] = 0
    }
  }
}

# NO
varMatrizNO5_NB <- cov(MatrizNO5_NB[,1:6])

for(i in 1:nrow(varMatrizNO5_NB)) {
  for(j in 1:nrow(varMatrizNO5_NB)) {
    if (i==j){
      varMatrizNO5_NB[i,j] = varMatrizNO5_NB[i,j]
    }
    else{
      varMatrizNO5_NB[i,j] = 0
    }
  }
}

# SL
varMatrizSL5_NB <- cov(MatrizSL5_NB[,1:6])

for(i in 1:nrow(varMatrizSL5_NB)) {
  for(j in 1:nrow(varMatrizSL5_NB)) {
    if (i==j){
      varMatrizSL5_NB[i,j] = varMatrizSL5_NB[i,j]
    }
    else{
      varMatrizSL5_NB[i,j] = 0
    }
  }
}

# Matriz de Variancias - Identidade (Data Set 5)
varMatrizDH5_NB
varMatrizNO5_NB
varMatrizSL5_NB

# Normalizacao do Teste com base na media e desvios-padrao do Treino
TesteNormalizado5 <- Teste5[,1:7]

for(i in 1:nrow(TesteNormalizado5)) {
  TesteNormalizado5[i,1] <- (TesteNormalizado5[i,1] - MD51) / DP51
  TesteNormalizado5[i,2] <- (TesteNormalizado5[i,2] - MD52) / DP52
  TesteNormalizado5[i,3] <- (TesteNormalizado5[i,3] - MD53) / DP53
  TesteNormalizado5[i,4] <- (TesteNormalizado5[i,4] - MD54) / DP54
  TesteNormalizado5[i,5] <- (TesteNormalizado5[i,5] - MD55) / DP55
  TesteNormalizado5[i,6] <- (TesteNormalizado5[i,6] - MD56) / DP56
}

# Data Set 6
############
# Treinamento Normalizado
TreiNormalizado6

#Subset 6 (Dividindo por classe)
MatrizDH6_NB <- subset(TreiNormalizado6, TreiNormalizado6[,7] == 1)
MatrizNO6_NB <- subset(TreiNormalizado6, TreiNormalizado6[,7] == 2)
MatrizSL6_NB <- subset(TreiNormalizado6, TreiNormalizado6[,7] == 3)

######################################
# Centroides - DataSet 6 (Normalizado)
MD_DH6_NB = matrix(c(mean(MatrizDH6_NB[,1]), mean(MatrizDH6_NB[,2]), mean(MatrizDH6_NB[,3]), mean(MatrizDH6_NB[,4]), mean(MatrizDH6_NB[,5]), mean(MatrizDH6_NB[,6]), 1),1,7,1)
MD_NO6_NB = matrix(c(mean(MatrizNO6_NB[,1]), mean(MatrizNO6_NB[,2]), mean(MatrizNO6_NB[,3]), mean(MatrizNO6_NB[,4]), mean(MatrizNO6_NB[,5]), mean(MatrizNO6_NB[,6]), 2),1,7,1)
MD_SL6_NB = matrix(c(mean(MatrizSL6_NB[,1]), mean(MatrizSL6_NB[,2]), mean(MatrizSL6_NB[,3]), mean(MatrizSL6_NB[,4]), mean(MatrizSL6_NB[,5]), mean(MatrizSL6_NB[,6]), 3),1,7,1)

#################################
# Matriz de Variancias - Identidade (Data Set 6)
# DH
varMatrizDH6_NB <- cov(MatrizDH6_NB[,1:6])

for(i in 1:nrow(varMatrizDH6_NB)) {
  for(j in 1:nrow(varMatrizDH6_NB)) {
    if (i==j){
      varMatrizDH6_NB[i,j] = varMatrizDH6_NB[i,j]
    }
    else{
      varMatrizDH6_NB[i,j] = 0
    }
  }
}

# NO
varMatrizNO6_NB <- cov(MatrizNO6_NB[,1:6])

for(i in 1:nrow(varMatrizNO6_NB)) {
  for(j in 1:nrow(varMatrizNO6_NB)) {
    if (i==j){
      varMatrizNO6_NB[i,j] = varMatrizNO6_NB[i,j]
    }
    else{
      varMatrizNO6_NB[i,j] = 0
    }
  }
}

# SL
varMatrizSL6_NB <- cov(MatrizSL6_NB[,1:6])

for(i in 1:nrow(varMatrizSL6_NB)) {
  for(j in 1:nrow(varMatrizSL6_NB)) {
    if (i==j){
      varMatrizSL6_NB[i,j] = varMatrizSL6_NB[i,j]
    }
    else{
      varMatrizSL6_NB[i,j] = 0
    }
  }
}

# Matriz de Variancias - Identidade (Data Set 6)
varMatrizDH6_NB
varMatrizNO6_NB
varMatrizSL6_NB

# Normalizacao do Teste com base na media e desvios-padrao do Treino
TesteNormalizado6 <- Teste6[,1:7]

for(i in 1:nrow(TesteNormalizado6)) {
  TesteNormalizado6[i,1] <- (TesteNormalizado6[i,1] - MD61) / DP61
  TesteNormalizado6[i,2] <- (TesteNormalizado6[i,2] - MD62) / DP62
  TesteNormalizado6[i,3] <- (TesteNormalizado6[i,3] - MD63) / DP63
  TesteNormalizado6[i,4] <- (TesteNormalizado6[i,4] - MD64) / DP64
  TesteNormalizado6[i,5] <- (TesteNormalizado6[i,5] - MD65) / DP65
  TesteNormalizado6[i,6] <- (TesteNormalizado6[i,6] - MD66) / DP66
}

# Data Set 7
############
# Treinamento Normalizado
TreiNormalizado7

#Subset 7 (Dividindo por classe)
MatrizDH7_NB <- subset(TreiNormalizado7, TreiNormalizado7[,7] == 1)
MatrizNO7_NB <- subset(TreiNormalizado7, TreiNormalizado7[,7] == 2)
MatrizSL7_NB <- subset(TreiNormalizado7, TreiNormalizado7[,7] == 3)

######################################
# Centroides - DataSet 7 (Normalizado)
MD_DH7_NB = matrix(c(mean(MatrizDH7_NB[,1]), mean(MatrizDH7_NB[,2]), mean(MatrizDH7_NB[,3]), mean(MatrizDH7_NB[,4]), mean(MatrizDH7_NB[,5]), mean(MatrizDH7_NB[,6]), 1),1,7,1)
MD_NO7_NB = matrix(c(mean(MatrizNO7_NB[,1]), mean(MatrizNO7_NB[,2]), mean(MatrizNO7_NB[,3]), mean(MatrizNO7_NB[,4]), mean(MatrizNO7_NB[,5]), mean(MatrizNO7_NB[,6]), 2),1,7,1)
MD_SL7_NB = matrix(c(mean(MatrizSL7_NB[,1]), mean(MatrizSL7_NB[,2]), mean(MatrizSL7_NB[,3]), mean(MatrizSL7_NB[,4]), mean(MatrizSL7_NB[,5]), mean(MatrizSL7_NB[,6]), 3),1,7,1)

#################################
# Matriz de Variancias - Identidade (Data Set 7)
# DH
varMatrizDH7_NB <- cov(MatrizDH7_NB[,1:6])

for(i in 1:nrow(varMatrizDH7_NB)) {
  for(j in 1:nrow(varMatrizDH7_NB)) {
    if (i==j){
      varMatrizDH7_NB[i,j] = varMatrizDH7_NB[i,j]
    }
    else{
      varMatrizDH7_NB[i,j] = 0
    }
  }
}

# NO
varMatrizNO7_NB <- cov(MatrizNO7_NB[,1:6])

for(i in 1:nrow(varMatrizNO7_NB)) {
  for(j in 1:nrow(varMatrizNO7_NB)) {
    if (i==j){
      varMatrizNO7_NB[i,j] = varMatrizNO7_NB[i,j]
    }
    else{
      varMatrizNO7_NB[i,j] = 0
    }
  }
}

# SL
varMatrizSL7_NB <- cov(MatrizSL7_NB[,1:6])

for(i in 1:nrow(varMatrizSL7_NB)) {
  for(j in 1:nrow(varMatrizSL7_NB)) {
    if (i==j){
      varMatrizSL7_NB[i,j] = varMatrizSL7_NB[i,j]
    }
    else{
      varMatrizSL7_NB[i,j] = 0
    }
  }
}

# Matriz de Variancias - Identidade (Data Set 7)
varMatrizDH7_NB
varMatrizNO7_NB
varMatrizSL7_NB

# Normalizacao do Teste com base na media e desvios-padrao do Treino
TesteNormalizado7 <- Teste7[,1:7]

for(i in 1:nrow(TesteNormalizado7)) {
  TesteNormalizado7[i,1] <- (TesteNormalizado7[i,1] - MD71) / DP71
  TesteNormalizado7[i,2] <- (TesteNormalizado7[i,2] - MD72) / DP72
  TesteNormalizado7[i,3] <- (TesteNormalizado7[i,3] - MD73) / DP73
  TesteNormalizado7[i,4] <- (TesteNormalizado7[i,4] - MD74) / DP74
  TesteNormalizado7[i,5] <- (TesteNormalizado7[i,5] - MD75) / DP75
  TesteNormalizado7[i,6] <- (TesteNormalizado7[i,6] - MD76) / DP76
}

# Data Set 8
############
# Treinamento Normalizado
TreiNormalizado8

#Subset 8 (Dividindo por classe)
MatrizDH8_NB <- subset(TreiNormalizado8, TreiNormalizado8[,7] == 1)
MatrizNO8_NB <- subset(TreiNormalizado8, TreiNormalizado8[,7] == 2)
MatrizSL8_NB <- subset(TreiNormalizado8, TreiNormalizado8[,7] == 3)

######################################
# Centroides - DataSet 8 (Normalizado)
MD_DH8_NB = matrix(c(mean(MatrizDH8_NB[,1]), mean(MatrizDH8_NB[,2]), mean(MatrizDH8_NB[,3]), mean(MatrizDH8_NB[,4]), mean(MatrizDH8_NB[,5]), mean(MatrizDH8_NB[,6]), 1),1,7,1)
MD_NO8_NB = matrix(c(mean(MatrizNO8_NB[,1]), mean(MatrizNO8_NB[,2]), mean(MatrizNO8_NB[,3]), mean(MatrizNO8_NB[,4]), mean(MatrizNO8_NB[,5]), mean(MatrizNO8_NB[,6]), 2),1,7,1)
MD_SL8_NB = matrix(c(mean(MatrizSL8_NB[,1]), mean(MatrizSL8_NB[,2]), mean(MatrizSL8_NB[,3]), mean(MatrizSL8_NB[,4]), mean(MatrizSL8_NB[,5]), mean(MatrizSL8_NB[,6]), 3),1,7,1)

#################################
# Matriz de Variancias - Identidade (Data Set 8)
# DH
varMatrizDH8_NB <- cov(MatrizDH8_NB[,1:6])

for(i in 1:nrow(varMatrizDH8_NB)) {
  for(j in 1:nrow(varMatrizDH8_NB)) {
    if (i==j){
      varMatrizDH8_NB[i,j] = varMatrizDH8_NB[i,j]
    }
    else{
      varMatrizDH8_NB[i,j] = 0
    }
  }
}

# NO
varMatrizNO8_NB <- cov(MatrizNO8_NB[,1:6])

for(i in 1:nrow(varMatrizNO8_NB)) {
  for(j in 1:nrow(varMatrizNO8_NB)) {
    if (i==j){
      varMatrizNO8_NB[i,j] = varMatrizNO8_NB[i,j]
    }
    else{
      varMatrizNO8_NB[i,j] = 0
    }
  }
}

# SL
varMatrizSL8_NB <- cov(MatrizSL8_NB[,1:6])

for(i in 1:nrow(varMatrizSL8_NB)) {
  for(j in 1:nrow(varMatrizSL8_NB)) {
    if (i==j){
      varMatrizSL8_NB[i,j] = varMatrizSL8_NB[i,j]
    }
    else{
      varMatrizSL8_NB[i,j] = 0
    }
  }
}

# Matriz de Variancias - Identidade (Data Set 8)
varMatrizDH8_NB
varMatrizNO8_NB
varMatrizSL8_NB

# Normalizacao do Teste com base na media e desvios-padrao do Treino
TesteNormalizado8 <- Teste8[,1:7]

for(i in 1:nrow(TesteNormalizado8)) {
  TesteNormalizado8[i,1] <- (TesteNormalizado8[i,1] - MD81) / DP81
  TesteNormalizado8[i,2] <- (TesteNormalizado8[i,2] - MD82) / DP82
  TesteNormalizado8[i,3] <- (TesteNormalizado8[i,3] - MD83) / DP83
  TesteNormalizado8[i,4] <- (TesteNormalizado8[i,4] - MD84) / DP84
  TesteNormalizado8[i,5] <- (TesteNormalizado8[i,5] - MD85) / DP85
  TesteNormalizado8[i,6] <- (TesteNormalizado8[i,6] - MD86) / DP86
}

# Data Set 9
############
# Treinamento Normalizado
TreiNormalizado9

#Subset 9 (Dividindo por classe)
MatrizDH9_NB <- subset(TreiNormalizado9, TreiNormalizado9[,7] == 1)
MatrizNO9_NB <- subset(TreiNormalizado9, TreiNormalizado9[,7] == 2)
MatrizSL9_NB <- subset(TreiNormalizado9, TreiNormalizado9[,7] == 3)

######################################
# Centroides - DataSet 9 (Normalizado)
MD_DH9_NB = matrix(c(mean(MatrizDH9_NB[,1]), mean(MatrizDH9_NB[,2]), mean(MatrizDH9_NB[,3]), mean(MatrizDH9_NB[,4]), mean(MatrizDH9_NB[,5]), mean(MatrizDH9_NB[,6]), 1),1,7,1)
MD_NO9_NB = matrix(c(mean(MatrizNO9_NB[,1]), mean(MatrizNO9_NB[,2]), mean(MatrizNO9_NB[,3]), mean(MatrizNO9_NB[,4]), mean(MatrizNO9_NB[,5]), mean(MatrizNO9_NB[,6]), 2),1,7,1)
MD_SL9_NB = matrix(c(mean(MatrizSL9_NB[,1]), mean(MatrizSL9_NB[,2]), mean(MatrizSL9_NB[,3]), mean(MatrizSL9_NB[,4]), mean(MatrizSL9_NB[,5]), mean(MatrizSL9_NB[,6]), 3),1,7,1)

#################################
# Matriz de Variancias - Identidade (Data Set 9)
# DH
varMatrizDH9_NB <- cov(MatrizDH9_NB[,1:6])

for(i in 1:nrow(varMatrizDH9_NB)) {
  for(j in 1:nrow(varMatrizDH9_NB)) {
    if (i==j){
      varMatrizDH9_NB[i,j] = varMatrizDH9_NB[i,j]
    }
    else{
      varMatrizDH9_NB[i,j] = 0
    }
  }
}

# NO
varMatrizNO9_NB <- cov(MatrizNO9_NB[,1:6])

for(i in 1:nrow(varMatrizNO9_NB)) {
  for(j in 1:nrow(varMatrizNO9_NB)) {
    if (i==j){
      varMatrizNO9_NB[i,j] = varMatrizNO9_NB[i,j]
    }
    else{
      varMatrizNO9_NB[i,j] = 0
    }
  }
}

# SL
varMatrizSL9_NB <- cov(MatrizSL9_NB[,1:6])

for(i in 1:nrow(varMatrizSL9_NB)) {
  for(j in 1:nrow(varMatrizSL9_NB)) {
    if (i==j){
      varMatrizSL9_NB[i,j] = varMatrizSL9_NB[i,j]
    }
    else{
      varMatrizSL9_NB[i,j] = 0
    }
  }
}

# Matriz de Variancias - Identidade (Data Set 9)
varMatrizDH9_NB
varMatrizNO9_NB
varMatrizSL9_NB

# Normalizacao do Teste com base na media e desvios-padrao do Treino
TesteNormalizado9 <- Teste9[,1:7]

for(i in 1:nrow(TesteNormalizado9)) {
  TesteNormalizado9[i,1] <- (TesteNormalizado9[i,1] - MD91) / DP91
  TesteNormalizado9[i,2] <- (TesteNormalizado9[i,2] - MD92) / DP92
  TesteNormalizado9[i,3] <- (TesteNormalizado9[i,3] - MD93) / DP93
  TesteNormalizado9[i,4] <- (TesteNormalizado9[i,4] - MD94) / DP94
  TesteNormalizado9[i,5] <- (TesteNormalizado9[i,5] - MD95) / DP95
  TesteNormalizado9[i,6] <- (TesteNormalizado9[i,6] - MD96) / DP96
}

# Data Set 10
############
# Treinamento Normalizado
TreiNormalizado10

#Subset 10 (Dividindo por classe)
MatrizDH10_NB <- subset(TreiNormalizado10, TreiNormalizado10[,7] == 1)
MatrizNO10_NB <- subset(TreiNormalizado10, TreiNormalizado10[,7] == 2)
MatrizSL10_NB <- subset(TreiNormalizado10, TreiNormalizado10[,7] == 3)

######################################
# Centroides - DataSet 10 (Normalizado)
MD_DH10_NB = matrix(c(mean(MatrizDH10_NB[,1]), mean(MatrizDH10_NB[,2]), mean(MatrizDH10_NB[,3]), mean(MatrizDH10_NB[,4]), mean(MatrizDH10_NB[,5]), mean(MatrizDH10_NB[,6]), 1),1,7,1)
MD_NO10_NB = matrix(c(mean(MatrizNO10_NB[,1]), mean(MatrizNO10_NB[,2]), mean(MatrizNO10_NB[,3]), mean(MatrizNO10_NB[,4]), mean(MatrizNO10_NB[,5]), mean(MatrizNO10_NB[,6]), 2),1,7,1)
MD_SL10_NB = matrix(c(mean(MatrizSL10_NB[,1]), mean(MatrizSL10_NB[,2]), mean(MatrizSL10_NB[,3]), mean(MatrizSL10_NB[,4]), mean(MatrizSL10_NB[,5]), mean(MatrizSL10_NB[,6]), 3),1,7,1)


#################################
# Matriz de Variancias - Identidade (Data Set 10)
# DH
varMatrizDH10_NB <- cov(MatrizDH10_NB[,1:6])

for(i in 1:nrow(varMatrizDH10_NB)) {
  for(j in 1:nrow(varMatrizDH10_NB)) {
    if (i==j){
      varMatrizDH10_NB[i,j] = varMatrizDH10_NB[i,j]
    }
    else{
      varMatrizDH10_NB[i,j] = 0
    }
  }
}

# NO
varMatrizNO10_NB <- cov(MatrizNO10_NB[,1:6])

for(i in 1:nrow(varMatrizNO10_NB)) {
  for(j in 1:nrow(varMatrizNO10_NB)) {
    if (i==j){
      varMatrizNO10_NB[i,j] = varMatrizNO10_NB[i,j]
    }
    else{
      varMatrizNO10_NB[i,j] = 0
    }
  }
}

# SL
varMatrizSL10_NB <- cov(MatrizSL10_NB[,1:6])

for(i in 1:nrow(varMatrizSL10_NB)) {
  for(j in 1:nrow(varMatrizSL10_NB)) {
    if (i==j){
      varMatrizSL10_NB[i,j] = varMatrizSL10_NB[i,j]
    }
    else{
      varMatrizSL10_NB[i,j] = 0
    }
  }
}

# Matriz de Variancias - Identidade (Data Set 10)
varMatrizDH10_NB
varMatrizNO10_NB
varMatrizSL10_NB

# Normalizacao do Teste com base na media e desvios-padrao do Treino
TesteNormalizado10 <- Teste10[,1:7]

for(i in 1:nrow(TesteNormalizado10)) {
  TesteNormalizado10[i,1] <- (TesteNormalizado10[i,1] - MD101) / DP101
  TesteNormalizado10[i,2] <- (TesteNormalizado10[i,2] - MD102) / DP102
  TesteNormalizado10[i,3] <- (TesteNormalizado10[i,3] - MD103) / DP103
  TesteNormalizado10[i,4] <- (TesteNormalizado10[i,4] - MD104) / DP104
  TesteNormalizado10[i,5] <- (TesteNormalizado10[i,5] - MD105) / DP105
  TesteNormalizado10[i,6] <- (TesteNormalizado10[i,6] - MD106) / DP106
}

# Testes Normalizados
TesteNormalizado1
TesteNormalizado2
TesteNormalizado3
TesteNormalizado4
TesteNormalizado5
TesteNormalizado6
TesteNormalizado7
TesteNormalizado8
TesteNormalizado9
TesteNormalizado10

################
# Naive Bayes #
###############
# Formula 2.8 (Pagina: 36)

# Usando a matriz de Teste - Data Set 1 (Teste Normalizado)
###########################################################

densDH_1 = log(nrow(MatrizDH1_NB) / nrow(TreiNormalizado1))
densNO_1 = log(nrow(MatrizNO1_NB) / nrow(TreiNormalizado1))
densSL_1 = log(nrow(MatrizSL1_NB) / nrow(TreiNormalizado1))

ResultadoNaiveBayes1 <- matrix(nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteNormalizado1)) {
  
 #tNB_DH <- densDH_1+(-0.5*log(abs(det(varMatrizDH1_NB))) - 0.5* mahalanobis(TesteNormalizado1[i,1:6], MD_DH1_NB[,1:6], varMatrizDH1_NB))
 #tNB_NO <- densNO_1+(-0.5*log(abs(det(varMatrizNO1_NB))) - 0.5* mahalanobis(TesteNormalizado1[i,1:6], MD_NO1_NB[,1:6], varMatrizNO1_NB))
 #tNB_SL <- densSL_1+(-0.5*log(abs(det(varMatrizSL1_NB))) - 0.5* mahalanobis(TesteNormalizado1[i,1:6], MD_SL1_NB[,1:6], varMatrizSL1_NB))
  
  TN1 = matrix(TesteNormalizado1[1,1:6],1,6,1)
  MD1 = matrix(MD_DH1_NB[,1:6],1,6,1)
  IN1 = matrix(varMatrizDH1_NB,6,6,1)
  
 #tNB_DH <- densDH_1+(-0.5*log(abs(det(varMatrizDH1_NB))) - 0.5* (t(TN1 - MD1)%*%solve(IN1)%*%(TN1 - MD1)))
  tNB_DH <- densDH_1+(-0.5*log(abs(det(varMatrizDH1_NB))) - 0.5* mahalanobis(TesteNormalizado1[i,1:6], MD_DH1_NB[,1:6], varMatrizDH1_NB))
  tNB_NO <- densNO_1+(-0.5*log(abs(det(varMatrizNO1_NB))) - 0.5* mahalanobis(TesteNormalizado1[i,1:6], MD_NO1_NB[,1:6], varMatrizNO1_NB))
  tNB_SL <- densSL_1+(-0.5*log(abs(det(varMatrizSL1_NB))) - 0.5* mahalanobis(TesteNormalizado1[i,1:6], MD_SL1_NB[,1:6], varMatrizSL1_NB))
  
  if (tNB_DH  > tNB_NO) {
    maior   <- tNB_DH
    classe  <- 1 # DH
  } else {
    maior   <- tNB_NO
    classe  <- 2 } # NO  
  
  if (maior   > tNB_SL) {
    maior   <- maior
    classe  <- classe
  } else {
    maior   <- tNB_SL
    classe  <- 3 } # SL
  
  ResultadoNaiveBayes1[i,1] <- maior
  ResultadoNaiveBayes1[i,2] <- classe
}

###################
# Matriz de Confusao

MatConfusaoNB1 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado1)) {
  if (TesteNormalizado1[i,7] == 1 && ResultadoNaiveBayes1[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoNB1[1,1] <- aux11 }
  if (TesteNormalizado1[i,7] == 1 && ResultadoNaiveBayes1[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoNB1[1,2] <- aux12 }
  if (TesteNormalizado1[i,7] == 1 && ResultadoNaiveBayes1[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoNB1[1,3] <- aux13 }
  
  if (TesteNormalizado1[i,7] == 2 && ResultadoNaiveBayes1[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoNB1[2,1] <- aux21 }
  if (TesteNormalizado1[i,7] == 2 && ResultadoNaiveBayes1[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoNB1[2,2] <- aux22 }
  if (TesteNormalizado1[i,7] == 2 && ResultadoNaiveBayes1[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoNB1[2,3] <- aux23 }
  
  if (TesteNormalizado1[i,7] == 3 && ResultadoNaiveBayes1[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoNB1[3,1] <- aux31 }
  if (TesteNormalizado1[i,7] == 3 && ResultadoNaiveBayes1[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoNB1[3,2] <- aux32 }
  if (TesteNormalizado1[i,7] == 3 && ResultadoNaiveBayes1[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoNB1[3,3] <- aux33 }
}

# Usando a matriz de Teste - Data Set 2 (Teste Normalizado)
###########################################################

densDH_2 = log(nrow(MatrizDH2_NB) / nrow(TreiNormalizado2))
densNO_2 = log(nrow(MatrizNO2_NB) / nrow(TreiNormalizado2))
densSL_2 = log(nrow(MatrizSL2_NB) / nrow(TreiNormalizado2))

ResultadoNaiveBayes2 <- matrix(nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteNormalizado2)) {
  
  tNB_DH <- densDH_2+(-0.5*log(abs(det(varMatrizDH2_NB))) - 0.5* mahalanobis(TesteNormalizado2[i,1:6], MD_DH2_NB[,1:6], varMatrizDH2_NB))
  tNB_NO <- densNO_2+(-0.5*log(abs(det(varMatrizNO2_NB))) - 0.5* mahalanobis(TesteNormalizado2[i,1:6], MD_NO2_NB[,1:6], varMatrizNO2_NB))
  tNB_SL <- densSL_2+(-0.5*log(abs(det(varMatrizSL2_NB))) - 0.5* mahalanobis(TesteNormalizado2[i,1:6], MD_SL2_NB[,1:6], varMatrizSL2_NB))
  
  if (tNB_DH  > tNB_NO) {
    maior   <- tNB_DH
    classe  <- 1 # DH
  } else {
    maior   <- tNB_NO
    classe  <- 2 } # NO  
  
  if (maior   > tNB_SL) {
    maior   <- maior
    classe  <- classe
  } else {
    maior   <- tNB_SL
    classe  <- 3 } # SL
  
  ResultadoNaiveBayes2[i,1] <- maior
  ResultadoNaiveBayes2[i,2] <- classe
}

###################
# Matriz de Confusao

MatConfusaoNB2 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado2)) {
  if (TesteNormalizado2[i,7] == 1 && ResultadoNaiveBayes2[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoNB2[1,1] <- aux11 }
  if (TesteNormalizado2[i,7] == 1 && ResultadoNaiveBayes2[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoNB2[1,2] <- aux12 }
  if (TesteNormalizado2[i,7] == 1 && ResultadoNaiveBayes2[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoNB2[1,3] <- aux13 }
  
  if (TesteNormalizado2[i,7] == 2 && ResultadoNaiveBayes2[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoNB2[2,1] <- aux21 }
  if (TesteNormalizado2[i,7] == 2 && ResultadoNaiveBayes2[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoNB2[2,2] <- aux22 }
  if (TesteNormalizado2[i,7] == 2 && ResultadoNaiveBayes2[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoNB2[2,3] <- aux23 }
  
  if (TesteNormalizado2[i,7] == 3 && ResultadoNaiveBayes2[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoNB2[3,1] <- aux31 }
  if (TesteNormalizado2[i,7] == 3 && ResultadoNaiveBayes2[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoNB2[3,2] <- aux32 }
  if (TesteNormalizado2[i,7] == 3 && ResultadoNaiveBayes2[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoNB2[3,3] <- aux33 }
}

# Usando a matriz de Teste - Data Set 3 (Teste Normalizado)
###########################################################

densDH_3 = log(nrow(MatrizDH3_NB) / nrow(TreiNormalizado3))
densNO_3 = log(nrow(MatrizNO3_NB) / nrow(TreiNormalizado3))
densSL_3 = log(nrow(MatrizSL3_NB) / nrow(TreiNormalizado3))

ResultadoNaiveBayes3 <- matrix(nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteNormalizado3)) {
  
  tNB_DH <- densDH_3+(-0.5*log(abs(det(varMatrizDH3_NB))) - 0.5* mahalanobis(TesteNormalizado3[i,1:6], MD_DH3_NB[,1:6], varMatrizDH3_NB))
  tNB_NO <- densNO_3+(-0.5*log(abs(det(varMatrizNO3_NB))) - 0.5* mahalanobis(TesteNormalizado3[i,1:6], MD_NO3_NB[,1:6], varMatrizNO3_NB))
  tNB_SL <- densSL_3+(-0.5*log(abs(det(varMatrizSL3_NB))) - 0.5* mahalanobis(TesteNormalizado3[i,1:6], MD_SL3_NB[,1:6], varMatrizSL3_NB))
  
  if (tNB_DH  > tNB_NO) {
    maior   <- tNB_DH
    classe  <- 1 # DH
  } else {
    maior   <- tNB_NO
    classe  <- 2 } # NO  
  
  if (maior   > tNB_SL) {
    maior   <- maior
    classe  <- classe
  } else {
    maior   <- tNB_SL
    classe  <- 3 } # SL
  
  ResultadoNaiveBayes3[i,1] <- maior
  ResultadoNaiveBayes3[i,2] <- classe
}

###################
# Matriz de Confusao

MatConfusaoNB3 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado3)) {
  if (TesteNormalizado3[i,7] == 1 && ResultadoNaiveBayes3[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoNB3[1,1] <- aux11 }
  if (TesteNormalizado3[i,7] == 1 && ResultadoNaiveBayes3[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoNB3[1,2] <- aux12 }
  if (TesteNormalizado3[i,7] == 1 && ResultadoNaiveBayes3[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoNB3[1,3] <- aux13 }
  
  if (TesteNormalizado3[i,7] == 2 && ResultadoNaiveBayes3[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoNB3[2,1] <- aux21 }
  if (TesteNormalizado3[i,7] == 2 && ResultadoNaiveBayes3[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoNB3[2,2] <- aux22 }
  if (TesteNormalizado3[i,7] == 2 && ResultadoNaiveBayes3[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoNB3[2,3] <- aux23 }
  
  if (TesteNormalizado3[i,7] == 3 && ResultadoNaiveBayes3[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoNB3[3,1] <- aux31 }
  if (TesteNormalizado3[i,7] == 3 && ResultadoNaiveBayes3[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoNB3[3,2] <- aux32 }
  if (TesteNormalizado3[i,7] == 3 && ResultadoNaiveBayes3[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoNB3[3,3] <- aux33 }
}

# Usando a matriz de Teste - Data Set 4 (Teste Normalizado)
###########################################################

densDH_4 = log(nrow(MatrizDH4_NB) / nrow(TreiNormalizado4))
densNO_4 = log(nrow(MatrizNO4_NB) / nrow(TreiNormalizado4))
densSL_4 = log(nrow(MatrizSL4_NB) / nrow(TreiNormalizado4))

ResultadoNaiveBayes4 <- matrix(nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteNormalizado4)) {
  
  tNB_DH <- densDH_4+(-0.5*log(abs(det(varMatrizDH4_NB))) - 0.5* mahalanobis(TesteNormalizado4[i,1:6], MD_DH4_NB[,1:6], varMatrizDH4_NB))
  tNB_NO <- densNO_4+(-0.5*log(abs(det(varMatrizNO4_NB))) - 0.5* mahalanobis(TesteNormalizado4[i,1:6], MD_NO4_NB[,1:6], varMatrizNO4_NB))
  tNB_SL <- densSL_4+(-0.5*log(abs(det(varMatrizSL4_NB))) - 0.5* mahalanobis(TesteNormalizado4[i,1:6], MD_SL4_NB[,1:6], varMatrizSL4_NB))
  
  if (tNB_DH  > tNB_NO) {
    maior   <- tNB_DH
    classe  <- 1 # DH
  } else {
    maior   <- tNB_NO
    classe  <- 2 } # NO  
  
  if (maior   > tNB_SL) {
    maior   <- maior
    classe  <- classe
  } else {
    maior   <- tNB_SL
    classe  <- 3 } # SL
  
  ResultadoNaiveBayes4[i,1] <- maior
  ResultadoNaiveBayes4[i,2] <- classe
}

###################
# Matriz de Confusao

MatConfusaoNB4 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado4)) {
  if (TesteNormalizado4[i,7] == 1 && ResultadoNaiveBayes4[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoNB4[1,1] <- aux11 }
  if (TesteNormalizado4[i,7] == 1 && ResultadoNaiveBayes4[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoNB4[1,2] <- aux12 }
  if (TesteNormalizado4[i,7] == 1 && ResultadoNaiveBayes4[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoNB4[1,3] <- aux13 }
  
  if (TesteNormalizado4[i,7] == 2 && ResultadoNaiveBayes4[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoNB4[2,1] <- aux21 }
  if (TesteNormalizado4[i,7] == 2 && ResultadoNaiveBayes4[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoNB4[2,2] <- aux22 }
  if (TesteNormalizado4[i,7] == 2 && ResultadoNaiveBayes4[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoNB4[2,3] <- aux23 }
  
  if (TesteNormalizado4[i,7] == 3 && ResultadoNaiveBayes4[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoNB4[3,1] <- aux31 }
  if (TesteNormalizado4[i,7] == 3 && ResultadoNaiveBayes4[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoNB4[3,2] <- aux32 }
  if (TesteNormalizado4[i,7] == 3 && ResultadoNaiveBayes4[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoNB4[3,3] <- aux33 }
}

# Usando a matriz de Teste - Data Set 5 (Teste Normalizado)
###########################################################

densDH_5 = log(nrow(MatrizDH5_NB) / nrow(TreiNormalizado5))
densNO_5 = log(nrow(MatrizNO5_NB) / nrow(TreiNormalizado5))
densSL_5 = log(nrow(MatrizSL5_NB) / nrow(TreiNormalizado5))

ResultadoNaiveBayes5 <- matrix(nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteNormalizado5)) {
  
  tNB_DH <- densDH_5+(-0.5*log(abs(det(varMatrizDH5_NB))) - 0.5* mahalanobis(TesteNormalizado5[i,1:6], MD_DH5_NB[,1:6], varMatrizDH5_NB))
  tNB_NO <- densNO_5+(-0.5*log(abs(det(varMatrizNO5_NB))) - 0.5* mahalanobis(TesteNormalizado5[i,1:6], MD_NO5_NB[,1:6], varMatrizNO5_NB))
  tNB_SL <- densSL_5+(-0.5*log(abs(det(varMatrizSL5_NB))) - 0.5* mahalanobis(TesteNormalizado5[i,1:6], MD_SL5_NB[,1:6], varMatrizSL5_NB))
  
  if (tNB_DH  > tNB_NO) {
    maior   <- tNB_DH
    classe  <- 1 # DH
  } else {
    maior   <- tNB_NO
    classe  <- 2 } # NO  
  
  if (maior   > tNB_SL) {
    maior   <- maior
    classe  <- classe
  } else {
    maior   <- tNB_SL
    classe  <- 3 } # SL
  
  ResultadoNaiveBayes5[i,1] <- maior
  ResultadoNaiveBayes5[i,2] <- classe
}

###################
# Matriz de Confusao

MatConfusaoNB5 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado5)) {
  if (TesteNormalizado5[i,7] == 1 && ResultadoNaiveBayes5[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoNB5[1,1] <- aux11 }
  if (TesteNormalizado5[i,7] == 1 && ResultadoNaiveBayes5[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoNB5[1,2] <- aux12 }
  if (TesteNormalizado5[i,7] == 1 && ResultadoNaiveBayes5[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoNB5[1,3] <- aux13 }
  
  if (TesteNormalizado5[i,7] == 2 && ResultadoNaiveBayes5[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoNB5[2,1] <- aux21 }
  if (TesteNormalizado5[i,7] == 2 && ResultadoNaiveBayes5[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoNB5[2,2] <- aux22 }
  if (TesteNormalizado5[i,7] == 2 && ResultadoNaiveBayes5[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoNB5[2,3] <- aux23 }
  
  if (TesteNormalizado5[i,7] == 3 && ResultadoNaiveBayes5[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoNB5[3,1] <- aux31 }
  if (TesteNormalizado5[i,7] == 3 && ResultadoNaiveBayes5[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoNB5[3,2] <- aux32 }
  if (TesteNormalizado5[i,7] == 3 && ResultadoNaiveBayes5[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoNB5[3,3] <- aux33 }
}

# Usando a matriz de Teste - Data Set 6 (Teste Normalizado)
###########################################################

densDH_6 = log(nrow(MatrizDH6_NB) / nrow(TreiNormalizado6))
densNO_6 = log(nrow(MatrizNO6_NB) / nrow(TreiNormalizado6))
densSL_6 = log(nrow(MatrizSL6_NB) / nrow(TreiNormalizado6))

ResultadoNaiveBayes6 <- matrix(nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteNormalizado6)) {
  
  tNB_DH <- densDH_6+(-0.5*log(abs(det(varMatrizDH6_NB))) - 0.5* mahalanobis(TesteNormalizado6[i,1:6], MD_DH6_NB[,1:6], varMatrizDH6_NB))
  tNB_NO <- densNO_6+(-0.5*log(abs(det(varMatrizNO6_NB))) - 0.5* mahalanobis(TesteNormalizado6[i,1:6], MD_NO6_NB[,1:6], varMatrizNO6_NB))
  tNB_SL <- densSL_6+(-0.5*log(abs(det(varMatrizSL6_NB))) - 0.5* mahalanobis(TesteNormalizado6[i,1:6], MD_SL6_NB[,1:6], varMatrizSL6_NB))
  
  if (tNB_DH  > tNB_NO) {
    maior   <- tNB_DH
    classe  <- 1 # DH
  } else {
    maior   <- tNB_NO
    classe  <- 2 } # NO  
  
  if (maior   > tNB_SL) {
    maior   <- maior
    classe  <- classe
  } else {
    maior   <- tNB_SL
    classe  <- 3 } # SL
  
  ResultadoNaiveBayes6[i,1] <- maior
  ResultadoNaiveBayes6[i,2] <- classe
}

###################
# Matriz de Confusao

MatConfusaoNB6 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado6)) {
  if (TesteNormalizado6[i,7] == 1 && ResultadoNaiveBayes6[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoNB6[1,1] <- aux11 }
  if (TesteNormalizado6[i,7] == 1 && ResultadoNaiveBayes6[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoNB6[1,2] <- aux12 }
  if (TesteNormalizado6[i,7] == 1 && ResultadoNaiveBayes6[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoNB6[1,3] <- aux13 }
  
  if (TesteNormalizado6[i,7] == 2 && ResultadoNaiveBayes6[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoNB6[2,1] <- aux21 }
  if (TesteNormalizado6[i,7] == 2 && ResultadoNaiveBayes6[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoNB6[2,2] <- aux22 }
  if (TesteNormalizado6[i,7] == 2 && ResultadoNaiveBayes6[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoNB6[2,3] <- aux23 }
  
  if (TesteNormalizado6[i,7] == 3 && ResultadoNaiveBayes6[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoNB6[3,1] <- aux31 }
  if (TesteNormalizado6[i,7] == 3 && ResultadoNaiveBayes6[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoNB6[3,2] <- aux32 }
  if (TesteNormalizado6[i,7] == 3 && ResultadoNaiveBayes6[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoNB6[3,3] <- aux33 }
}

# Usando a matriz de Teste - Data Set 7 (Teste Normalizado)
###########################################################

densDH_7 = log(nrow(MatrizDH7_NB) / nrow(TreiNormalizado7))
densNO_7 = log(nrow(MatrizNO7_NB) / nrow(TreiNormalizado7))
densSL_7 = log(nrow(MatrizSL7_NB) / nrow(TreiNormalizado7))

ResultadoNaiveBayes7 <- matrix(nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteNormalizado7)) {
  
  tNB_DH <- densDH_7+(-0.5*log(abs(det(varMatrizDH7_NB))) - 0.5* mahalanobis(TesteNormalizado7[i,1:6], MD_DH7_NB[,1:6], varMatrizDH7_NB))
  tNB_NO <- densNO_7+(-0.5*log(abs(det(varMatrizNO7_NB))) - 0.5* mahalanobis(TesteNormalizado7[i,1:6], MD_NO7_NB[,1:6], varMatrizNO7_NB))
  tNB_SL <- densSL_7+(-0.5*log(abs(det(varMatrizSL7_NB))) - 0.5* mahalanobis(TesteNormalizado7[i,1:6], MD_SL7_NB[,1:6], varMatrizSL7_NB))
  
  if (tNB_DH  > tNB_NO) {
    maior   <- tNB_DH
    classe  <- 1 # DH
  } else {
    maior   <- tNB_NO
    classe  <- 2 } # NO  
  
  if (maior   > tNB_SL) {
    maior   <- maior
    classe  <- classe
  } else {
    maior   <- tNB_SL
    classe  <- 3 } # SL
  
  ResultadoNaiveBayes7[i,1] <- maior
  ResultadoNaiveBayes7[i,2] <- classe
}

###################
# Matriz de Confusao

MatConfusaoNB7 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado7)) {
  if (TesteNormalizado7[i,7] == 1 && ResultadoNaiveBayes7[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoNB7[1,1] <- aux11 }
  if (TesteNormalizado7[i,7] == 1 && ResultadoNaiveBayes7[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoNB7[1,2] <- aux12 }
  if (TesteNormalizado7[i,7] == 1 && ResultadoNaiveBayes7[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoNB7[1,3] <- aux13 }
  
  if (TesteNormalizado7[i,7] == 2 && ResultadoNaiveBayes7[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoNB7[2,1] <- aux21 }
  if (TesteNormalizado7[i,7] == 2 && ResultadoNaiveBayes7[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoNB7[2,2] <- aux22 }
  if (TesteNormalizado7[i,7] == 2 && ResultadoNaiveBayes7[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoNB7[2,3] <- aux23 }
  
  if (TesteNormalizado7[i,7] == 3 && ResultadoNaiveBayes7[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoNB7[3,1] <- aux31 }
  if (TesteNormalizado7[i,7] == 3 && ResultadoNaiveBayes7[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoNB7[3,2] <- aux32 }
  if (TesteNormalizado7[i,7] == 3 && ResultadoNaiveBayes7[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoNB7[3,3] <- aux33 }
}

# Usando a matriz de Teste - Data Set 8 (Teste Normalizado)
###########################################################

densDH_8 = log(nrow(MatrizDH8_NB) / nrow(TreiNormalizado8))
densNO_8 = log(nrow(MatrizNO8_NB) / nrow(TreiNormalizado8))
densSL_8 = log(nrow(MatrizSL8_NB) / nrow(TreiNormalizado8))

ResultadoNaiveBayes8 <- matrix(nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteNormalizado8)) {
  
  tNB_DH <- densDH_8+(-0.5*log(abs(det(varMatrizDH8_NB))) - 0.5* mahalanobis(TesteNormalizado8[i,1:6], MD_DH8_NB[,1:6], varMatrizDH8_NB))
  tNB_NO <- densNO_8+(-0.5*log(abs(det(varMatrizNO8_NB))) - 0.5* mahalanobis(TesteNormalizado8[i,1:6], MD_NO8_NB[,1:6], varMatrizNO8_NB))
  tNB_SL <- densSL_8+(-0.5*log(abs(det(varMatrizSL8_NB))) - 0.5* mahalanobis(TesteNormalizado8[i,1:6], MD_SL8_NB[,1:6], varMatrizSL8_NB))
  
  if (tNB_DH  > tNB_NO) {
    maior   <- tNB_DH
    classe  <- 1 # DH
  } else {
    maior   <- tNB_NO
    classe  <- 2 } # NO  
  
  if (maior   > tNB_SL) {
    maior   <- maior
    classe  <- classe
  } else {
    maior   <- tNB_SL
    classe  <- 3 } # SL
  
  ResultadoNaiveBayes8[i,1] <- maior
  ResultadoNaiveBayes8[i,2] <- classe
}

###################
# Matriz de Confusao

MatConfusaoNB8 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado8)) {
  if (TesteNormalizado8[i,7] == 1 && ResultadoNaiveBayes8[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoNB8[1,1] <- aux11 }
  if (TesteNormalizado8[i,7] == 1 && ResultadoNaiveBayes8[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoNB8[1,2] <- aux12 }
  if (TesteNormalizado8[i,7] == 1 && ResultadoNaiveBayes8[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoNB8[1,3] <- aux13 }
  
  if (TesteNormalizado8[i,7] == 2 && ResultadoNaiveBayes8[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoNB8[2,1] <- aux21 }
  if (TesteNormalizado8[i,7] == 2 && ResultadoNaiveBayes8[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoNB8[2,2] <- aux22 }
  if (TesteNormalizado8[i,7] == 2 && ResultadoNaiveBayes8[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoNB8[2,3] <- aux23 }
  
  if (TesteNormalizado8[i,7] == 3 && ResultadoNaiveBayes8[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoNB8[3,1] <- aux31 }
  if (TesteNormalizado8[i,7] == 3 && ResultadoNaiveBayes8[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoNB8[3,2] <- aux32 }
  if (TesteNormalizado8[i,7] == 3 && ResultadoNaiveBayes8[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoNB8[3,3] <- aux33 }
}

# Usando a matriz de Teste - Data Set 9 (Teste Normalizado)
###########################################################

densDH_9 = log(nrow(MatrizDH9_NB) / nrow(TreiNormalizado9))
densNO_9 = log(nrow(MatrizNO9_NB) / nrow(TreiNormalizado9))
densSL_9 = log(nrow(MatrizSL9_NB) / nrow(TreiNormalizado9))

ResultadoNaiveBayes9 <- matrix(nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteNormalizado9)) {
  
  tNB_DH <- densDH_9+(-0.5*log(abs(det(varMatrizDH9_NB))) - 0.5* mahalanobis(TesteNormalizado9[i,1:6], MD_DH9_NB[,1:6], varMatrizDH9_NB))
  tNB_NO <- densNO_9+(-0.5*log(abs(det(varMatrizNO9_NB))) - 0.5* mahalanobis(TesteNormalizado9[i,1:6], MD_NO9_NB[,1:6], varMatrizNO9_NB))
  tNB_SL <- densSL_9+(-0.5*log(abs(det(varMatrizSL9_NB))) - 0.5* mahalanobis(TesteNormalizado9[i,1:6], MD_SL9_NB[,1:6], varMatrizSL9_NB))
  
  if (tNB_DH  > tNB_NO) {
    maior   <- tNB_DH
    classe  <- 1 # DH
  } else {
    maior   <- tNB_NO
    classe  <- 2 } # NO  
  
  if (maior   > tNB_SL) {
    maior   <- maior
    classe  <- classe
  } else {
    maior   <- tNB_SL
    classe  <- 3 } # SL
  
  ResultadoNaiveBayes9[i,1] <- maior
  ResultadoNaiveBayes9[i,2] <- classe
}

###################
# Matriz de Confusao

MatConfusaoNB9 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado9)) {
  if (TesteNormalizado9[i,7] == 1 && ResultadoNaiveBayes9[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoNB9[1,1] <- aux11 }
  if (TesteNormalizado9[i,7] == 1 && ResultadoNaiveBayes9[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoNB9[1,2] <- aux12 }
  if (TesteNormalizado9[i,7] == 1 && ResultadoNaiveBayes9[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoNB9[1,3] <- aux13 }
  
  if (TesteNormalizado9[i,7] == 2 && ResultadoNaiveBayes9[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoNB9[2,1] <- aux21 }
  if (TesteNormalizado9[i,7] == 2 && ResultadoNaiveBayes9[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoNB9[2,2] <- aux22 }
  if (TesteNormalizado9[i,7] == 2 && ResultadoNaiveBayes9[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoNB9[2,3] <- aux23 }
  
  if (TesteNormalizado9[i,7] == 3 && ResultadoNaiveBayes9[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoNB9[3,1] <- aux31 }
  if (TesteNormalizado9[i,7] == 3 && ResultadoNaiveBayes9[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoNB9[3,2] <- aux32 }
  if (TesteNormalizado9[i,7] == 3 && ResultadoNaiveBayes9[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoNB9[3,3] <- aux33 }
}

# Usando a matriz de Teste - Data Set 10 (Teste Normalizado)
###########################################################

densDH_10 = log(nrow(MatrizDH10_NB) / nrow(TreiNormalizado10))
densNO_10 = log(nrow(MatrizNO10_NB) / nrow(TreiNormalizado10))
densSL_10 = log(nrow(MatrizSL10_NB) / nrow(TreiNormalizado10))

ResultadoNaiveBayes10 <- matrix(nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteNormalizado10)) {
  
  tNB_DH <- densDH_10+(-0.5*log(abs(det(varMatrizDH10_NB))) - 0.5* mahalanobis(TesteNormalizado10[i,1:6], MD_DH10_NB[,1:6], varMatrizDH10_NB))
  tNB_NO <- densNO_10+(-0.5*log(abs(det(varMatrizNO10_NB))) - 0.5* mahalanobis(TesteNormalizado10[i,1:6], MD_NO10_NB[,1:6], varMatrizNO10_NB))
  tNB_SL <- densSL_10+(-0.5*log(abs(det(varMatrizSL10_NB))) - 0.5* mahalanobis(TesteNormalizado10[i,1:6], MD_SL10_NB[,1:6], varMatrizSL10_NB))
  
  if (tNB_DH  > tNB_NO) {
    maior   <- tNB_DH
    classe  <- 1 # DH
  } else {
    maior   <- tNB_NO
    classe  <- 2 } # NO  
  
  if (maior   > tNB_SL) {
    maior   <- maior
    classe  <- classe
  } else {
    maior   <- tNB_SL
    classe  <- 3 } # SL
  
  ResultadoNaiveBayes10[i,1] <- maior
  ResultadoNaiveBayes10[i,2] <- classe
}

###################
# Matriz de Confusao

MatConfusaoNB10 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado10)) {
  if (TesteNormalizado10[i,7] == 1 && ResultadoNaiveBayes10[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoNB10[1,1] <- aux11 }
  if (TesteNormalizado10[i,7] == 1 && ResultadoNaiveBayes10[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoNB10[1,2] <- aux12 }
  if (TesteNormalizado10[i,7] == 1 && ResultadoNaiveBayes10[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoNB10[1,3] <- aux13 }
  
  if (TesteNormalizado10[i,7] == 2 && ResultadoNaiveBayes10[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoNB10[2,1] <- aux21 }
  if (TesteNormalizado10[i,7] == 2 && ResultadoNaiveBayes10[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoNB10[2,2] <- aux22 }
  if (TesteNormalizado10[i,7] == 2 && ResultadoNaiveBayes10[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoNB10[2,3] <- aux23 }
  
  if (TesteNormalizado10[i,7] == 3 && ResultadoNaiveBayes10[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoNB10[3,1] <- aux31 }
  if (TesteNormalizado10[i,7] == 3 && ResultadoNaiveBayes10[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoNB10[3,2] <- aux32 }
  if (TesteNormalizado10[i,7] == 3 && ResultadoNaiveBayes10[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoNB10[3,3] <- aux33 }
}

# Matriz de Confusao - Naive bayes
MatConfusaoNB1
MatConfusaoNB2
MatConfusaoNB3
MatConfusaoNB4
MatConfusaoNB5
MatConfusaoNB6
MatConfusaoNB7
MatConfusaoNB8
MatConfusaoNB9
MatConfusaoNB10