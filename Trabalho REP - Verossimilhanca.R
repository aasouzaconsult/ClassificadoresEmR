# MV - Máxima Verossimilhanca

# Usando a matriz de Teste - Data Set 1 (Teste Normalizado)
###########################################################

# Matriz de Covariancia - Completa
covMatrizDH1_NB <- cov(MatrizDH1_NB[,1:6])
covMatrizNO1_NB <- cov(MatrizNO1_NB[,1:6])
covMatrizSL1_NB <- cov(MatrizSL1_NB[,1:6])

densDH_1 = nrow(MatrizDH1_NB) / nrow(TreiNormalizado1)
densNO_1 = nrow(MatrizNO1_NB) / nrow(TreiNormalizado1)
densSL_1 = nrow(MatrizSL1_NB) / nrow(TreiNormalizado1)

ResultadoMV1 <- matrix(0,nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteNormalizado1)) {
  
  tNB_DH <- densDH_1*(-0.5*log(det(covMatrizDH1_NB)) - 0.5* mahalanobis(TesteNormalizado1[i,1:6], MD_DH1_NB[,1:6], covMatrizDH1_NB))
  tNB_NO <- densNO_1*(-0.5*log(det(covMatrizNO1_NB)) - 0.5* mahalanobis(TesteNormalizado1[i,1:6], MD_NO1_NB[,1:6], covMatrizNO1_NB))
  tNB_SL <- densSL_1*(-0.5*log(det(covMatrizSL1_NB)) - 0.5* mahalanobis(TesteNormalizado1[i,1:6], MD_SL1_NB[,1:6], covMatrizSL1_NB))
  
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
  
  ResultadoMV1[i,1] <- maior
  ResultadoMV1[i,2] <- classe
}

# Matriz de Confusao

MatConfusaoMV1 <- matrix(0,nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado1)) {
  if (TesteNormalizado1[i,7] == 1 && ResultadoMV1[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoMV1[1,1] <- aux11 }
  if (TesteNormalizado1[i,7] == 1 && ResultadoMV1[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoMV1[1,2] <- aux12 }
  if (TesteNormalizado1[i,7] == 1 && ResultadoMV1[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoMV1[1,3] <- aux13 }
  
  if (TesteNormalizado1[i,7] == 2 && ResultadoMV1[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoMV1[2,1] <- aux21 }
  if (TesteNormalizado1[i,7] == 2 && ResultadoMV1[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoMV1[2,2] <- aux22 }
  if (TesteNormalizado1[i,7] == 2 && ResultadoMV1[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoMV1[2,3] <- aux23 }
  
  if (TesteNormalizado1[i,7] == 3 && ResultadoMV1[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoMV1[3,1] <- aux31 }
  if (TesteNormalizado1[i,7] == 3 && ResultadoMV1[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoMV1[3,2] <- aux32 }
  if (TesteNormalizado1[i,7] == 3 && ResultadoMV1[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoMV1[3,3] <- aux33 }
}

# Usando a matriz de Teste - Data Set 2 (Teste Normalizado)
###########################################################

# Matriz de Covariancia - Completa
covMatrizDH2_NB <- cov(MatrizDH2_NB[,1:6])
covMatrizNO2_NB <- cov(MatrizNO2_NB[,1:6])
covMatrizSL2_NB <- cov(MatrizSL2_NB[,1:6])

densDH_2 = nrow(MatrizDH2_NB) / nrow(TreiNormalizado2)
densNO_2 = nrow(MatrizNO2_NB) / nrow(TreiNormalizado2)
densSL_2 = nrow(MatrizSL2_NB) / nrow(TreiNormalizado2)

ResultadoMV2 <- matrix(nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteNormalizado2)) {
  
  tNB_DH <- densDH_2*(-0.5*log(det(covMatrizDH2_NB)) - 0.5* mahalanobis(TesteNormalizado2[i,1:6], MD_DH2_NB[,1:6], covMatrizDH2_NB))
  tNB_NO <- densNO_2*(-0.5*log(det(covMatrizNO2_NB)) - 0.5* mahalanobis(TesteNormalizado2[i,1:6], MD_NO2_NB[,1:6], covMatrizNO2_NB))
  tNB_SL <- densSL_2*(-0.5*log(det(covMatrizSL2_NB)) - 0.5* mahalanobis(TesteNormalizado2[i,1:6], MD_SL2_NB[,1:6], covMatrizSL2_NB))
  
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
  
  ResultadoMV2[i,1] <- maior
  ResultadoMV2[i,2] <- classe
}

# Matriz de Confusao

MatConfusaoMV2 <- matrix(0,nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado2)) {
  if (TesteNormalizado2[i,7] == 1 && ResultadoMV2[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoMV2[1,1] <- aux11 }
  if (TesteNormalizado2[i,7] == 1 && ResultadoMV2[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoMV2[1,2] <- aux12 }
  if (TesteNormalizado2[i,7] == 1 && ResultadoMV2[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoMV2[1,3] <- aux13 }
  
  if (TesteNormalizado2[i,7] == 2 && ResultadoMV2[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoMV2[2,1] <- aux21 }
  if (TesteNormalizado2[i,7] == 2 && ResultadoMV2[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoMV2[2,2] <- aux22 }
  if (TesteNormalizado2[i,7] == 2 && ResultadoMV2[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoMV2[2,3] <- aux23 }
  
  if (TesteNormalizado2[i,7] == 3 && ResultadoMV2[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoMV2[3,1] <- aux31 }
  if (TesteNormalizado2[i,7] == 3 && ResultadoMV2[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoMV2[3,2] <- aux32 }
  if (TesteNormalizado2[i,7] == 3 && ResultadoMV2[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoMV2[3,3] <- aux33 }
}


# Usando a matriz de Teste - Data Set 3 (Teste Normalizado)
###########################################################

# Matriz de Covariancia - Completa
covMatrizDH3_NB <- cov(MatrizDH1_NB[,1:6])
covMatrizNO3_NB <- cov(MatrizNO1_NB[,1:6])
covMatrizSL3_NB <- cov(MatrizSL1_NB[,1:6])

densDH_3 = nrow(MatrizDH3_NB) / nrow(TreiNormalizado3)
densNO_3 = nrow(MatrizNO3_NB) / nrow(TreiNormalizado3)
densSL_3 = nrow(MatrizSL3_NB) / nrow(TreiNormalizado3)

ResultadoMV3 <- matrix(nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteNormalizado3)) {
  
  tNB_DH <- densDH_3*(-0.5*log(det(covMatrizDH3_NB)) - 0.5* mahalanobis(TesteNormalizado3[i,1:6], MD_DH3_NB[,1:6], covMatrizDH3_NB))
  tNB_NO <- densNO_3*(-0.5*log(det(covMatrizNO3_NB)) - 0.5* mahalanobis(TesteNormalizado3[i,1:6], MD_NO3_NB[,1:6], covMatrizNO3_NB))
  tNB_SL <- densSL_3*(-0.5*log(det(covMatrizSL3_NB)) - 0.5* mahalanobis(TesteNormalizado3[i,1:6], MD_SL3_NB[,1:6], covMatrizSL3_NB))
  
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
  
  ResultadoMV3[i,1] <- maior
  ResultadoMV3[i,2] <- classe
}

# Matriz de Confusao

MatConfusaoMV3 <- matrix(0,nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado3)) {
  if (TesteNormalizado3[i,7] == 1 && ResultadoMV3[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoMV3[1,1] <- aux11 }
  if (TesteNormalizado3[i,7] == 1 && ResultadoMV3[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoMV3[1,2] <- aux12 }
  if (TesteNormalizado3[i,7] == 1 && ResultadoMV3[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoMV3[1,3] <- aux13 }
  
  if (TesteNormalizado3[i,7] == 2 && ResultadoMV3[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoMV3[2,1] <- aux21 }
  if (TesteNormalizado3[i,7] == 2 && ResultadoMV3[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoMV3[2,2] <- aux22 }
  if (TesteNormalizado3[i,7] == 2 && ResultadoMV3[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoMV3[2,3] <- aux23 }
  
  if (TesteNormalizado3[i,7] == 3 && ResultadoMV3[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoMV3[3,1] <- aux31 }
  if (TesteNormalizado3[i,7] == 3 && ResultadoMV3[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoMV3[3,2] <- aux32 }
  if (TesteNormalizado3[i,7] == 3 && ResultadoMV3[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoMV3[3,3] <- aux33 }
}

# Usando a matriz de Teste - Data Set 4 (Teste Normalizado)
###########################################################

# Matriz de Covariancia - Completa
covMatrizDH4_NB <- cov(MatrizDH4_NB[,1:6])
covMatrizNO4_NB <- cov(MatrizNO4_NB[,1:6])
covMatrizSL4_NB <- cov(MatrizSL4_NB[,1:6])

densDH_4 = nrow(MatrizDH4_NB) / nrow(TreiNormalizado4)
densNO_4 = nrow(MatrizNO4_NB) / nrow(TreiNormalizado4)
densSL_4 = nrow(MatrizSL4_NB) / nrow(TreiNormalizado4)

ResultadoMV4 <- matrix(nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteNormalizado4)) {
  
  tNB_DH <- densDH_4*(-0.5*log(det(covMatrizDH4_NB)) - 0.5* mahalanobis(TesteNormalizado4[i,1:6], MD_DH4_NB[,1:6], covMatrizDH4_NB))
  tNB_NO <- densNO_4*(-0.5*log(det(covMatrizNO4_NB)) - 0.5* mahalanobis(TesteNormalizado4[i,1:6], MD_NO4_NB[,1:6], covMatrizNO4_NB))
  tNB_SL <- densSL_4*(-0.5*log(det(covMatrizSL4_NB)) - 0.5* mahalanobis(TesteNormalizado4[i,1:6], MD_SL4_NB[,1:6], covMatrizSL4_NB))
  
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
  
  ResultadoMV4[i,1] <- maior
  ResultadoMV4[i,2] <- classe
}

# Matriz de Confusao

MatConfusaoMV4 <- matrix(0,nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado4)) {
  if (TesteNormalizado4[i,7] == 1 && ResultadoMV4[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoMV4[1,1] <- aux11 }
  if (TesteNormalizado4[i,7] == 1 && ResultadoMV4[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoMV4[1,2] <- aux12 }
  if (TesteNormalizado4[i,7] == 1 && ResultadoMV4[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoMV4[1,3] <- aux13 }
  
  if (TesteNormalizado4[i,7] == 2 && ResultadoMV4[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoMV4[2,1] <- aux21 }
  if (TesteNormalizado4[i,7] == 2 && ResultadoMV4[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoMV4[2,2] <- aux22 }
  if (TesteNormalizado4[i,7] == 2 && ResultadoMV4[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoMV4[2,3] <- aux23 }
  
  if (TesteNormalizado4[i,7] == 3 && ResultadoMV4[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoMV4[3,1] <- aux31 }
  if (TesteNormalizado4[i,7] == 3 && ResultadoMV4[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoMV4[3,2] <- aux32 }
  if (TesteNormalizado4[i,7] == 3 && ResultadoMV4[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoMV4[3,3] <- aux33 }
}

# Usando a matriz de Teste - Data Set 5 (Teste Normalizado)
###########################################################

# Matriz de Covariancia - Completa
covMatrizDH5_NB <- cov(MatrizDH5_NB[,1:6])
covMatrizNO5_NB <- cov(MatrizNO5_NB[,1:6])
covMatrizSL5_NB <- cov(MatrizSL5_NB[,1:6])

densDH_5 = nrow(MatrizDH5_NB) / nrow(TreiNormalizado5)
densNO_5 = nrow(MatrizNO5_NB) / nrow(TreiNormalizado5)
densSL_5 = nrow(MatrizSL5_NB) / nrow(TreiNormalizado5)

ResultadoMV5 <- matrix(0,nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteNormalizado5)) {
  
  tNB_DH <- densDH_5*(-0.5*log(det(covMatrizDH5_NB)) - 0.5* mahalanobis(TesteNormalizado5[i,1:6], MD_DH5_NB[,1:6], covMatrizDH5_NB))
  tNB_NO <- densNO_5*(-0.5*log(det(covMatrizNO5_NB)) - 0.5* mahalanobis(TesteNormalizado5[i,1:6], MD_NO5_NB[,1:6], covMatrizNO5_NB))
  tNB_SL <- densSL_5*(-0.5*log(det(covMatrizSL5_NB)) - 0.5* mahalanobis(TesteNormalizado5[i,1:6], MD_SL5_NB[,1:6], covMatrizSL5_NB))
  
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
  
  ResultadoMV5[i,1] <- maior
  ResultadoMV5[i,2] <- classe
}

# Matriz de Confusao

MatConfusaoMV5 <- matrix(0,nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado5)) {
  if (TesteNormalizado5[i,7] == 1 && ResultadoMV5[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoMV5[1,1] <- aux11 }
  if (TesteNormalizado5[i,7] == 1 && ResultadoMV5[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoMV5[1,2] <- aux12 }
  if (TesteNormalizado5[i,7] == 1 && ResultadoMV5[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoMV5[1,3] <- aux13 }
  
  if (TesteNormalizado5[i,7] == 2 && ResultadoMV5[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoMV5[2,1] <- aux21 }
  if (TesteNormalizado5[i,7] == 2 && ResultadoMV5[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoMV5[2,2] <- aux22 }
  if (TesteNormalizado5[i,7] == 2 && ResultadoMV5[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoMV5[2,3] <- aux23 }
  
  if (TesteNormalizado5[i,7] == 3 && ResultadoMV5[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoMV5[3,1] <- aux31 }
  if (TesteNormalizado5[i,7] == 3 && ResultadoMV5[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoMV5[3,2] <- aux32 }
  if (TesteNormalizado5[i,7] == 3 && ResultadoMV5[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoMV5[3,3] <- aux33 }
}

# Usando a matriz de Teste - Data Set 6 (Teste Normalizado)
###########################################################

# Matriz de Covariancia - Completa
covMatrizDH6_NB <- cov(MatrizDH6_NB[,1:6])
covMatrizNO6_NB <- cov(MatrizNO6_NB[,1:6])
covMatrizSL6_NB <- cov(MatrizSL6_NB[,1:6])

densDH_6 = nrow(MatrizDH6_NB) / nrow(TreiNormalizado6)
densNO_6 = nrow(MatrizNO6_NB) / nrow(TreiNormalizado6)
densSL_6 = nrow(MatrizSL6_NB) / nrow(TreiNormalizado6)

ResultadoMV6 <- matrix(nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteNormalizado6)) {
  
  tNB_DH <- densDH_6*(-0.5*log(det(covMatrizDH6_NB)) - 0.5* mahalanobis(TesteNormalizado6[i,1:6], MD_DH6_NB[,1:6], covMatrizDH6_NB))
  tNB_NO <- densNO_6*(-0.5*log(det(covMatrizNO6_NB)) - 0.5* mahalanobis(TesteNormalizado6[i,1:6], MD_NO6_NB[,1:6], covMatrizNO6_NB))
  tNB_SL <- densSL_6*(-0.5*log(det(covMatrizSL6_NB)) - 0.5* mahalanobis(TesteNormalizado6[i,1:6], MD_SL6_NB[,1:6], covMatrizSL6_NB))
  
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
  
  ResultadoMV6[i,1] <- maior
  ResultadoMV6[i,2] <- classe
}

# Matriz de Confusao

MatConfusaoMV6 <- matrix(0,nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado6)) {
  if (TesteNormalizado6[i,7] == 1 && ResultadoMV6[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoMV6[1,1] <- aux11 }
  if (TesteNormalizado6[i,7] == 1 && ResultadoMV6[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoMV6[1,2] <- aux12 }
  if (TesteNormalizado6[i,7] == 1 && ResultadoMV6[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoMV6[1,3] <- aux13 }
  
  if (TesteNormalizado6[i,7] == 2 && ResultadoMV6[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoMV6[2,1] <- aux21 }
  if (TesteNormalizado6[i,7] == 2 && ResultadoMV6[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoMV6[2,2] <- aux22 }
  if (TesteNormalizado6[i,7] == 2 && ResultadoMV6[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoMV6[2,3] <- aux23 }
  
  if (TesteNormalizado6[i,7] == 3 && ResultadoMV6[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoMV6[3,1] <- aux31 }
  if (TesteNormalizado6[i,7] == 3 && ResultadoMV6[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoMV6[3,2] <- aux32 }
  if (TesteNormalizado6[i,7] == 3 && ResultadoMV6[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoMV6[3,3] <- aux33 }
}

# Usando a matriz de Teste - Data Set 7 (Teste Normalizado)
###########################################################

# Matriz de Covariancia - Completa
covMatrizDH7_NB <- cov(MatrizDH7_NB[,1:6])
covMatrizNO7_NB <- cov(MatrizNO7_NB[,1:6])
covMatrizSL7_NB <- cov(MatrizSL7_NB[,1:6])

densDH_7 = nrow(MatrizDH7_NB) / nrow(TreiNormalizado7)
densNO_7 = nrow(MatrizNO7_NB) / nrow(TreiNormalizado7)
densSL_7 = nrow(MatrizSL7_NB) / nrow(TreiNormalizado7)

ResultadoMV7 <- matrix(nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteNormalizado7)) {
  
  tNB_DH <- densDH_7*(-0.5*log(det(covMatrizDH7_NB)) - 0.5* mahalanobis(TesteNormalizado7[i,1:6], MD_DH7_NB[,1:6], covMatrizDH7_NB))
  tNB_NO <- densNO_7*(-0.5*log(det(covMatrizNO7_NB)) - 0.5* mahalanobis(TesteNormalizado7[i,1:6], MD_NO7_NB[,1:6], covMatrizNO7_NB))
  tNB_SL <- densSL_7*(-0.5*log(det(covMatrizSL7_NB)) - 0.5* mahalanobis(TesteNormalizado7[i,1:6], MD_SL7_NB[,1:6], covMatrizSL7_NB))
  
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
  
  ResultadoMV7[i,1] <- maior
  ResultadoMV7[i,2] <- classe
}

# Matriz de Confusao

MatConfusaoMV7 <- matrix(0,nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado7)) {
  if (TesteNormalizado7[i,7] == 1 && ResultadoMV7[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoMV7[1,1] <- aux11 }
  if (TesteNormalizado7[i,7] == 1 && ResultadoMV7[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoMV7[1,2] <- aux12 }
  if (TesteNormalizado7[i,7] == 1 && ResultadoMV7[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoMV7[1,3] <- aux13 }
  
  if (TesteNormalizado7[i,7] == 2 && ResultadoMV7[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoMV7[2,1] <- aux21 }
  if (TesteNormalizado7[i,7] == 2 && ResultadoMV7[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoMV7[2,2] <- aux22 }
  if (TesteNormalizado7[i,7] == 2 && ResultadoMV7[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoMV7[2,3] <- aux23 }
  
  if (TesteNormalizado7[i,7] == 3 && ResultadoMV7[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoMV7[3,1] <- aux31 }
  if (TesteNormalizado7[i,7] == 3 && ResultadoMV7[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoMV7[3,2] <- aux32 }
  if (TesteNormalizado7[i,7] == 3 && ResultadoMV7[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoMV7[3,3] <- aux33 }
}

# Usando a matriz de Teste - Data Set 8 (Teste Normalizado)
###########################################################

# Matriz de Covariancia - Completa
covMatrizDH8_NB <- cov(MatrizDH8_NB[,1:6])
covMatrizNO8_NB <- cov(MatrizNO8_NB[,1:6])
covMatrizSL8_NB <- cov(MatrizSL8_NB[,1:6])

densDH_8 = nrow(MatrizDH8_NB) / nrow(TreiNormalizado8)
densNO_8 = nrow(MatrizNO8_NB) / nrow(TreiNormalizado8)
densSL_8 = nrow(MatrizSL8_NB) / nrow(TreiNormalizado8)

ResultadoMV8 <- matrix(nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteNormalizado8)) {
  
  tNB_DH <- densDH_8*(-0.5*log(det(covMatrizDH8_NB)) - 0.5* mahalanobis(TesteNormalizado8[i,1:6], MD_DH8_NB[,1:6], covMatrizDH8_NB))
  tNB_NO <- densNO_8*(-0.5*log(det(covMatrizNO8_NB)) - 0.5* mahalanobis(TesteNormalizado8[i,1:6], MD_NO8_NB[,1:6], covMatrizNO8_NB))
  tNB_SL <- densSL_8*(-0.5*log(det(covMatrizSL8_NB)) - 0.5* mahalanobis(TesteNormalizado8[i,1:6], MD_SL8_NB[,1:6], covMatrizSL8_NB))
  
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
  
  ResultadoMV8[i,1] <- maior
  ResultadoMV8[i,2] <- classe
}

# Matriz de Confusao

MatConfusaoMV8 <- matrix(0,nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado8)) {
  if (TesteNormalizado8[i,7] == 1 && ResultadoMV8[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoMV8[1,1] <- aux11 }
  if (TesteNormalizado8[i,7] == 1 && ResultadoMV8[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoMV8[1,2] <- aux12 }
  if (TesteNormalizado8[i,7] == 1 && ResultadoMV8[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoMV8[1,3] <- aux13 }
  
  if (TesteNormalizado8[i,7] == 2 && ResultadoMV8[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoMV8[2,1] <- aux21 }
  if (TesteNormalizado8[i,7] == 2 && ResultadoMV8[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoMV8[2,2] <- aux22 }
  if (TesteNormalizado8[i,7] == 2 && ResultadoMV8[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoMV8[2,3] <- aux23 }
  
  if (TesteNormalizado8[i,7] == 3 && ResultadoMV8[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoMV8[3,1] <- aux31 }
  if (TesteNormalizado8[i,7] == 3 && ResultadoMV8[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoMV8[3,2] <- aux32 }
  if (TesteNormalizado8[i,7] == 3 && ResultadoMV8[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoMV8[3,3] <- aux33 }
}

# Usando a matriz de Teste - Data Set 9 (Teste Normalizado)
###########################################################

# Matriz de Covariancia - Completa
covMatrizDH9_NB <- cov(MatrizDH9_NB[,1:6])
covMatrizNO9_NB <- cov(MatrizNO9_NB[,1:6])
covMatrizSL9_NB <- cov(MatrizSL9_NB[,1:6])

densDH_9 = nrow(MatrizDH9_NB) / nrow(TreiNormalizado9)
densNO_9 = nrow(MatrizNO9_NB) / nrow(TreiNormalizado9)
densSL_9 = nrow(MatrizSL9_NB) / nrow(TreiNormalizado9)

ResultadoMV9 <- matrix(nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteNormalizado9)) {
  
  tNB_DH <- densDH_9*(-0.5*log(det(covMatrizDH9_NB)) - 0.5* mahalanobis(TesteNormalizado9[i,1:6], MD_DH9_NB[,1:6], covMatrizDH9_NB))
  tNB_NO <- densNO_9*(-0.5*log(det(covMatrizNO9_NB)) - 0.5* mahalanobis(TesteNormalizado9[i,1:6], MD_NO9_NB[,1:6], covMatrizNO9_NB))
  tNB_SL <- densSL_9*(-0.5*log(det(covMatrizSL9_NB)) - 0.5* mahalanobis(TesteNormalizado9[i,1:6], MD_SL9_NB[,1:6], covMatrizSL9_NB))
  
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
  
  ResultadoMV9[i,1] <- maior
  ResultadoMV9[i,2] <- classe
}

# Matriz de Confusao

MatConfusaoMV9 <- matrix(0,nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado9)) {
  if (TesteNormalizado9[i,7] == 1 && ResultadoMV9[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoMV9[1,1] <- aux11 }
  if (TesteNormalizado9[i,7] == 1 && ResultadoMV9[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoMV9[1,2] <- aux12 }
  if (TesteNormalizado9[i,7] == 1 && ResultadoMV9[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoMV9[1,3] <- aux13 }
  
  if (TesteNormalizado9[i,7] == 2 && ResultadoMV9[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoMV9[2,1] <- aux21 }
  if (TesteNormalizado9[i,7] == 2 && ResultadoMV9[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoMV9[2,2] <- aux22 }
  if (TesteNormalizado9[i,7] == 2 && ResultadoMV9[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoMV9[2,3] <- aux23 }
  
  if (TesteNormalizado9[i,7] == 3 && ResultadoMV9[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoMV9[3,1] <- aux31 }
  if (TesteNormalizado9[i,7] == 3 && ResultadoMV9[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoMV9[3,2] <- aux32 }
  if (TesteNormalizado9[i,7] == 3 && ResultadoMV9[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoMV9[3,3] <- aux33 }
}

# Usando a matriz de Teste - Data Set 10 (Teste Normalizado)
###########################################################

# Matriz de Covariancia - Completa
covMatrizDH10_NB <- cov(MatrizDH10_NB[,1:6])
covMatrizNO10_NB <- cov(MatrizNO10_NB[,1:6])
covMatrizSL10_NB <- cov(MatrizSL10_NB[,1:6])

densDH_10 = nrow(MatrizDH10_NB) / nrow(TreiNormalizado10)
densNO_10 = nrow(MatrizNO10_NB) / nrow(TreiNormalizado10)
densSL_10 = nrow(MatrizSL10_NB) / nrow(TreiNormalizado10)

ResultadoMV10 <- matrix(nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteNormalizado10)) {
  
  tNB_DH <- densDH_10*(-0.5*log(det(covMatrizDH10_NB)) - 0.5* mahalanobis(TesteNormalizado10[i,1:6], MD_DH10_NB[,1:6], covMatrizDH10_NB))
  tNB_NO <- densNO_10*(-0.5*log(det(covMatrizNO10_NB)) - 0.5* mahalanobis(TesteNormalizado10[i,1:6], MD_NO10_NB[,1:6], covMatrizNO10_NB))
  tNB_SL <- densSL_10*(-0.5*log(det(covMatrizSL10_NB)) - 0.5* mahalanobis(TesteNormalizado10[i,1:6], MD_SL10_NB[,1:6], covMatrizSL10_NB))
  
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
  
  ResultadoMV10[i,1] <- maior
  ResultadoMV10[i,2] <- classe
}

# Matriz de Confusao

MatConfusaoMV10 <- matrix(0,nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado10)) {
  if (TesteNormalizado10[i,7] == 1 && ResultadoMV10[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoMV10[1,1] <- aux11 }
  if (TesteNormalizado10[i,7] == 1 && ResultadoMV10[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoMV10[1,2] <- aux12 }
  if (TesteNormalizado10[i,7] == 1 && ResultadoMV10[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoMV10[1,3] <- aux13 }
  
  if (TesteNormalizado10[i,7] == 2 && ResultadoMV10[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoMV10[2,1] <- aux21 }
  if (TesteNormalizado10[i,7] == 2 && ResultadoMV10[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoMV10[2,2] <- aux22 }
  if (TesteNormalizado10[i,7] == 2 && ResultadoMV10[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoMV10[2,3] <- aux23 }
  
  if (TesteNormalizado10[i,7] == 3 && ResultadoMV10[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoMV10[3,1] <- aux31 }
  if (TesteNormalizado10[i,7] == 3 && ResultadoMV10[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoMV10[3,2] <- aux32 }
  if (TesteNormalizado10[i,7] == 3 && ResultadoMV10[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoMV10[3,3] <- aux33 }
}

# Matriz de Confusao - MV
MatConfusaoMV1
MatConfusaoMV2
MatConfusaoMV3
MatConfusaoMV4
MatConfusaoMV5
MatConfusaoMV6
MatConfusaoMV7
MatConfusaoMV8
MatConfusaoMV9
MatConfusaoMV10
