options(max.print=5.5E5) #Numero de linhas - retorno
options(error=recover)   #Debugar

###############################################################################################
# (c) Classificador de distancia euclidiana minima ao centroide (minimum-distance classifier) #
###############################################################################################

########################
# Centroides - DataSet 1

MD_DH1 = matrix(c(mean(MatrizDH1[,1]), mean(MatrizDH1[,2]), mean(MatrizDH1[,3]), mean(MatrizDH1[,4]), mean(MatrizDH1[,5]), mean(MatrizDH1[,6]), 1),1,7,1) 
MD_NO1 = matrix(c(mean(MatrizNO1[,1]), mean(MatrizNO1[,2]), mean(MatrizNO1[,3]), mean(MatrizNO1[,4]), mean(MatrizNO1[,5]), mean(MatrizNO1[,6]), 2),1,7,1) 
MD_SL1 = matrix(c(mean(MatrizSL1[,1]), mean(MatrizSL1[,2]), mean(MatrizSL1[,3]), mean(MatrizSL1[,4]), mean(MatrizSL1[,5]), mean(MatrizSL1[,6]), 3),1,7,1) 

# Usando a matriz de Teste
TesteCentroide1     <- Teste1[,1:6]
ResultadoCentroide1 <- matrix(nrow=124,ncol=2) # V1 - Menor Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteCentroide1)) {
  
  t1dh <- ((TesteCentroide1[i,1] - MD_DH1[1,1])^2 + (TesteCentroide1[i,2] - MD_DH1[1,2])^2 + (TesteCentroide1[i,3] - MD_DH1[1,3])^2 + (TesteCentroide1[i,4] - MD_DH1[1,4])^2 + (TesteCentroide1[i,5] - MD_DH1[1,5])^2 + (TesteCentroide1[i,6] - MD_DH1[1,6])^2)
  t1no <- ((TesteCentroide1[i,1] - MD_NO1[1,1])^2 + (TesteCentroide1[i,2] - MD_NO1[1,2])^2 + (TesteCentroide1[i,3] - MD_NO1[1,3])^2 + (TesteCentroide1[i,4] - MD_NO1[1,4])^2 + (TesteCentroide1[i,5] - MD_NO1[1,5])^2 + (TesteCentroide1[i,6] - MD_NO1[1,6])^2)
  t1sl <- ((TesteCentroide1[i,1] - MD_SL1[1,1])^2 + (TesteCentroide1[i,2] - MD_SL1[1,2])^2 + (TesteCentroide1[i,3] - MD_SL1[1,3])^2 + (TesteCentroide1[i,4] - MD_SL1[1,4])^2 + (TesteCentroide1[i,5] - MD_SL1[1,5])^2 + (TesteCentroide1[i,6] - MD_SL1[1,6])^2)
  
  if (t1sl < t1dh) {
    menor1 <- t1sl
    classe1 <- 3 # SL
  } else {
    menor1 <- t1dh
    classe1 <- 1 } # DH  
  
  if (menor1 < t1no) {
    menor1 <- menor1
    classe1 <- classe1
  } else {
    menor1 <- t1no
    classe1 <- 2 } # NO
    
  ResultadoCentroide1[i,1] <- menor1
  ResultadoCentroide1[i,2] <- classe1
}

Teste1              # Matriz de Teste
ResultadoCentroide1 # Matriz de Resultados do Centroide

###################
# Matriz de Confusao

MatConfusao1 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteCentroide1)) {
  if (Teste1[i,7] == 1 && ResultadoCentroide1[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusao1[1,1] <- aux11 }
  if (Teste1[i,7] == 1 && ResultadoCentroide1[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusao1[1,2] <- aux12 }
  if (Teste1[i,7] == 1 && ResultadoCentroide1[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusao1[1,3] <- aux13 }
  
  if (Teste1[i,7] == 2 && ResultadoCentroide1[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusao1[2,1] <- aux21 }
  if (Teste1[i,7] == 2 && ResultadoCentroide1[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusao1[2,2] <- aux22 }
  if (Teste1[i,7] == 2 && ResultadoCentroide1[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusao1[2,3] <- aux23 }
  
  if (Teste1[i,7] == 3 && ResultadoCentroide1[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusao1[3,1] <- aux31 }
  if (Teste1[i,7] == 3 && ResultadoCentroide1[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusao1[3,2] <- aux32 }
  if (Teste1[i,7] == 3 && ResultadoCentroide1[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusao1[3,3] <- aux33 }
}

########################
# Centroides - DataSet 2
MD_DH2 = matrix(c(mean(MatrizDH2[,1]), mean(MatrizDH2[,2]), mean(MatrizDH2[,3]), mean(MatrizDH2[,4]), mean(MatrizDH2[,5]), mean(MatrizDH2[,6]), 1),1,7,1) 
MD_NO2 = matrix(c(mean(MatrizNO2[,1]), mean(MatrizNO2[,2]), mean(MatrizNO2[,3]), mean(MatrizNO2[,4]), mean(MatrizNO2[,5]), mean(MatrizNO2[,6]), 2),1,7,1) 
MD_SL2 = matrix(c(mean(MatrizSL2[,1]), mean(MatrizSL2[,2]), mean(MatrizSL2[,3]), mean(MatrizSL2[,4]), mean(MatrizSL2[,5]), mean(MatrizSL2[,6]), 3),1,7,1) 

# Usando a matriz de Teste
TesteCentroide2     <- Teste2[,1:6]
ResultadoCentroide2 <- matrix(nrow=124,ncol=2) # V1 - Menor Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteCentroide2)) {
  
  t2dh <- ((TesteCentroide2[i,1] - MD_DH2[1,1])^2 + (TesteCentroide2[i,2] - MD_DH2[1,2])^2 + (TesteCentroide2[i,3] - MD_DH2[1,3])^2 + (TesteCentroide2[i,4] - MD_DH2[1,4])^2 + (TesteCentroide2[i,5] - MD_DH2[1,5])^2 + (TesteCentroide2[i,6] - MD_DH2[1,6])^2)
  t2no <- ((TesteCentroide2[i,1] - MD_NO2[1,1])^2 + (TesteCentroide2[i,2] - MD_NO2[1,2])^2 + (TesteCentroide2[i,3] - MD_NO2[1,3])^2 + (TesteCentroide2[i,4] - MD_NO2[1,4])^2 + (TesteCentroide2[i,5] - MD_NO2[1,5])^2 + (TesteCentroide2[i,6] - MD_NO2[1,6])^2)
  t2sl <- ((TesteCentroide2[i,1] - MD_SL2[1,1])^2 + (TesteCentroide2[i,2] - MD_SL2[1,2])^2 + (TesteCentroide2[i,3] - MD_SL2[1,3])^2 + (TesteCentroide2[i,4] - MD_SL2[1,4])^2 + (TesteCentroide2[i,5] - MD_SL2[1,5])^2 + (TesteCentroide2[i,6] - MD_SL2[1,6])^2)
  
  if (t2sl < t2dh) {
    menor2 <- t2sl
    classe2 <- 3 # SL
  } else {
    menor2 <- t2dh
    classe2 <- 1 } # DH  
  
  if (menor2 < t2no) {
    menor2 <- menor2
    classe2 <- classe2
  } else {
    menor2 <- t2no
    classe2 <- 2 } # NO
    
  ResultadoCentroide2[i,1] <- menor2
  ResultadoCentroide2[i,2] <- classe2
}

Teste2              # Matriz de Teste
ResultadoCentroide2 # Matriz de Resultados do Centroide

####################
# Matriz de Confusao

MatConfusao2 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteCentroide2)) {
  if (Teste2[i,7] == 1 && ResultadoCentroide2[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusao2[1,1] <- aux11 }
  if (Teste2[i,7] == 1 && ResultadoCentroide2[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusao2[1,2] <- aux12 }
  if (Teste2[i,7] == 1 && ResultadoCentroide2[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusao2[1,3] <- aux13 }
  
  if (Teste2[i,7] == 2 && ResultadoCentroide2[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusao2[2,1] <- aux21 }
  if (Teste2[i,7] == 2 && ResultadoCentroide2[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusao2[2,2] <- aux22 }
  if (Teste2[i,7] == 2 && ResultadoCentroide2[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusao2[2,3] <- aux23 }
  
  if (Teste2[i,7] == 3 && ResultadoCentroide2[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusao2[3,1] <- aux31 }
  if (Teste2[i,7] == 3 && ResultadoCentroide2[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusao2[3,2] <- aux32 }
  if (Teste2[i,7] == 3 && ResultadoCentroide2[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusao2[3,3] <- aux33 }
}

########################
# Centroides - DataSet 3
MD_DH3 = matrix(c(mean(MatrizDH3[,1]), mean(MatrizDH3[,2]), mean(MatrizDH3[,3]), mean(MatrizDH3[,4]), mean(MatrizDH3[,5]), mean(MatrizDH3[,6]), 1),1,7,1) 
MD_NO3 = matrix(c(mean(MatrizNO3[,1]), mean(MatrizNO3[,2]), mean(MatrizNO3[,3]), mean(MatrizNO3[,4]), mean(MatrizNO3[,5]), mean(MatrizNO3[,6]), 2),1,7,1) 
MD_SL3 = matrix(c(mean(MatrizSL3[,1]), mean(MatrizSL3[,2]), mean(MatrizSL3[,3]), mean(MatrizSL3[,4]), mean(MatrizSL3[,5]), mean(MatrizSL3[,6]), 3),1,7,1)

# Usando a matriz de Teste
TesteCentroide3     <- Teste3[,1:6]
ResultadoCentroide3 <- matrix(nrow=124,ncol=2) # V1 - Menor Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteCentroide3)) {
  
  t3dh <- ((TesteCentroide3[i,1] - MD_DH3[1,1])^2 + (TesteCentroide3[i,2] - MD_DH3[1,2])^2 + (TesteCentroide3[i,3] - MD_DH3[1,3])^2 + (TesteCentroide3[i,4] - MD_DH3[1,4])^2 + (TesteCentroide3[i,5] - MD_DH3[1,5])^2 + (TesteCentroide3[i,6] - MD_DH3[1,6])^2)
  t3no <- ((TesteCentroide3[i,1] - MD_NO3[1,1])^2 + (TesteCentroide3[i,2] - MD_NO3[1,2])^2 + (TesteCentroide3[i,3] - MD_NO3[1,3])^2 + (TesteCentroide3[i,4] - MD_NO3[1,4])^2 + (TesteCentroide3[i,5] - MD_NO3[1,5])^2 + (TesteCentroide3[i,6] - MD_NO3[1,6])^2)
  t3sl <- ((TesteCentroide3[i,1] - MD_SL3[1,1])^2 + (TesteCentroide3[i,2] - MD_SL3[1,2])^2 + (TesteCentroide3[i,3] - MD_SL3[1,3])^2 + (TesteCentroide3[i,4] - MD_SL3[1,4])^2 + (TesteCentroide3[i,5] - MD_SL3[1,5])^2 + (TesteCentroide3[i,6] - MD_SL3[1,6])^2)
  
  if (t3sl  < t3dh) {
    menor3  <- t3sl
    classe3 <- 3 # SL
  } else {
    menor3  <- t3dh
    classe3 <- 1 } # DH  
  
  if (menor3 < t3no) {
    menor3   <- menor3
    classe3  <- classe3
  } else {
    menor3  <- t3no
    classe3 <- 2 } # NO
    
  ResultadoCentroide3[i,1] <- menor3
  ResultadoCentroide3[i,2] <- classe3
}

Teste3              # Matriz de Teste
ResultadoCentroide3 # Matriz de Resultados do Centroide

####################
# Matriz de Confusão

MatConfusao3 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteCentroide3)) {
  if (Teste3[i,7] == 1 && ResultadoCentroide3[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusao3[1,1] <- aux11 }
  if (Teste3[i,7] == 1 && ResultadoCentroide3[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusao3[1,2] <- aux12 }
  if (Teste3[i,7] == 1 && ResultadoCentroide3[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusao3[1,3] <- aux13 }
  
  if (Teste3[i,7] == 2 && ResultadoCentroide3[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusao3[2,1] <- aux21 }
  if (Teste3[i,7] == 2 && ResultadoCentroide3[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusao3[2,2] <- aux22 }
  if (Teste3[i,7] == 2 && ResultadoCentroide3[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusao3[2,3] <- aux23 }
  
  if (Teste3[i,7] == 3 && ResultadoCentroide3[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusao3[3,1] <- aux31 }
  if (Teste3[i,7] == 3 && ResultadoCentroide3[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusao3[3,2] <- aux32 }
  if (Teste3[i,7] == 3 && ResultadoCentroide3[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusao3[3,3] <- aux33 }
}

########################
# Centroides - DataSet 4
MD_DH4 = matrix(c(mean(MatrizDH4[,1]), mean(MatrizDH4[,2]), mean(MatrizDH4[,3]), mean(MatrizDH4[,4]), mean(MatrizDH4[,5]), mean(MatrizDH4[,6]), 1),1,7,1) 
MD_NO4 = matrix(c(mean(MatrizNO4[,1]), mean(MatrizNO4[,2]), mean(MatrizNO4[,3]), mean(MatrizNO4[,4]), mean(MatrizNO4[,5]), mean(MatrizNO4[,6]), 2),1,7,1) 
MD_SL4 = matrix(c(mean(MatrizSL4[,1]), mean(MatrizSL4[,2]), mean(MatrizSL4[,3]), mean(MatrizSL4[,4]), mean(MatrizSL4[,5]), mean(MatrizSL4[,6]), 3),1,7,1)

# Usando a matriz de Teste
TesteCentroide4     <- Teste4[,1:6]
ResultadoCentroide4 <- matrix(nrow=124,ncol=2) # V1 - Menor Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteCentroide4)) {
  
  t4dh <- ((TesteCentroide4[i,1] - MD_DH4[1,1])^2 + (TesteCentroide4[i,2] - MD_DH4[1,2])^2 + (TesteCentroide4[i,3] - MD_DH4[1,3])^2 + (TesteCentroide4[i,4] - MD_DH4[1,4])^2 + (TesteCentroide4[i,5] - MD_DH4[1,5])^2 + (TesteCentroide4[i,6] - MD_DH4[1,6])^2)
  t4no <- ((TesteCentroide4[i,1] - MD_NO4[1,1])^2 + (TesteCentroide4[i,2] - MD_NO4[1,2])^2 + (TesteCentroide4[i,3] - MD_NO4[1,3])^2 + (TesteCentroide4[i,4] - MD_NO4[1,4])^2 + (TesteCentroide4[i,5] - MD_NO4[1,5])^2 + (TesteCentroide4[i,6] - MD_NO4[1,6])^2)
  t4sl <- ((TesteCentroide4[i,1] - MD_SL4[1,1])^2 + (TesteCentroide4[i,2] - MD_SL4[1,2])^2 + (TesteCentroide4[i,3] - MD_SL4[1,3])^2 + (TesteCentroide4[i,4] - MD_SL4[1,4])^2 + (TesteCentroide4[i,5] - MD_SL4[1,5])^2 + (TesteCentroide4[i,6] - MD_SL4[1,6])^2)
  
  if (t4sl  < t4dh) {
    menor4  <- t4sl
    classe4 <- 3 # SL
  } else {
    menor4  <- t4dh
    classe4 <- 1 } # DH  
  
  if (menor4 < t4no) {
    menor4   <- menor4
    classe4  <- classe4
  } else {
    menor4  <- t1no
    classe4 <- 2 } # NO
    
  ResultadoCentroide4[i,1] <- menor4
  ResultadoCentroide4[i,2] <- classe4
}

Teste4              # Matriz de Teste
ResultadoCentroide4 # Matriz de Resultados do Centroide

####################
# Matriz de Confusao

MatConfusao4 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteCentroide4)) {
  if (Teste4[i,7] == 1 && ResultadoCentroide4[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusao4[1,1] <- aux11 }
  if (Teste4[i,7] == 1 && ResultadoCentroide4[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusao4[1,2] <- aux12 }
  if (Teste4[i,7] == 1 && ResultadoCentroide4[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusao4[1,3] <- aux13 }
  
  if (Teste4[i,7] == 2 && ResultadoCentroide4[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusao4[2,1] <- aux21 }
  if (Teste4[i,7] == 2 && ResultadoCentroide4[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusao4[2,2] <- aux22 }
  if (Teste4[i,7] == 2 && ResultadoCentroide4[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusao4[2,3] <- aux23 }
  
  if (Teste4[i,7] == 3 && ResultadoCentroide4[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusao4[3,1] <- aux31 }
  if (Teste4[i,7] == 3 && ResultadoCentroide4[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusao4[3,2] <- aux32 }
  if (Teste4[i,7] == 3 && ResultadoCentroide4[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusao4[3,3] <- aux33 }
}

########################
# Centroides - DataSet 5
MD_DH5 = matrix(c(mean(MatrizDH5[,1]), mean(MatrizDH5[,2]), mean(MatrizDH5[,3]), mean(MatrizDH5[,4]), mean(MatrizDH5[,5]), mean(MatrizDH5[,6]), 1),1,7,1)
MD_NO5 = matrix(c(mean(MatrizNO5[,1]), mean(MatrizNO5[,2]), mean(MatrizNO5[,3]), mean(MatrizNO5[,4]), mean(MatrizNO5[,5]), mean(MatrizNO5[,6]), 2),1,7,1)
MD_SL5 = matrix(c(mean(MatrizSL5[,1]), mean(MatrizSL5[,2]), mean(MatrizSL5[,3]), mean(MatrizSL5[,4]), mean(MatrizSL5[,5]), mean(MatrizSL5[,6]), 3),1,7,1)

# Usando a matriz de Teste
TesteCentroide5     <- Teste5[,1:6]
ResultadoCentroide5 <- matrix(nrow=124,ncol=2) # V1 - Menor Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteCentroide5)) {
  
  t5dh <- ((TesteCentroide5[i,1] - MD_DH5[1,1])^2 + (TesteCentroide5[i,2] - MD_DH5[1,2])^2 + (TesteCentroide5[i,3] - MD_DH5[1,3])^2 + (TesteCentroide5[i,4] - MD_DH5[1,4])^2 + (TesteCentroide5[i,5] - MD_DH5[1,5])^2 + (TesteCentroide5[i,6] - MD_DH5[1,6])^2)
  t5no <- ((TesteCentroide5[i,1] - MD_NO5[1,1])^2 + (TesteCentroide5[i,2] - MD_NO5[1,2])^2 + (TesteCentroide5[i,3] - MD_NO5[1,3])^2 + (TesteCentroide5[i,4] - MD_NO5[1,4])^2 + (TesteCentroide5[i,5] - MD_NO5[1,5])^2 + (TesteCentroide5[i,6] - MD_NO5[1,6])^2)
  t5sl <- ((TesteCentroide5[i,1] - MD_SL5[1,1])^2 + (TesteCentroide5[i,2] - MD_SL5[1,2])^2 + (TesteCentroide5[i,3] - MD_SL5[1,3])^2 + (TesteCentroide5[i,4] - MD_SL5[1,4])^2 + (TesteCentroide5[i,5] - MD_SL5[1,5])^2 + (TesteCentroide5[i,6] - MD_SL5[1,6])^2)
  
  if (t5sl  < t5dh) {
    menor5  <- t5sl
    classe5 <- 3 # SL
  } else {
    menor5  <- t5dh
    classe5 <- 1 } # DH  
  
  if (menor5 < t5no) {
    menor5   <- menor5
    classe5  <- classe5
  } else {
    menor5  <- t5no
    classe5 <- 2 } # NO
    
  ResultadoCentroide5[i,1] <- menor5
  ResultadoCentroide5[i,2] <- classe5
}

Teste5              # Matriz de Teste
ResultadoCentroide5 # Matriz de Resultados do Centroide

####################
# Matriz de Confusao

MatConfusao5 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteCentroide5)) {
  if (Teste5[i,7] == 1 && ResultadoCentroide5[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusao5[1,1] <- aux11 }
  if (Teste5[i,7] == 1 && ResultadoCentroide5[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusao5[1,2] <- aux12 }
  if (Teste5[i,7] == 1 && ResultadoCentroide5[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusao5[1,3] <- aux13 }
  
  if (Teste5[i,7] == 2 && ResultadoCentroide5[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusao5[2,1] <- aux21 }
  if (Teste5[i,7] == 2 && ResultadoCentroide5[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusao5[2,2] <- aux22 }
  if (Teste5[i,7] == 2 && ResultadoCentroide5[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusao5[2,3] <- aux23 }
  
  if (Teste5[i,7] == 3 && ResultadoCentroide5[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusao5[3,1] <- aux31 }
  if (Teste5[i,7] == 3 && ResultadoCentroide5[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusao5[3,2] <- aux32 }
  if (Teste5[i,7] == 3 && ResultadoCentroide5[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusao5[3,3] <- aux33 }
}

########################
# Centroides - DataSet 6
MD_DH6 = matrix(c(mean(MatrizDH6[,1]), mean(MatrizDH6[,2]), mean(MatrizDH6[,3]), mean(MatrizDH6[,4]), mean(MatrizDH6[,5]), mean(MatrizDH6[,6]), 1),1,7,1)
MD_NO6 = matrix(c(mean(MatrizNO6[,1]), mean(MatrizNO6[,2]), mean(MatrizNO6[,3]), mean(MatrizNO6[,4]), mean(MatrizNO6[,5]), mean(MatrizNO6[,6]), 2),1,7,1)
MD_SL6 = matrix(c(mean(MatrizSL6[,1]), mean(MatrizSL6[,2]), mean(MatrizSL6[,3]), mean(MatrizSL6[,4]), mean(MatrizSL6[,5]), mean(MatrizSL6[,6]), 3),1,7,1)

# Usando a matriz de Teste
TesteCentroide6     <- Teste6[,1:6]
ResultadoCentroide6 <- matrix(nrow=124,ncol=2) # V1 - Menor Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteCentroide6)) {
  
  t6dh <- ((TesteCentroide6[i,1] - MD_DH6[1,1])^2 + (TesteCentroide6[i,2] - MD_DH6[1,2])^2 + (TesteCentroide6[i,3] - MD_DH6[1,3])^2 + (TesteCentroide6[i,4] - MD_DH6[1,4])^2 + (TesteCentroide6[i,5] - MD_DH6[1,5])^2 + (TesteCentroide6[i,6] - MD_DH6[1,6])^2)
  t6no <- ((TesteCentroide6[i,1] - MD_NO6[1,1])^2 + (TesteCentroide6[i,2] - MD_NO6[1,2])^2 + (TesteCentroide6[i,3] - MD_NO6[1,3])^2 + (TesteCentroide6[i,4] - MD_NO6[1,4])^2 + (TesteCentroide6[i,5] - MD_NO6[1,5])^2 + (TesteCentroide6[i,6] - MD_NO6[1,6])^2)
  t6sl <- ((TesteCentroide6[i,1] - MD_SL6[1,1])^2 + (TesteCentroide6[i,2] - MD_SL6[1,2])^2 + (TesteCentroide6[i,3] - MD_SL6[1,3])^2 + (TesteCentroide6[i,4] - MD_SL6[1,4])^2 + (TesteCentroide6[i,5] - MD_SL6[1,5])^2 + (TesteCentroide6[i,6] - MD_SL6[1,6])^2)
  
  if (t6sl  < t6dh) {
    menor6  <- t6sl
    classe6 <- 3 # SL
  } else {
    menor6  <- t6dh
    classe6 <- 1 } # DH  
  
  if (menor6 < t6no) {
    menor6   <- menor6
    classe6  <- classe6
  } else {
    menor6  <- t6no
    classe6 <- 2 } # NO
    
  ResultadoCentroide6[i,1] <- menor6
  ResultadoCentroide6[i,2] <- classe6
}

Teste6              # Matriz de Teste
ResultadoCentroide6 # Matriz de Resultados do Centroide

####################
# Matriz de Confusao

MatConfusao6 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteCentroide6)) {
  if (Teste6[i,7] == 1 && ResultadoCentroide6[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusao6[1,1] <- aux11 }
  if (Teste6[i,7] == 1 && ResultadoCentroide6[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusao6[1,2] <- aux12 }
  if (Teste6[i,7] == 1 && ResultadoCentroide6[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusao6[1,3] <- aux13 }
  
  if (Teste6[i,7] == 2 && ResultadoCentroide6[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusao6[2,1] <- aux21 }
  if (Teste6[i,7] == 2 && ResultadoCentroide6[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusao6[2,2] <- aux22 }
  if (Teste6[i,7] == 2 && ResultadoCentroide6[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusao6[2,3] <- aux23 }
  
  if (Teste6[i,7] == 3 && ResultadoCentroide6[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusao6[3,1] <- aux31 }
  if (Teste6[i,7] == 3 && ResultadoCentroide6[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusao6[3,2] <- aux32 }
  if (Teste6[i,7] == 3 && ResultadoCentroide6[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusao6[3,3] <- aux33 }
}

########################
# Centroides - DataSet 7
MD_DH7 = matrix(c(mean(MatrizDH7[,1]), mean(MatrizDH7[,2]), mean(MatrizDH7[,3]), mean(MatrizDH7[,4]), mean(MatrizDH7[,5]), mean(MatrizDH7[,6]), 1),1,7,1)
MD_NO7 = matrix(c(mean(MatrizNO7[,1]), mean(MatrizNO7[,2]), mean(MatrizNO7[,3]), mean(MatrizNO7[,4]), mean(MatrizNO7[,5]), mean(MatrizNO7[,6]), 2),1,7,1)
MD_SL7 = matrix(c(mean(MatrizSL7[,1]), mean(MatrizSL7[,2]), mean(MatrizSL7[,3]), mean(MatrizSL7[,4]), mean(MatrizSL7[,5]), mean(MatrizSL7[,6]), 3),1,7,1)

# Usando a matriz de Teste
TesteCentroide7     <- Teste7[,1:6]
ResultadoCentroide7 <- matrix(nrow=124,ncol=2) # V1 - Menor Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteCentroide7)) {
  
  t7dh <- ((TesteCentroide7[i,1] - MD_DH7[1,1])^2 + (TesteCentroide7[i,2] - MD_DH7[1,2])^2 + (TesteCentroide7[i,3] - MD_DH7[1,3])^2 + (TesteCentroide7[i,4] - MD_DH7[1,4])^2 + (TesteCentroide7[i,5] - MD_DH7[1,5])^2 + (TesteCentroide7[i,6] - MD_DH7[1,6])^2)
  t7no <- ((TesteCentroide7[i,1] - MD_NO7[1,1])^2 + (TesteCentroide7[i,2] - MD_NO7[1,2])^2 + (TesteCentroide7[i,3] - MD_NO7[1,3])^2 + (TesteCentroide7[i,4] - MD_NO7[1,4])^2 + (TesteCentroide7[i,5] - MD_NO7[1,5])^2 + (TesteCentroide7[i,6] - MD_NO7[1,6])^2)
  t7sl <- ((TesteCentroide7[i,1] - MD_SL7[1,1])^2 + (TesteCentroide7[i,2] - MD_SL7[1,2])^2 + (TesteCentroide7[i,3] - MD_SL7[1,3])^2 + (TesteCentroide7[i,4] - MD_SL7[1,4])^2 + (TesteCentroide7[i,5] - MD_SL7[1,5])^2 + (TesteCentroide7[i,6] - MD_SL7[1,6])^2)
  
  if (t7sl  < t7dh) {
    menor7  <- t7sl
    classe7 <- 3 # SL
  } else {
    menor7  <- t7dh
    classe7 <- 1 } # DH  
  
  if (menor7 < t7no) {
    menor7   <- menor7
    classe7  <- classe7
  } else {
    menor7  <- t7no
    classe7 <- 2 } # NO
    
  ResultadoCentroide7[i,1] <- menor7
  ResultadoCentroide7[i,2] <- classe7
}

Teste7              # Matriz de Teste
ResultadoCentroide7 # Matriz de Resultados do Centroide

####################
# Matriz de Confusao

MatConfusao7 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteCentroide7)) {
  if (Teste7[i,7] == 1 && ResultadoCentroide7[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusao7[1,1] <- aux11 }
  if (Teste7[i,7] == 1 && ResultadoCentroide7[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusao7[1,2] <- aux12 }
  if (Teste7[i,7] == 1 && ResultadoCentroide7[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusao7[1,3] <- aux13 }
  
  if (Teste7[i,7] == 2 && ResultadoCentroide7[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusao7[2,1] <- aux21 }
  if (Teste7[i,7] == 2 && ResultadoCentroide7[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusao7[2,2] <- aux22 }
  if (Teste7[i,7] == 2 && ResultadoCentroide7[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusao7[2,3] <- aux23 }
  
  if (Teste7[i,7] == 3 && ResultadoCentroide7[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusao7[3,1] <- aux31 }
  if (Teste7[i,7] == 3 && ResultadoCentroide7[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusao7[3,2] <- aux32 }
  if (Teste7[i,7] == 3 && ResultadoCentroide7[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusao7[3,3] <- aux33 }
}

########################
# Centroides - DataSet 8
MD_DH8 = matrix(c(mean(MatrizDH8[,1]), mean(MatrizDH8[,2]), mean(MatrizDH8[,3]), mean(MatrizDH8[,4]), mean(MatrizDH8[,5]), mean(MatrizDH8[,6]), 1),1,7,1)
MD_NO8 = matrix(c(mean(MatrizNO8[,1]), mean(MatrizNO8[,2]), mean(MatrizNO8[,3]), mean(MatrizNO8[,4]), mean(MatrizNO8[,5]), mean(MatrizNO8[,6]), 2),1,7,1)
MD_SL8 = matrix(c(mean(MatrizSL8[,1]), mean(MatrizSL8[,2]), mean(MatrizSL8[,3]), mean(MatrizSL8[,4]), mean(MatrizSL8[,5]), mean(MatrizSL8[,6]), 3),1,7,1)

# Usando a matriz de Teste
TesteCentroide8     <- Teste8[,1:6]
ResultadoCentroide8 <- matrix(nrow=124,ncol=2) # V1 - Menor Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteCentroide8)) {
  
  t8dh <- ((TesteCentroide8[i,1] - MD_DH8[1,1])^2 + (TesteCentroide8[i,2] - MD_DH8[1,2])^2 + (TesteCentroide8[i,3] - MD_DH8[1,3])^2 + (TesteCentroide8[i,4] - MD_DH8[1,4])^2 + (TesteCentroide8[i,5] - MD_DH8[1,5])^2 + (TesteCentroide8[i,6] - MD_DH8[1,6])^2)
  t8no <- ((TesteCentroide8[i,1] - MD_NO8[1,1])^2 + (TesteCentroide8[i,2] - MD_NO8[1,2])^2 + (TesteCentroide8[i,3] - MD_NO8[1,3])^2 + (TesteCentroide8[i,4] - MD_NO8[1,4])^2 + (TesteCentroide8[i,5] - MD_NO8[1,5])^2 + (TesteCentroide8[i,6] - MD_NO8[1,6])^2)
  t8sl <- ((TesteCentroide8[i,1] - MD_SL8[1,1])^2 + (TesteCentroide8[i,2] - MD_SL8[1,2])^2 + (TesteCentroide8[i,3] - MD_SL8[1,3])^2 + (TesteCentroide8[i,4] - MD_SL8[1,4])^2 + (TesteCentroide8[i,5] - MD_SL8[1,5])^2 + (TesteCentroide8[i,6] - MD_SL8[1,6])^2)
  
  if (t8sl  < t8dh) {
    menor8  <- t8sl
    classe8 <- 3 # SL
  } else {
    menor8  <- t8dh
    classe8 <- 1 } # DH  
  
  if (menor8 < t8no) {
    menor8   <- menor8
    classe8  <- classe8
  } else {
    menor8   <- t8no
    classe8  <- 2 } # NO
    
  ResultadoCentroide8[i,1] <- menor8
  ResultadoCentroide8[i,2] <- classe8
}

Teste8              # Matriz de Teste
ResultadoCentroide8 # Matriz de Resultados do Centroide

####################
# Matriz de Confusao

MatConfusao8 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteCentroide8)) {
  if (Teste8[i,7] == 1 && ResultadoCentroide8[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusao8[1,1] <- aux11 }
  if (Teste8[i,7] == 1 && ResultadoCentroide8[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusao8[1,2] <- aux12 }
  if (Teste8[i,7] == 1 && ResultadoCentroide8[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusao8[1,3] <- aux13 }
  
  if (Teste8[i,7] == 2 && ResultadoCentroide8[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusao8[2,1] <- aux21 }
  if (Teste8[i,7] == 2 && ResultadoCentroide8[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusao8[2,2] <- aux22 }
  if (Teste8[i,7] == 2 && ResultadoCentroide8[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusao8[2,3] <- aux23 }
  
  if (Teste8[i,7] == 3 && ResultadoCentroide8[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusao8[3,1] <- aux31 }
  if (Teste8[i,7] == 3 && ResultadoCentroide8[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusao8[3,2] <- aux32 }
  if (Teste8[i,7] == 3 && ResultadoCentroide8[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusao8[3,3] <- aux33 }
}

########################
# Centroides - DataSet 9
MD_DH9 = matrix(c(mean(MatrizDH9[,1]), mean(MatrizDH9[,2]), mean(MatrizDH9[,3]), mean(MatrizDH9[,4]), mean(MatrizDH9[,5]), mean(MatrizDH9[,6]), 1),1,7,1)
MD_NO9 = matrix(c(mean(MatrizNO9[,1]), mean(MatrizNO9[,2]), mean(MatrizNO9[,3]), mean(MatrizNO9[,4]), mean(MatrizNO9[,5]), mean(MatrizNO9[,6]), 2),1,7,1)
MD_SL9 = matrix(c(mean(MatrizSL9[,1]), mean(MatrizSL9[,2]), mean(MatrizSL9[,3]), mean(MatrizSL9[,4]), mean(MatrizSL9[,5]), mean(MatrizSL9[,6]), 3),1,7,1)

# Usando a matriz de Teste
TesteCentroide9     <- Teste9[,1:6]
ResultadoCentroide9 <- matrix(nrow=124,ncol=2) # V1 - Menor Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteCentroide9)) {
  
  t9dh <- ((TesteCentroide9[i,1] - MD_DH9[1,1])^2 + (TesteCentroide9[i,2] - MD_DH9[1,2])^2 + (TesteCentroide9[i,3] - MD_DH9[1,3])^2 + (TesteCentroide9[i,4] - MD_DH9[1,4])^2 + (TesteCentroide9[i,5] - MD_DH9[1,5])^2 + (TesteCentroide9[i,6] - MD_DH9[1,6])^2)
  t9no <- ((TesteCentroide9[i,1] - MD_NO9[1,1])^2 + (TesteCentroide9[i,2] - MD_NO9[1,2])^2 + (TesteCentroide9[i,3] - MD_NO9[1,3])^2 + (TesteCentroide9[i,4] - MD_NO9[1,4])^2 + (TesteCentroide9[i,5] - MD_NO9[1,5])^2 + (TesteCentroide9[i,6] - MD_NO9[1,6])^2)
  t9sl <- ((TesteCentroide9[i,1] - MD_SL9[1,1])^2 + (TesteCentroide9[i,2] - MD_SL9[1,2])^2 + (TesteCentroide9[i,3] - MD_SL9[1,3])^2 + (TesteCentroide9[i,4] - MD_SL9[1,4])^2 + (TesteCentroide9[i,5] - MD_SL9[1,5])^2 + (TesteCentroide9[i,6] - MD_SL9[1,6])^2)
  
  if (t9sl  < t9dh) {
    menor9  <- t9sl
    classe9 <- 3 # SL
  } else {
    menor9  <- t9dh
    classe9 <- 1 } # DH  
  
  if (menor9 < t9no) {
    menor9   <- menor9
    classe9  <- classe9
  } else {
    menor9  <- t9no
    classe9 <- 2 } # NO
    
  ResultadoCentroide9[i,1] <- menor9
  ResultadoCentroide9[i,2] <- classe9
}

Teste9              # Matriz de Teste
ResultadoCentroide9 # Matriz de Resultados do Centroide

####################
# Matriz de Confusao

MatConfusao9 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteCentroide9)) {
  if (Teste9[i,7] == 1 && ResultadoCentroide9[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusao9[1,1] <- aux11 }
  if (Teste9[i,7] == 1 && ResultadoCentroide9[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusao9[1,2] <- aux12 }
  if (Teste9[i,7] == 1 && ResultadoCentroide9[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusao9[1,3] <- aux13 }
  
  if (Teste9[i,7] == 2 && ResultadoCentroide9[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusao9[2,1] <- aux21 }
  if (Teste9[i,7] == 2 && ResultadoCentroide9[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusao9[2,2] <- aux22 }
  if (Teste9[i,7] == 2 && ResultadoCentroide9[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusao9[2,3] <- aux23 }
  
  if (Teste9[i,7] == 3 && ResultadoCentroide9[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusao9[3,1] <- aux31 }
  if (Teste9[i,7] == 3 && ResultadoCentroide9[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusao9[3,2] <- aux32 }
  if (Teste9[i,7] == 3 && ResultadoCentroide9[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusao9[3,3] <- aux33 }
}

########################
# Centroides - DataSet 10
MD_DH10 = matrix(c(mean(MatrizDH10[,1]), mean(MatrizDH10[,2]), mean(MatrizDH10[,3]), mean(MatrizDH10[,4]), mean(MatrizDH10[,5]), mean(MatrizDH10[,6]), 1),1,7,1)
MD_NO10 = matrix(c(mean(MatrizNO10[,1]), mean(MatrizNO10[,2]), mean(MatrizNO10[,3]), mean(MatrizNO10[,4]), mean(MatrizNO10[,5]), mean(MatrizNO10[,6]), 2),1,7,1)
MD_SL10 = matrix(c(mean(MatrizSL10[,1]), mean(MatrizSL10[,2]), mean(MatrizSL10[,3]), mean(MatrizSL10[,4]), mean(MatrizSL10[,5]), mean(MatrizSL10[,6]), 3),1,7,1)

# Usando a matriz de Teste
TesteCentroide10     <- Teste10[,1:6]
ResultadoCentroide10 <- matrix(nrow=124,ncol=2) # V1 - Menor Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(TesteCentroide10)) {
  
  t10dh <- ((TesteCentroide10[i,1] - MD_DH10[1,1])^2 + (TesteCentroide10[i,2] - MD_DH10[1,2])^2 + (TesteCentroide10[i,3] - MD_DH10[1,3])^2 + (TesteCentroide10[i,4] - MD_DH10[1,4])^2 + (TesteCentroide10[i,5] - MD_DH10[1,5])^2 + (TesteCentroide10[i,6] - MD_DH10[1,6])^2)
  t10no <- ((TesteCentroide10[i,1] - MD_NO10[1,1])^2 + (TesteCentroide10[i,2] - MD_NO10[1,2])^2 + (TesteCentroide10[i,3] - MD_NO10[1,3])^2 + (TesteCentroide10[i,4] - MD_NO10[1,4])^2 + (TesteCentroide10[i,5] - MD_NO10[1,5])^2 + (TesteCentroide10[i,6] - MD_NO10[1,6])^2)
  t10sl <- ((TesteCentroide10[i,1] - MD_SL10[1,1])^2 + (TesteCentroide10[i,2] - MD_SL10[1,2])^2 + (TesteCentroide10[i,3] - MD_SL10[1,3])^2 + (TesteCentroide10[i,4] - MD_SL10[1,4])^2 + (TesteCentroide10[i,5] - MD_SL10[1,5])^2 + (TesteCentroide10[i,6] - MD_SL10[1,6])^2)
  
  if (t10sl  < t10dh) {
    menor10  <- t10sl
    classe10 <- 3 # SL
  } else {
    menor10 <- t10dh
    classe10 <- 1 } # DH  
  
  if (menor10 < t10no) {
    menor10   <- menor10
    classe10  <- classe10
  } else {
    menor10 <- t10no
    classe10 <- 2 } # NO
    
  ResultadoCentroide10[i,1] <- menor10
  ResultadoCentroide10[i,2] <- classe10
}

Teste10              # Matriz de Teste
ResultadoCentroide10 # Matriz de Resultados do Centroide

####################
# Matriz de Confusao

MatConfusao10 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteCentroide10)) {
  if (Teste10[i,7] == 1 && ResultadoCentroide10[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusao10[1,1] <- aux11 }
  if (Teste10[i,7] == 1 && ResultadoCentroide10[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusao10[1,2] <- aux12 }
  if (Teste10[i,7] == 1 && ResultadoCentroide10[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusao10[1,3] <- aux13 }
  
  if (Teste10[i,7] == 2 && ResultadoCentroide10[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusao10[2,1] <- aux21 }
  if (Teste10[i,7] == 2 && ResultadoCentroide10[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusao10[2,2] <- aux22 }
  if (Teste10[i,7] == 2 && ResultadoCentroide10[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusao10[2,3] <- aux23 }
  
  if (Teste10[i,7] == 3 && ResultadoCentroide10[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusao10[3,1] <- aux31 }
  if (Teste10[i,7] == 3 && ResultadoCentroide10[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusao10[3,2] <- aux32 }
  if (Teste10[i,7] == 3 && ResultadoCentroide10[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusao10[3,3] <- aux33 }
}

# sum(diag(MatConfusao1))

MatConfusao1  # 94
MatConfusao2  # 96
MatConfusao3  # 101
MatConfusao4  # 91
MatConfusao5  # 94
MatConfusao6  # 100
MatConfusao7  # 91
MatConfusao8  # 102
MatConfusao9  # 91
MatConfusao10 # 93

mdConf = (sum(diag(MatConfusao1)) + sum(diag(MatConfusao2)) + sum(diag(MatConfusao3)) + sum(diag(MatConfusao4)) + sum(diag(MatConfusao5)) + sum(diag(MatConfusao6)) + sum(diag(MatConfusao7)) + sum(diag(MatConfusao8)) + sum(diag(MatConfusao9)) + sum(diag(MatConfusao10))) / 10
# 95.3