options(max.print=5.5E5) #Numero de linhas - retorno
options(error=recover)   #Debugar

#Importar arquivo
#library(readr)
ColVertebral <- read.table("C:/temp/column_3C.dat") 

#Matriz -> DH 1 - SL 3 - NO 2
MatrizColVertebral <- data.matrix(data.frame(ColVertebral, stringsAsFactors = FALSE))

# Dados de Treinamento (60%) / Teste (40%) - Aleatorios
# Data Set 1
TreinamentoAux1 = sample(1:nrow(MatrizColVertebral), nrow(MatrizColVertebral)*0.60, replace=FALSE) 
TreinamentoAux1 = sort(TreinamentoAux1) # Ordena os Indices
Treinamento1    = MatrizColVertebral[TreinamentoAux1,]  # retorna as linhas correspondendo aos indices
Teste1          = MatrizColVertebral[-TreinamentoAux1,] # pega o restante

# Data Set 1 - Normalizacao
# -------------------------
TreiNormalizado1 <- Treinamento1[,1:7]

for(i in 1:nrow(TreiNormalizado1)) {
  TreiNormalizado1[i,1] <- (Treinamento1[i,1] - mean(Treinamento1[,1])) / sd(Treinamento1[,1])
  TreiNormalizado1[i,2] <- (Treinamento1[i,2] - mean(Treinamento1[,2])) / sd(Treinamento1[,2])
  TreiNormalizado1[i,3] <- (Treinamento1[i,3] - mean(Treinamento1[,3])) / sd(Treinamento1[,3])
  TreiNormalizado1[i,4] <- (Treinamento1[i,4] - mean(Treinamento1[,4])) / sd(Treinamento1[,4])
  TreiNormalizado1[i,5] <- (Treinamento1[i,5] - mean(Treinamento1[,5])) / sd(Treinamento1[,5])
  TreiNormalizado1[i,6] <- (Treinamento1[i,6] - mean(Treinamento1[,6])) / sd(Treinamento1[,6])
}

# Normalizacao do Teste com base na media e desvios-padrao do Treino
TesteNormalizado1 <- Teste1[,1:7]

for(i in 1:nrow(TesteNormalizado1)) {
  TesteNormalizado1[i,1] <- (Teste1[i,1] - mean(Treinamento1[,1])) / sd(Treinamento1[,1])
  TesteNormalizado1[i,2] <- (Teste1[i,2] - mean(Treinamento1[,2])) / sd(Treinamento1[,2])
  TesteNormalizado1[i,3] <- (Teste1[i,3] - mean(Treinamento1[,3])) / sd(Treinamento1[,3])
  TesteNormalizado1[i,4] <- (Teste1[i,4] - mean(Treinamento1[,4])) / sd(Treinamento1[,4])
  TesteNormalizado1[i,5] <- (Teste1[i,5] - mean(Treinamento1[,5])) / sd(Treinamento1[,5])
  TesteNormalizado1[i,6] <- (Teste1[i,6] - mean(Treinamento1[,6])) / sd(Treinamento1[,6])
}

# K-Means
TreiNormalizado1[,1:6]

wss <- (nrow(TreiNormalizado1[,1:6])-1)*sum(apply(TreiNormalizado1[,1:6],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(TreiNormalizado1[,1:6], centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Numero de Agrupamentos", ylab="Within groups sum of squares")

(kc<-kmeans(TreiNormalizado1[,1:6],6))

# Escolhido o 6 por ter pouca variacao do "joelho"
K <- 6

# Numero de iteracoes
it <- 1

# Vetor de armazens de centroides
ct1 <- matrix(0, nrow=20,ncol=6)

# Vetor de armazens de resultado das iteracoes
it1 <- matrix(0, nrow=20,ncol=6)

# (Primeiros Centroides - linha aleatoria do Treinamento)
ct1[1,1:6] <- TreiNormalizado1[7,1:6]

# Auxiliares
km_1     <- TreiNormalizado1[,1:7]
Res_km_1 <- matrix(0,nrow=186,ncol=8)

i <- 1 ;
while(it < 999){
  for(i in 1:nrow(TreiNormalizado1[,1:6])) {
    
    t1g1 <- ((km_1[i,1] - ct1[it,1])^2 + (km_1[i,2] - ct1[it,1])^2 + (km_1[i,3] - ct1[it,1])^2 + (km_1[i,4] - ct1[it,1])^2 + (km_1[i,5] - ct1[it,1])^2 + (km_1[i,6] - ct1[it,1])^2) / K
    t1g2 <- ((km_1[i,1] - ct1[it,2])^2 + (km_1[i,2] - ct1[it,2])^2 + (km_1[i,3] - ct1[it,2])^2 + (km_1[i,4] - ct1[it,2])^2 + (km_1[i,5] - ct1[it,2])^2 + (km_1[i,6] - ct1[it,2])^2) / K
    t1g3 <- ((km_1[i,1] - ct1[it,3])^2 + (km_1[i,2] - ct1[it,3])^2 + (km_1[i,3] - ct1[it,3])^2 + (km_1[i,4] - ct1[it,3])^2 + (km_1[i,5] - ct1[it,3])^2 + (km_1[i,6] - ct1[it,3])^2) / K
    t1g4 <- ((km_1[i,1] - ct1[it,4])^2 + (km_1[i,2] - ct1[it,4])^2 + (km_1[i,3] - ct1[it,4])^2 + (km_1[i,4] - ct1[it,4])^2 + (km_1[i,5] - ct1[it,4])^2 + (km_1[i,6] - ct1[it,4])^2) / K
    t1g5 <- ((km_1[i,1] - ct1[it,5])^2 + (km_1[i,2] - ct1[it,5])^2 + (km_1[i,3] - ct1[it,5])^2 + (km_1[i,4] - ct1[it,5])^2 + (km_1[i,5] - ct1[it,5])^2 + (km_1[i,6] - ct1[it,5])^2) / K
    t1g6 <- ((km_1[i,1] - ct1[it,6])^2 + (km_1[i,2] - ct1[it,6])^2 + (km_1[i,3] - ct1[it,6])^2 + (km_1[i,4] - ct1[it,6])^2 + (km_1[i,5] - ct1[it,6])^2 + (km_1[i,6] - ct1[it,6])^2) / K
    
    if (t1g1 < t1g2) {
      menor <- t1g1
      grupo <- 1
    } else {
      menor <- t1g2
      grupo <- 2 }
    
    if (menor < t1g3) {
      menor <- menor
      grupo <- grupo
    } else {
      menor <- t1g3
      grupo <- 3 }
    
    if (menor < t1g4) {
      menor <- menor
      grupo <- grupo
    } else {
      menor <- t1g4
      grupo <- 4 }
    
    if (menor < t1g5) {
      menor <- menor
      grupo <- grupo
    } else {
      menor <- t1g5
      grupo <- 5 }
    
    if (menor < t1g6) {
      menor <- menor
      grupo <- grupo
    } else {
      menor <- t1g6
      grupo <- 6 }  
    
    Res_km_1[i,1] <- km_1[i,1]
    Res_km_1[i,2] <- km_1[i,2]
    Res_km_1[i,3] <- km_1[i,3]
    Res_km_1[i,4] <- km_1[i,4]
    Res_km_1[i,5] <- km_1[i,5]
    Res_km_1[i,6] <- km_1[i,6]
    Res_km_1[i,7] <- km_1[i,7]
    Res_km_1[i,8] <- grupo
  }
  
  # Subset 1 (Dividindo por Grupo - Guardando pontos por grupo)
  it1[it,1] <- nrow(subset(Res_km_1, Res_km_1[,8] == 1))
  it1[it,2] <- nrow(subset(Res_km_1, Res_km_1[,8] == 2))
  it1[it,3] <- nrow(subset(Res_km_1, Res_km_1[,8] == 3))
  it1[it,4] <- nrow(subset(Res_km_1, Res_km_1[,8] == 4))
  it1[it,5] <- nrow(subset(Res_km_1, Res_km_1[,8] == 5))
  it1[it,6] <- nrow(subset(Res_km_1, Res_km_1[,8] == 6))
  
  # Subset 1 (Dividindo por Grupo)
  Res_km_1_G1 = subset(Res_km_1, Res_km_1[,8] == 1)
  Res_km_1_G2 = subset(Res_km_1, Res_km_1[,8] == 2)
  Res_km_1_G3 = subset(Res_km_1, Res_km_1[,8] == 3)
  Res_km_1_G4 = subset(Res_km_1, Res_km_1[,8] == 4)
  Res_km_1_G5 = subset(Res_km_1, Res_km_1[,8] == 5)
  Res_km_1_G6 = subset(Res_km_1, Res_km_1[,8] == 6)
  
  # Centroides
  ct1[it+1,1] <- (mean(Res_km_1_G1[,1]) + mean(Res_km_1_G1[,2]) + mean(Res_km_1_G1[,3]) + mean(Res_km_1_G1[,4]) + mean(Res_km_1_G1[,5]) + mean(Res_km_1_G1[,6]))/ K
  ct1[it+1,2] <- (mean(Res_km_1_G2[,1]) + mean(Res_km_1_G2[,2]) + mean(Res_km_1_G2[,3]) + mean(Res_km_1_G2[,4]) + mean(Res_km_1_G2[,5]) + mean(Res_km_1_G2[,6]))/ K
  ct1[it+1,3] <- (mean(Res_km_1_G3[,1]) + mean(Res_km_1_G3[,2]) + mean(Res_km_1_G3[,3]) + mean(Res_km_1_G3[,4]) + mean(Res_km_1_G3[,5]) + mean(Res_km_1_G3[,6]))/ K
  ct1[it+1,4] <- (mean(Res_km_1_G4[,1]) + mean(Res_km_1_G4[,2]) + mean(Res_km_1_G4[,3]) + mean(Res_km_1_G4[,4]) + mean(Res_km_1_G4[,5]) + mean(Res_km_1_G4[,6]))/ K
  ct1[it+1,5] <- (mean(Res_km_1_G5[,1]) + mean(Res_km_1_G5[,2]) + mean(Res_km_1_G5[,3]) + mean(Res_km_1_G5[,4]) + mean(Res_km_1_G5[,5]) + mean(Res_km_1_G5[,6]))/ K
  ct1[it+1,6] <- (mean(Res_km_1_G6[,1]) + mean(Res_km_1_G6[,2]) + mean(Res_km_1_G6[,3]) + mean(Res_km_1_G6[,4]) + mean(Res_km_1_G6[,5]) + mean(Res_km_1_G6[,6]))/ K
  
  # Verifica alteracoes nos centroides (ponto de parada)
  if ((mean(ct1[it+1,]) - mean(ct1[it,])) > 0.01){
    it <- it + 1
  } else {
    it <- 999
  } # if
  
} # While


# Teste
##########

# Auxiliares
km_t1     <- TesteNormalizado1[,1:7]
Res_km_t1 <- matrix(0,nrow=124,ncol=8)

for(i in 1:nrow(TesteNormalizado1[,1:6])) {
  
  # Ct1 - Ultima linha na Matriz de Centroides (Nao automatizado ainda)
  t1g1 <- ((km_t1[i,1] - ct1[7,1])^2 + (km_t1[i,2] - ct1[7,1])^2 + (km_t1[i,3] - ct1[7,1])^2 + (km_t1[i,4] - ct1[7,1])^2 + (km_t1[i,5] - ct1[7,1])^2 + (km_t1[i,6] - ct1[7,1])^2) / K
  t1g2 <- ((km_t1[i,1] - ct1[7,2])^2 + (km_t1[i,2] - ct1[7,2])^2 + (km_t1[i,3] - ct1[7,2])^2 + (km_t1[i,4] - ct1[7,2])^2 + (km_t1[i,5] - ct1[7,2])^2 + (km_t1[i,6] - ct1[7,2])^2) / K
  t1g3 <- ((km_t1[i,1] - ct1[7,3])^2 + (km_t1[i,2] - ct1[7,3])^2 + (km_t1[i,3] - ct1[7,3])^2 + (km_t1[i,4] - ct1[7,3])^2 + (km_t1[i,5] - ct1[7,3])^2 + (km_t1[i,6] - ct1[7,3])^2) / K
  t1g4 <- ((km_t1[i,1] - ct1[7,4])^2 + (km_t1[i,2] - ct1[7,4])^2 + (km_t1[i,3] - ct1[7,4])^2 + (km_t1[i,4] - ct1[7,4])^2 + (km_t1[i,5] - ct1[7,4])^2 + (km_t1[i,6] - ct1[7,4])^2) / K
  t1g5 <- ((km_t1[i,1] - ct1[7,5])^2 + (km_t1[i,2] - ct1[7,5])^2 + (km_t1[i,3] - ct1[7,5])^2 + (km_t1[i,4] - ct1[7,5])^2 + (km_t1[i,5] - ct1[7,5])^2 + (km_t1[i,6] - ct1[7,5])^2) / K
  t1g6 <- ((km_t1[i,1] - ct1[7,6])^2 + (km_t1[i,2] - ct1[7,6])^2 + (km_t1[i,3] - ct1[7,6])^2 + (km_t1[i,4] - ct1[7,6])^2 + (km_t1[i,5] - ct1[7,6])^2 + (km_t1[i,6] - ct1[7,6])^2) / K
  
  if (t1g1 < t1g2) {
    menor <- t1g1
    grupo <- 1
  } else {
    menor <- t1g2
    grupo <- 2 }
  
  if (menor < t1g3) {
    menor <- menor
    grupo <- grupo
  } else {
    menor <- t1g3
    grupo <- 3 }
  
  if (menor < t1g4) {
    menor <- menor
    grupo <- grupo
  } else {
    menor <- t1g4
    grupo <- 4 }
  
  if (menor < t1g5) {
    menor <- menor
    grupo <- grupo
  } else {
    menor <- t1g5
    grupo <- 5 }
  
  if (menor < t1g6) {
    menor <- menor
    grupo <- grupo
  } else {
    menor <- t1g6
    grupo <- 6 }  
  
  Res_km_t1[i,1] <- km_t1[i,1]
  Res_km_t1[i,2] <- km_t1[i,2]
  Res_km_t1[i,3] <- km_t1[i,3]
  Res_km_t1[i,4] <- km_t1[i,4]
  Res_km_t1[i,5] <- km_t1[i,5]
  Res_km_t1[i,6] <- km_t1[i,6]
  Res_km_t1[i,7] <- km_t1[i,7]
  Res_km_t1[i,8] <- grupo
}

# Grupos
# Vetor de armazens de resultado das iteracoes
it1_Teste <- matrix(0, nrow=1,ncol=6)

it1_Teste[1,1] <- nrow(subset(Res_km_t1, Res_km_t1[,8] == 1))
it1_Teste[1,2] <- nrow(subset(Res_km_t1, Res_km_t1[,8] == 2))
it1_Teste[1,3] <- nrow(subset(Res_km_t1, Res_km_t1[,8] == 3))
it1_Teste[1,4] <- nrow(subset(Res_km_t1, Res_km_t1[,8] == 4))
it1_Teste[1,5] <- nrow(subset(Res_km_t1, Res_km_t1[,8] == 5))
it1_Teste[1,6] <- nrow(subset(Res_km_t1, Res_km_t1[,8] == 6))

###########################################################################
################################## RBFNN ##################################
###########################################################################

# Classes (Y)
# DH 1 - NO 2 - SL 3 
Y = matrix(0, 186, 3)

i <- 1
for(i in 1:nrow(TreiNormalizado1[,1:6])) {
  if (TreiNormalizado1[i,7] == 1) {
    Y[i,1] = 1
  }
  if (TreiNormalizado1[i,7] == 2) {
    Y[i,2] = 1
  }
  if (TreiNormalizado1[i,7] == 3) {
    Y[i,3] = 1
  }
}

# Variancias
############
# G1 - Sigma
G1 = matrix(0, ncol(Res_km_1_G1[,1:6]), ncol(Res_km_1_G1[,1:6]))
diag(G1) = diag(cov(Res_km_1_G1[,1:6]))

# G2 - Sigma
G2 = matrix(0, ncol(Res_km_1_G2[,1:6]), ncol(Res_km_1_G2[,1:6]))
diag(G2) = diag(cov(Res_km_1_G2[,1:6]))

# G3 - Sigma
G3 = matrix(0, ncol(Res_km_1_G3[,1:6]), ncol(Res_km_1_G3[,1:6]))
diag(G3) = diag(cov(Res_km_1_G3[,1:6]))

# G4 - Sigma
G4 = matrix(0, ncol(Res_km_1_G4[,1:6]), ncol(Res_km_1_G4[,1:6]))
diag(G4) = diag(cov(Res_km_1_G4[,1:6]))

# G5 - Sigma
G5 = matrix(0, ncol(Res_km_1_G5[,1:6]), ncol(Res_km_1_G5[,1:6]))
diag(G5) = diag(cov(Res_km_1_G5[,1:6]))

# G6 - Sigma
G6 = matrix(0, ncol(Res_km_1_G6[,1:6]), ncol(Res_km_1_G6[,1:6]))
diag(G6) = diag(cov(Res_km_1_G6[,1:6]))

# Variancias
G1; G2; G3; G4; G5; G6

# RBFNN
TreiNormalizado1
TesteNormalizado1
Y # Classes

K

# Grupos
Res_km_1_G1
Res_km_1_G2
Res_km_1_G3
Res_km_1_G4
Res_km_1_G5
Res_km_1_G6

# Matriz de Medias
MD_G1 = matrix(c(mean(Res_km_1_G1[,1]), mean(Res_km_1_G1[,2]), mean(Res_km_1_G1[,3]), mean(Res_km_1_G1[,4]), mean(Res_km_1_G1[,5]), mean(Res_km_1_G1[,6]), 1),1,7,1)
MD_G2 = matrix(c(mean(Res_km_1_G2[,1]), mean(Res_km_1_G2[,2]), mean(Res_km_1_G2[,3]), mean(Res_km_1_G2[,4]), mean(Res_km_1_G2[,5]), mean(Res_km_1_G2[,6]), 2),1,7,1)
MD_G3 = matrix(c(mean(Res_km_1_G3[,1]), mean(Res_km_1_G3[,2]), mean(Res_km_1_G3[,3]), mean(Res_km_1_G3[,4]), mean(Res_km_1_G3[,5]), mean(Res_km_1_G3[,6]), 3),1,7,1)
MD_G4 = matrix(c(mean(Res_km_1_G4[,1]), mean(Res_km_1_G4[,2]), mean(Res_km_1_G4[,3]), mean(Res_km_1_G4[,4]), mean(Res_km_1_G4[,5]), mean(Res_km_1_G4[,6]), 4),1,7,1)
MD_G5 = matrix(c(mean(Res_km_1_G5[,1]), mean(Res_km_1_G5[,2]), mean(Res_km_1_G5[,3]), mean(Res_km_1_G5[,4]), mean(Res_km_1_G5[,5]), mean(Res_km_1_G5[,6]), 5),1,7,1)
MD_G6 = matrix(c(mean(Res_km_1_G6[,1]), mean(Res_km_1_G6[,2]), mean(Res_km_1_G6[,3]), mean(Res_km_1_G6[,4]), mean(Res_km_1_G6[,5]), mean(Res_km_1_G6[,6]), 6),1,7,1)

# Calculando o H
H1 <- matrix(1, nrow(TreiNormalizado1),K+1)
#H2 <- matrix(1, nrow(TreiNormalizado2),K+1)
#H3 <- matrix(1, nrow(TreiNormalizado3),K+1)
#H4 <- matrix(1, nrow(TreiNormalizado4),K+1)
#H5 <- matrix(1, nrow(TreiNormalizado5),K+1)
#H6 <- matrix(1, nrow(TreiNormalizado6),K+1)
#H7 <- matrix(1, nrow(TreiNormalizado7),K+1)
#H8 <- matrix(1, nrow(TreiNormalizado8),K+1)
#H9 <- matrix(1, nrow(TreiNormalizado9),K+1)
#H10 <- matrix(1, nrow(TreiNormalizado10),K+1)

i <- 1
for(i in 1:nrow(TreiNormalizado1[,1:6])){
  # Maralanobis
  h1 <- t(t(matrix(c(TreiNormalizado1[i,1:6] - MD_G1[,1:6]), 1,6,1)))%*%solve(G1)%*%t(matrix(c(TreiNormalizado1[i,1:6] - MD_G1[,1:6]), 1,6,1))
  h2 <- t(t(matrix(c(TreiNormalizado1[i,1:6] - MD_G2[,1:6]), 1,6,1)))%*%solve(G2)%*%t(matrix(c(TreiNormalizado1[i,1:6] - MD_G2[,1:6]), 1,6,1))
  h3 <- t(t(matrix(c(TreiNormalizado1[i,1:6] - MD_G3[,1:6]), 1,6,1)))%*%solve(G3)%*%t(matrix(c(TreiNormalizado1[i,1:6] - MD_G3[,1:6]), 1,6,1))
  h4 <- t(t(matrix(c(TreiNormalizado1[i,1:6] - MD_G4[,1:6]), 1,6,1)))%*%solve(G4)%*%t(matrix(c(TreiNormalizado1[i,1:6] - MD_G4[,1:6]), 1,6,1))
  h5 <- t(t(matrix(c(TreiNormalizado1[i,1:6] - MD_G5[,1:6]), 1,6,1)))%*%solve(G5)%*%t(matrix(c(TreiNormalizado1[i,1:6] - MD_G5[,1:6]), 1,6,1))
  h6 <- t(t(matrix(c(TreiNormalizado1[i,1:6] - MD_G6[,1:6]), 1,6,1)))%*%solve(G6)%*%t(matrix(c(TreiNormalizado1[i,1:6] - MD_G6[,1:6]), 1,6,1))
  
  H1[i,2] = exp(1)^-h1
  H1[i,3] = exp(1)^-h2
  H1[i,4] = exp(1)^-h3
  H1[i,5] = exp(1)^-h4
  H1[i,6] = exp(1)^-h5
  H1[i,7] = exp(1)^-h6
  
  #H1[i,2] = h1
  #H1[i,3] = h2
  #H1[i,4] = h3
  #H1[i,5] = h4
  #H1[i,6] = h5
  #H1[i,7] = h6
  
  #print(h1)
  #print(h2)
  #print(h3)
  #print(h4)
  #print(h5)
  #print(h6)
  
}

# Calculando o w
w1 = solve(t(H1)%*%H1)%*%t(H1)%*%Y

#########
# TESTE #
#########

HTeste1  <- matrix(1, nrow(TesteNormalizado1) ,K+1)
#HTeste2  <- matrix(1, nrow(TesteNormalizado2) ,K+1)
#HTeste3  <- matrix(1, nrow(TesteNormalizado3) ,K+1)
#HTeste4  <- matrix(1, nrow(TesteNormalizado4) ,K+1)
#HTeste5  <- matrix(1, nrow(TesteNormalizado5) ,K+1)
#HTeste6  <- matrix(1, nrow(TesteNormalizado6) ,K+1)
#HTeste7  <- matrix(1, nrow(TesteNormalizado7) ,K+1)
#HTeste8  <- matrix(1, nrow(TesteNormalizado8) ,K+1)
#HTeste9  <- matrix(1, nrow(TesteNormalizado9) ,K+1)
#HTeste10 <- matrix(1, nrow(TesteNormalizado10),K+1)

i <- 1
for(i in 1:nrow(TesteNormalizado1[,1:6])){
  h1 <- t(t(matrix(c(TesteNormalizado1[i,1:6] - MD_G1[,1:6]), 1,6,1)))%*%solve(G1)%*%t(matrix(c(TesteNormalizado1[i,1:6] - MD_G1[,1:6]), 1,6,1))
  h2 <- t(t(matrix(c(TesteNormalizado1[i,1:6] - MD_G2[,1:6]), 1,6,1)))%*%solve(G2)%*%t(matrix(c(TesteNormalizado1[i,1:6] - MD_G2[,1:6]), 1,6,1))
  h3 <- t(t(matrix(c(TesteNormalizado1[i,1:6] - MD_G3[,1:6]), 1,6,1)))%*%solve(G3)%*%t(matrix(c(TesteNormalizado1[i,1:6] - MD_G3[,1:6]), 1,6,1))
  h4 <- t(t(matrix(c(TesteNormalizado1[i,1:6] - MD_G4[,1:6]), 1,6,1)))%*%solve(G4)%*%t(matrix(c(TesteNormalizado1[i,1:6] - MD_G4[,1:6]), 1,6,1))
  h5 <- t(t(matrix(c(TesteNormalizado1[i,1:6] - MD_G5[,1:6]), 1,6,1)))%*%solve(G5)%*%t(matrix(c(TesteNormalizado1[i,1:6] - MD_G5[,1:6]), 1,6,1))
  h6 <- t(t(matrix(c(TesteNormalizado1[i,1:6] - MD_G6[,1:6]), 1,6,1)))%*%solve(G6)%*%t(matrix(c(TesteNormalizado1[i,1:6] - MD_G6[,1:6]), 1,6,1))
  
  HTeste1[i,2] = exp(1)^-h1
  HTeste1[i,3] = exp(1)^-h2
  HTeste1[i,4] = exp(1)^-h3
  HTeste1[i,5] = exp(1)^-h4
  HTeste1[i,6] = exp(1)^-h5
  HTeste1[i,7] = exp(1)^-h6
}

##########
# wTh +w0
#wt1 = t(w1)%*%t(HTeste1) + w1[1,]
#t(wt1)

HTeste1
w1
# DH 1 - NO 2 - SL 3

HTeste1[1,1:7]%*%t(t(w1[,1]))
HTeste1[1,1:7]%*%t(t(w1[,2]))
HTeste1[1,1:7]%*%t(t(w1[,3]))

RBFNN1 <- matrix(nrow=124,ncol=2) # V1 - Maior Valor e V2 - Classe 

i <- 1
for(i in 1:nrow(HTeste1)) {
  
  tRBF_DH <- HTeste1[i,]%*%t(t(w1[,1]))
  tRBF_NO <- HTeste1[i,]%*%t(t(w1[,2]))
  tRBF_SL <- HTeste1[i,]%*%t(t(w1[,3]))
  
  if (tRBF_DH  > tRBF_NO) {
    maior   <- tRBF_DH
    classe  <- 1 # DH
  } else {
    maior   <- tRBF_NO
    classe  <- 2 } # NO  
  
  if (maior   > tRBF_SL) {
    maior   <- maior
    classe  <- classe
  } else {
    maior   <- tRBF_SL
    classe  <- 3 } # SL
  
  RBFNN1[i,1] <- maior
  RBFNN1[i,2] <- classe
}

###################
# Matriz de Confusao

MatConfusaoRBF1 <- matrix(nrow=3,ncol=3)
aux11 <- 0; aux12 <- 0; aux13 <- 0; aux21 <- 0; aux22 <- 0; aux23 <- 0; aux31 <- 0; aux32 <- 0; aux33 <- 0

for(i in 1:nrow(TesteNormalizado1)) {
  if (TesteNormalizado1[i,7] == 1 && RBFNN1[i,2] == 1) { #DH DH
    aux11 <- aux11 + 1
    MatConfusaoRBF1[1,1] <- aux11 }
  if (TesteNormalizado1[i,7] == 1 && RBFNN1[i,2] == 2) { #DH NO
    aux12 <- aux12 + 1
    MatConfusaoRBF1[1,2] <- aux12 }
  if (TesteNormalizado1[i,7] == 1 && RBFNN1[i,2] == 3) { #DH SL
    aux13 <- aux13 + 1
    MatConfusaoRBF1[1,3] <- aux13 }
  
  if (TesteNormalizado1[i,7] == 2 && RBFNN1[i,2] == 1) { #NO DH
    aux21 <- aux21 + 1
    MatConfusaoRBF1[2,1] <- aux21 }
  if (TesteNormalizado1[i,7] == 2 && RBFNN1[i,2] == 2) { #NO NO
    aux22 <- aux22 + 1
    MatConfusaoRBF1[2,2] <- aux22 }
  if (TesteNormalizado1[i,7] == 2 && RBFNN1[i,2] == 3) { #NO SL
    aux23 <- aux23 + 1
    MatConfusaoRBF1[2,3] <- aux23 }
  
  if (TesteNormalizado1[i,7] == 3 && RBFNN1[i,2] == 1) { #SL DH
    aux31 <- aux31 + 1
    MatConfusaoRBF1[3,1] <- aux31 }
  if (TesteNormalizado1[i,7] == 3 && RBFNN1[i,2] == 2) { #SL NO
    aux32 <- aux32 + 1
    MatConfusaoRBF1[3,2] <- aux32 }
  if (TesteNormalizado1[i,7] == 3 && RBFNN1[i,2] == 3) { #SL SL
    aux33 <- aux33 + 1
    MatConfusaoRBF1[3,3] <- aux33 }
}

# DH 1 - NO 2 - SL 3
MatConfusaoRBF1

################
# Validacoes

#p1 = t(t(matrix(c(Treinamento1[1,1:6] - MD_G1[1,1:6]), 1,6,1)))
#p2 = solve(G1)
#p3 = t(matrix(c(Treinamento1[1,1:6] - MD_G1[1,1:6]), 1,6,1))
#ml = p1%*%p2%*%p3
#mahalanobis(Treinamento1[1,1:6], MD_G1[1,1:6], G1)


###################
# Proximas versoes

# + Automatizacao (calcular os 10 DataSets automaticamente)
# + Calculo do SSE
# + Automatizar o teste (pegar ultima linha da matriz de centroides)