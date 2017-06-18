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

# (Primeiros Centroides - linha aleatória do Treinamento)
ct1[1,1:6] <- TreiNormalizado1[7,1:6]

# Auxiliares
km_1     <- TreiNormalizado1[,1:6]
Res_km_1 <- matrix(0,nrow=186,ncol=8)

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
    Res_km_1[i,7] <- menor
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
# Auxiliares
km_t1     <- TesteNormalizado1[,1:7]
Res_km_t1 <- matrix(0,nrow=124,ncol=8)

for(i in 1:nrow(TesteNormalizado1[,1:6])) {
  
  # Ct1 - Ultima linha na Matriz de Centroides (Não automatizado ainda)
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
  Res_km_t1[i,7] <- menor
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

######################################################
# Não realizado o procedimentos nos demais DataSet´s #
######################################################

###################
# Proximas versões
# + Automatização (calcular os 10 DataSet´s automaticamente)
# + Calculo do SSE
# + Automatizar o teste (pegar ultima linha da matriz de centroides)