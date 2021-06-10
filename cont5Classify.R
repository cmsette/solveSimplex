### Functions to generate eigenvalues & run test stability (5-players, continuous time replicator dynamics)
# Calculates eigenvalues of scaled Jacobian matrix, classifes equilibrium by combination of eigenvalues
# Sink (all -), Source (all +), Saddle (mix of - and +): Saddle 1-3 denotes number of positive eigen values
# NaN(?) (any is NaN, divided by 0), Inconclusive (2nd smallest is 0), Validation error (test function validation steps were > 1e -14, i.e. not 0)
# No eq (no equilibrium internal to simplex)
# Error1: failed equilibrium calculation validation step in Test functions (may be resolved by increasing value of "zero" var)
# Error2: projection matrix cannot be calculated
# Error3: no zero eigenvalue calculated for projection matrix
cont5Classifynew <- function(seq, W, zero){
  if(is.na(seq[6])==T){type<-c("Error1")}
  else if(seq[6]==0){type<-c("No eq")}
  else{
    W1<-W[1,1]; W2<-W[1,2]; W3<-W[1,3]; W4<-W[1,4]; W5<-W[1,5]; W6<-W[2,1]; W7<-W[2,2]; W8<-W[2,3]; W9<-W[2,4]; W10<-W[2,5]
    W11<-W[3,1]; W12<-W[3,2]; W13<-W[3,3]; W14<-W[3,4]; W15<-W[3,5]; W16<-W[4,1]; W17<-W[4,2]; W18<-W[4,3]; W19<-W[4,4]; W20<-W[4,5]
    W21<-W[5,1]; W22<-W[5,2];W23<-W[5,3]; W24<-W[5,4]; W25<-W[5,5]
    r <- seq[1]; p <- seq[2]; s <- seq[3]; l <- seq[4]; k <- seq[5]
    P <- matrix( c( (4/5),(-1/5),(-1/5),(-1/5),(-1/5), (-1/5),(4/5),(-1/5),(-1/5),(-1/5), (-1/5),(-1/5),(4/5),(-1/5),(-1/5), 				(-1/5),(-1/5),(-1/5),(4/5),(-1/5), (-1/5),(-1/5),(-1/5),(-1/5),(4/5) ), ncol=5, byrow=T)
    # Calculates Jacobian matrix expressions for r: ∆r expression derived by r, p, s, l, k respectively
    Jac1 <- 2*r*W1 + p*W2 + s*W3 + l*W4 + k*W5 - (r*(3*r*W1 + p*W2 + 2*p*W6 + s*W3 + 2*s*W11 + l*W4 + 2*l*W16 + k*W5 + 2*k*W21) + p*(r*W2 + p*W7 + s*W12 + l*W17 + k*W22) + s*(r*W3 + p*W8 + s*W13 + l*W18 + k*W23) + l*(r*W4 + p*W9 +  s*W14 + l*W19 + k*W24) + k*(r*W5 + p*W10 + s*W15 + l*W20 + k*W25))
    Jac2 <- r*W2 - r*(r*W2 + r*W6 + 2*p*W7 + s*W8 + s*W12 + l*W9 + l*W17 + k*W10 +k*W22)
    Jac3 <- r*W3 - r*(r*W3 + r*W11 + p*W8 + p*W12 + 2*s*W13 + l*W14 + l*W18 + k*W15 + k*W23)
    Jac4 <- r*W4 - r*(r*W4 + r*W16 + p*W9 + p*W17 + s*W14 + s*W18 + 2*l*W19 + k*W20 + k*W24)
    Jac5 <- r*W5 - r*r*W5 + (r*W21 + p*W10 + p*W22 + s*W15 + s*W23 + l*W20 + l*W24 + 2*k*W25)
    # Calculates Jacobian matrix expressions for p: ∆p expression derived by r, p, s, l, k respectively
    Jac6 <- p*W6 - p*(2*r*W1 + p*W2 + p*W6 + s*W3 + s*W11 + l*W4 + l*W16 + k*W5 + k*W21)
    Jac7 <- r*W6 + p*W7 + s*W8 + l*W9 + k*W10 + p*W7 - (r*(r*W1 + p*W6 + s*W11 + l*W16 + k*W21) + p*(2*r*W2 + r*W6 + 3*p*W7 + s*W8 + 2*s*W12 + l*W9 + 2*l*W17 + k*W10 + 2*k*W22) + s*(r*W3 + p*W8 + s*W13 + l*W18 + k*W23) + l*(r*W4 + p*W9 + s*W14 + l*W19 + k*W24) + k*(r*W5 + p*W10 + s*W15 + l*W20 + k*W25))
    Jac8 <- p*W8 - p*(r*W3 + r*W11 + p*W8 + p*W12 + 2*s*W13 + l*W14 + l*W18 + k*W15 + k*W23)
    Jac9 <- p*W9 - p*(r*W4 + r*W16 + p*W9 + p*W17 + s*W14 + s*W18 + 2*l*W19 + k*W20 + k*W24)
    Jac10 <- p*W10 - p*(r*W5 + r*W21 + p*W10 + p*W22 + s*W15 + s*W23 + l*W20 + l*W24 + 2*k*W25)
    # Calculates Jacobian matrix expressions for s: ∆s expression derived by r, p, s, l, k respectively
    Jac11 <- s*W11 - s*(2*r*W1 + s*W3 + p*W2 + p*W6 + s*W11 + l*W4 + l*W16 + k*W5 + k*W21)
    Jac12 <- s*W12 - s*(r*W2 + r*W6 + 2*p*W7 + s*W8 + s*W12 + l*W9 + l*W17 + k*W10 + k*W22)
    Jac13 <- r*W11 + p*W12 + 2*s*W13 + l*W14 + k*W15 - (r*(r*W1 + p*W6 + s*W11 + l*W16 + k*W21) + p*(r*W2 + p*W7 + s*W12 + l*W17 + k*W22) + s*(2*r*W3 + r*W11 + 2*p*W8 + p*W12 + 3*s*W13 + l*W14 + 2*l*W18 + k*W15 + 2*k*W23) + l*(r*W4 + p*W9 + s*W14 + l*W19 + k*W24) + k*(r*W5 + p*W10 + s*W15 + l*W20 + k*W25))
    Jac14 <- s*W14 - s*(r*W4 + r*W16 + p*W9 + p*W17 + s*W14 + s*W18 + 2*l*W19 + k*W20 + k*W24)
    Jac15 <- s*W15 - s*(r*W5 + r*W21 + p*W10 + p*W22 + s*W15 + s*W23 + l*W20 + l*W24 + 2*k*W25)
    # Calculates Jacobian matrix expressions for l: ∆l expression derived by r, p, s, l, k respectively
    Jac16 <- l*W16 - l*(2*r*W1 + p*W2 + p*W6 + s*W3 + s*W11 + l*W4 + l*W16 + k*W5 + k*W21)
    Jac17 <- l*W17 - l*(r*W2 + r*W6 + 2*p*W7 + s*W8 + s*W12 + l*W9 + l*W17 + k*W10 + k*W22)
    Jac18 <- l*W18 - l*(r*W3 + r*W11 + p*W8 + p*W12 + 2*s*W13 + l*W14 + l*W18 + k*W15 + k*W23)
    Jac19 <- r*W16 + p*W17 + s*W18 + 2*l*W19 + k*W20 + - (r*(r*W1 + p*W6 + s*W11 + l*W16 + k*W21) + p*(r*W2 + p*W7 + s*W12 + l*W17 + k*W22) + s*(r*W3 + p*W8 + s*W13 + l*W18 + k*W23) + l*(2*r*W4 + r*W16 + 2*p*W9 + p*W17 + 2*s*W14 + s*W18 + 3*l*W19 + k*W20 + 2*k*W24) + k*(r*W5 + p*W10 + s*W15 + l*W20 + k*W25))
    Jac20 <- l*W20 - l*(r*W5 + r*W21 + p*W10 + p*W22 + s*W15 + s*W23 + l*W20 + l*W24 + 2*k*W25)
    # Calculates Jacobian matrix expressions for k: ∆k expression derived by r, p, s, l, k respectively
    Jac21 <- k*W21 - k*(2*r*W1 + p*W2 + p*W6 + s*W3 + s*W11 + l*W4 + l*W16 + k*W5 + k*W21)
    Jac22 <- k*W22 - k*(r*W2 + r*W6 + 2*p*W7 + s*W8 + s*W12 + l*W9  + l*W17+ k*W10 + k*W22)
    Jac23 <- k*W23 - k*(r*W3 + r*W11 + p*W8 + p*W12 + 2*s*W13 + k*W15 + l*W14 + l*W18 + k*W23)
    Jac24 <- k*W24 - k*(r*W4 + r*W16 + p*W17 + p*W9 + s*W14 + s*W18 + 2*l*W19 + k*W20 + k*W24)
    Jac25 <- r*W21 + p*W22 + s*W23 + l*W24 + 2*k*W25 - (r*(r*W1 + p*W6 + s*W11 + l*W16 + k*W21) + p*(r*W2 + p*W7 + s*W12 + l*W17 + k*W22) + s*(r*W3 + p*W8 + s*W13 + l*W18 + k*W23) + l*(r*W4 + p*W9 + s*W14 + l*W19 + k*W24) + k*(2*r*W5 + r*W21 + 2*p*W10 + p*W22 + 2*s*W15 + s*W23 + 2*l*W20 + l*W24 + 3*k*W25))
    Jac <- matrix(c(Jac1, Jac2, Jac3, Jac4, Jac5, Jac6, Jac7, Jac8, Jac9, Jac10, Jac11, Jac12, Jac13, Jac14, Jac15, Jac16, Jac17, Jac18, Jac19, Jac20, Jac21, Jac22, Jac23, Jac24, Jac25), nrow=5, ncol=4, byrow=T)
    # Calculates eigenvectors for projection matrix, returns values
    M <- Jac %*% P; print(Jac)
    if(isTRUE(any(M=="NaN"))==T){type<-c("Error2")} 
    else{EigM <- eigen(M, symmetric=F)
    if(abs(Re(EigM$values[5])) < zero){	
      EigM$values[1:4] <- sort(Re(EigM$values)[1:4], decreasing=T)
      if(abs(Re(EigM$values[1])) < zero){type<-c("Inconclusive")}
      else if(Re(EigM$values[1]) < -zero){type<-c("Sink")}
      else if(Re(EigM$values[4]) > zero){type<-c("Source")}	
      else{signs<-ifelse(Re(EigM$values[1:4]) > zero, 1, 0)
      if(sum(signs) == 1){type<-c("Saddle1")}; if(sum(signs) == 2){type<-c("Saddle2")}; if(sum(signs) == 3){type<-									c("Saddle3")}}}		
    else{type<-c("Error3")}		
    }
  }; return(type)
}	