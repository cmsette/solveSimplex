### Functions to generate eigenvalues & run test stability (5-players, continuous time replicator dynamics)
# Calculates eigenvalues of scaled Jacobian matrix, classifes equilibrium by combination of eigenvalues
# Sink (all -), Source (all +), Saddle (mix of - and +): Saddle 1-3 denotes number of positive eigen values
# NaN(?) (any is NaN, divided by 0), Inconclusive (2nd smallest is 0), Validation error (test function validation steps were > 1e -14, i.e. not 0)
# No eq (no equilibrium internal to simplex)
cont5Classify<-function(seq, W, zero){
  if(is.na(seq[6])==T){type<-c("Validation error")}											#validation trouble
  else if(seq[6]==0){type<-c("No eq")}
  else{W1<-W[1]; W2<-W[2]; W3<-W[3]; W4<-W[4]; W5<-W[5]; W6<-W[6]; W7<-W[7]; W8<-W[8]; W9<-W[9]; W10<-W[10]; W11<-W[11]; W12<-W[12]; 		W13<-W[13]; W14<-W[14]; W15<-W[15]; W16<-W[16]; W17<-W[17]; W18<-W[18]; W19<-W[19]; W20<-W[20]; W21<-W[21]; W22<-W[22]; 			W23<-W[23]; W24<-W[24]; W25<-W[25]
  r <- seq[1]; p <- seq[2]; s <- seq[3]; l <- seq[4]; k <- seq[5]
  P <- matrix( c( (4/5),(-1/5),(-1/5),(-1/5),(-1/5), (-1/5),(4/5),(-1/5),(-1/5),(-1/5), (-1/5),(-1/5),(4/5),(-1/5),(-1/5), 				(-1/5),(-1/5),(-1/5),(4/5),(-1/5), (-1/5),(-1/5),(-1/5),(-1/5),(4/5) ), ncol=5, byrow=T)
  #Calculates Jacobian matrix expressions for r: ∆r expression derived by r, p, s, l, k respectively
  Jac1<-as.expression(D(expression(r*((r*W1)+(p*W2)+(s*W3)+(l*W4)+(k*W5)) - r*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 				+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "r"))
  Jac2<-as.expression(D(expression(r*((r*W1)+(p*W2)+(s*W3)+(l*W4)+(k*W5)) - r*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 				+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "p"))
  Jac3<-as.expression(D(expression(r*((r*W1)+(p*W2)+(s*W3)+(l*W4)+(k*W5)) - r*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 				+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "s"))
  Jac4<-as.expression(D(expression(r*((r*W1)+(p*W2)+(s*W3)+(l*W4)+(k*W5)) - r*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 				+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "l"))
  Jac5<-as.expression(D(expression(r*((r*W1)+(p*W2)+(s*W3)+(l*W4)+(k*W5)) - r*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 				+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "k"))
  #Calculates Jacobian matrix expressions for p: ∆p expression derived by r, p, s, l, k respectively
  Jac6<-as.expression(D(expression(p*((r*W6)+(p*W7)+(s*W8)+(l*W9)+(k*W10)) - p*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 				+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "r"))
  Jac7<-as.expression(D(expression(p*((r*W6)+(p*W7)+(s*W8)+(l*W9)+(k*W10)) - p*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 				+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "p"))
  Jac8<-as.expression(D(expression(p*((r*W6)+(p*W7)+(s*W8)+(l*W9)+(k*W10)) - p*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 				+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "s"))
  Jac9<-as.expression(D(expression(p*((r*W6)+(p*W7)+(s*W8)+(l*W9)+(k*W10)) - p*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 				+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "l"))
  Jac10<-as.expression(D(expression(p*((r*W6)+(p*W7)+(s*W8)+(l*W9)+(k*W10)) - p*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 				+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "k"))
  #Calculates Jacobian matrix expressions for s: ∆s expression derived by r, p, s, l, k respectively
  Jac11<-as.expression(D(expression(s*((r*W11)+(p*W12)+(s*W13)+(l*W14)+(k*W15)) - s*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 			+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "r"))
  Jac12<-as.expression(D(expression(s*((r*W11)+(p*W12)+(s*W13)+(l*W14)+(k*W15)) - s*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 			+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "p"))
  Jac13<-as.expression(D(expression(s*((r*W11)+(p*W12)+(s*W13)+(l*W14)+(k*W15)) - s*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 			+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "s"))
  Jac14<-as.expression(D(expression(s*((r*W11)+(p*W12)+(s*W13)+(l*W14)+(k*W15)) - s*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 			+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "l"))
  Jac15<-as.expression(D(expression(s*((r*W11)+(p*W12)+(s*W13)+(l*W14)+(k*W15)) - s*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 			+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "k"))
  #Calculates Jacobian matrix expressions for l: ∆l expression derived by r, p, s, l, k respectively
  Jac16<-as.expression(D(expression(l*((r*W16)+(p*W17)+(s*W18)+(l*W19)+(k*W20)) - l*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 			+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "r"))
  Jac17<-as.expression(D(expression(l*((r*W16)+(p*W17)+(s*W18)+(l*W19)+(k*W20)) - l*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 			+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "p"))
  Jac18<-as.expression(D(expression(l*((r*W16)+(p*W17)+(s*W18)+(l*W19)+(k*W20)) - l*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 			+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "s"))
  Jac19<-as.expression(D(expression(l*((r*W16)+(p*W17)+(s*W18)+(l*W19)+(k*W20)) - l*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 			+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "l"))
  Jac20<-as.expression(D(expression(l*((r*W16)+(p*W17)+(s*W18)+(l*W19)+(k*W20)) - l*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 			+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "k"))
  #Calculates Jacobian matrix expressions for k: ∆k expression derived by r, p, s, l, k respectively
  Jac21<-as.expression(D(expression(k*((r*W21)+(p*W22)+(s*W23)+(l*W24)+(k*W25)) - k*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 			+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "r"))
  Jac22<-as.expression(D(expression(k*((r*W21)+(p*W22)+(s*W23)+(l*W24)+(k*W25)) - k*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 			+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "p"))
  Jac23<-as.expression(D(expression(k*((r*W21)+(p*W22)+(s*W23)+(l*W24)+(k*W25)) - k*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 			+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "s"))
  Jac24<-as.expression(D(expression(k*((r*W21)+(p*W22)+(s*W23)+(l*W24)+(k*W25)) - k*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 			+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "l"))
  Jac25<-as.expression(D(expression(k*((r*W21)+(p*W22)+(s*W23)+(l*W24)+(k*W25)) - k*( r*((r*W1)+(p*W6)+(s*W11)+(l*W16)+(k*W21)) 			+ p*((r*W2)+(p*W7)+(s*W12)+(l*W17)+(k*W22)) + s*((r*W3)+(p*W8)+(s*W13)+(l*W18)+(k*W23)) 											+ l*((r*W4)+(p*W9)+(s*W14)+(l*W19)+(k*W24)) + k*((r*W5)+(p*W10)+(s*W15)+(l*W20)+(k*W25)) ) ), "k"))
  #Inputs payoff matrix and eq values into expression, saves solutions as Jacobian matrix (Jac) 
  e1 <- environment()
  Jac<-matrix(nrow=5, ncol=5, byrow=T)
  Jac[1,1]<-eval(Jac1, env=e1); Jac[1,2]<-eval(Jac2, env=e1); Jac[1,3]<-eval(Jac3, env=e1); Jac[1,4]<-eval(Jac4, env=e1); 				Jac[1,5]<-eval(Jac5, env=e1)
  Jac[2,1]<-eval(Jac6, env=e1); Jac[2,2]<-eval(Jac7, env=e1); Jac[2,3]<-eval(Jac8, env=e1); Jac[2,4]<-eval(Jac9, env=e1); 				Jac[2,5]<-eval(Jac10, env=e1)
  Jac[3,1]<-eval(Jac11, env=e1); Jac[3,2]<-eval(Jac12, env=e1); Jac[3,3]<-eval(Jac13, env=e1); Jac[3,4]<-eval(Jac14, env=e1); 			Jac[3,5]<-eval(Jac15, env=e1)
  Jac[4,1]<-eval(Jac16, env=e1); Jac[4,2]<-eval(Jac17, env=e1); Jac[4,3]<-eval(Jac18, env=e1); Jac[4,4]<-eval(Jac19, env=e1); 			Jac[4,5]<-eval(Jac20, env=e1)
  Jac[5,1]<-eval(Jac21, env=e1); Jac[5,2]<-eval(Jac22, env=e1); Jac[5,3]<-eval(Jac23, env=e1); Jac[5,4]<-eval(Jac24, env=e1); 			Jac[5,5]<-eval(Jac25, env=e1)
  #Calculates eigenvectors for projection matrix, returns values
  M <- Jac %*% P																					#Multiplies Jac & Identity
  if(isTRUE(any(M=="NaN"))==T){type<-c("NaN")} 
  else{EigM <- eigen(M, symmetric=F)
  if(abs(Re(EigM$values[5])) < zero){	
    EigM$values[1:4] <- sort(Re(EigM$values)[1:4], decreasing=T)
    if(abs(Re(EigM$values[1])) < zero){type<-c("Inconclusive")}
    else if(Re(EigM$values[1]) < -zero){type<-c("Sink")}
    else if(Re(EigM$values[4]) > zero){type<-c("Source")}	
    else{signs<-ifelse(Re(EigM$values[1:4]) > zero, 1, 0)
    if(sum(signs) == 1){type<-c("Saddle1")}; if(sum(signs) == 2){type<-c("Saddle2")}; if(sum(signs) == 3){type<-									c("Saddle3")}}}		
  else{type<-c("Validation error2")}		
  }
  }
  return(type)
}	