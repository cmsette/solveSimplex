### Functions to generate eigenvalues & run test stability (4-players, continuous time replicator dynamics)
# Calculates eigenvalues of scaled Jacobian matrix, classifes equilibrium by combination of eigenvalues
# Sink (all -), Source (all +), Saddle (mix of - and +): Saddle 1-3 denotes number of positive eigen values
# NaN(?) (any is NaN, divided by 0), Inconclusive (2nd smallest is 0), Validation error (test function validation steps were > 1e -14, i.e. not 0)
# No eq (no equilibrium internal to simplex)
cont4Classify<-function(seq, W, zero){
  if(is.na(seq[5])==T){type<-c("Validation error")}										#validation trouble
  else if(seq[5]==0){type<-c("No eq")}
  else{
    W1<-W[1]; W2<-W[2]; W3<-W[3]; W4<-W[4]; W5<-W[5]; W6<-W[6]; W7<-W[7]; W8<-W[8]; W9<-W[9]; W10<-W[10]; W11<-W[11]; W12<-W[12]; 			W13<-W[13]; W14<-W[14]; W15<-W[15]; W16<-W[16]
    r <- seq[1]; p <- seq[2]; s <- seq[3]; l <- seq[4]														#Defines eq
    P <- matrix( c( (3/4),(-1/4),(-1/4),(-1/4), (-1/4),(3/4),(-1/4),(-1/4), (-1/4),(-1/4),(3/4),(-1/4), 									(-1/4),(-1/4),(-1/4),(3/4) ), ncol=4, byrow=T)														#Identity matrix
    #Calculates Jacobian matrix expressions for r: ∆r expression derived by r, p, s, l respectively
    Jac1<-as.expression(D(expression(r*((r*W1)+(p*W2)+(s*W3)+(l*W4)) - r*( r*((r*W1)+(p*W5)+(s*W9)+(l*W13)) 									+ p*((r*W2)+(p*W6)+(s*W10)+(l*W14)) + s*((r*W3)+(p*W7)+(s*W11)+(l*W15)) + l*((r*W4)+(p*W8)+(s*W12)+(l*W16)) ) ), "r"))
    Jac2<-as.expression(D(expression(r*((r*W1)+(p*W2)+(s*W3)+(l*W4)) - r*( r*((r*W1)+(p*W5)+(s*W9)+(l*W13)) 									+ p*((r*W2)+(p*W6)+(s*W10)+(l*W14)) + s*((r*W3)+(p*W7)+(s*W11)+(l*W15)) + l*((r*W4)+(p*W8)+(s*W12)+(l*W16)) ) ), "p"))
    Jac3<-as.expression(D(expression(r*((r*W1)+(p*W2)+(s*W3)+(l*W4)) - r*( r*((r*W1)+(p*W5)+(s*W9)+(l*W13)) 									+ p*((r*W2)+(p*W6)+(s*W10)+(l*W14)) + s*((r*W3)+(p*W7)+(s*W11)+(l*W15)) + l*((r*W4)+(p*W8)+(s*W12)+(l*W16)) ) ), "s"))
    Jac4<-as.expression(D(expression(r*((r*W1)+(p*W2)+(s*W3)+(l*W4)) - r*( r*((r*W1)+(p*W5)+(s*W9)+(l*W13)) 									+ p*((r*W2)+(p*W6)+(s*W10)+(l*W14)) + s*((r*W3)+(p*W7)+(s*W11)+(l*W15)) + l*((r*W4)+(p*W8)+(s*W12)+(l*W16)) ) ), "l"))
    #Calculates Jacobian matrix expressions for p: ∆p expression derived by r, p, s, l respectively
    Jac5<-as.expression(D(expression(p*((r*W5)+(p*W6)+(s*W7)+(l*W8)) - p*( r*((r*W1)+(p*W5)+(s*W9)+(l*W13)) 									+ p*((r*W2)+(p*W6)+(s*W10)+(l*W14)) + s*((r*W3)+(p*W7)+(s*W11)+(l*W15)) + l*((r*W4)+(p*W8)+(s*W12)+(l*W16)) ) ), "r"))	
    Jac6<-as.expression(D(expression(p*((r*W5)+(p*W6)+(s*W7)+(l*W8)) - p*( r*((r*W1)+(p*W5)+(s*W9)+(l*W13)) 									+ p*((r*W2)+(p*W6)+(s*W10)+(l*W14)) + s*((r*W3)+(p*W7)+(s*W11)+(l*W15)) + l*((r*W4)+(p*W8)+(s*W12)+(l*W16)) ) ), "p"))	
    Jac7<-as.expression(D(expression(p*((r*W5)+(p*W6)+(s*W7)+(l*W8)) - p*( r*((r*W1)+(p*W5)+(s*W9)+(l*W13)) 									+ p*((r*W2)+(p*W6)+(s*W10)+(l*W14)) + s*((r*W3)+(p*W7)+(s*W11)+(l*W15)) + l*((r*W4)+(p*W8)+(s*W12)+(l*W16)) ) ), "s"))	
    Jac8<-as.expression(D(expression(p*((r*W5)+(p*W6)+(s*W7)+(l*W8)) - p*( r*((r*W1)+(p*W5)+(s*W9)+(l*W13)) 									+ p*((r*W2)+(p*W6)+(s*W10)+(l*W14)) + s*((r*W3)+(p*W7)+(s*W11)+(l*W15)) + l*((r*W4)+(p*W8)+(s*W12)+(l*W16)) ) ), "l"))
    #Calculates Jacobian matrix expressions for s: ∆s expression derived by r, p, s, l respectively
    Jac9<-as.expression(D(expression(s*((r*W9)+(p*W10)+(s*W11)+(l*W12)) - s*( r*((r*W1)+(p*W5)+(s*W9)+(l*W13)) 								+ p*((r*W2)+(p*W6)+(s*W10)+(l*W14)) + s*((r*W3)+(p*W7)+(s*W11)+(l*W15)) + l*((r*W4)+(p*W8)+(s*W12)+(l*W16)) ) ), "r"))
    Jac10<-as.expression(D(expression(s*((r*W9)+(p*W10)+(s*W11)+(l*W12)) - s*( r*((r*W1)+(p*W5)+(s*W9)+(l*W13)) 							+ p*((r*W2)+(p*W6)+(s*W10)+(l*W14)) + s*((r*W3)+(p*W7)+(s*W11)+(l*W15)) + l*((r*W4)+(p*W8)+(s*W12)+(l*W16)) ) ), "p"))
    Jac11<-as.expression(D(expression(s*((r*W9)+(p*W10)+(s*W11)+(l*W12)) - s*( r*((r*W1)+(p*W5)+(s*W9)+(l*W13)) 							+ p*((r*W2)+(p*W6)+(s*W10)+(l*W14)) + s*((r*W3)+(p*W7)+(s*W11)+(l*W15)) + l*((r*W4)+(p*W8)+(s*W12)+(l*W16)) ) ), "s"))
    Jac12<-as.expression(D(expression(s*((r*W9)+(p*W10)+(s*W11)+(l*W12)) - s*( r*((r*W1)+(p*W5)+(s*W9)+(l*W13)) 							+ p*((r*W2)+(p*W6)+(s*W10)+(l*W14)) + s*((r*W3)+(p*W7)+(s*W11)+(l*W15)) + l*((r*W4)+(p*W8)+(s*W12)+(l*W16)) ) ), "l"))
    #Calculates Jacobian matrix expressions for l: ∆l expression derived by r, p, s, l respectively
    Jac13<-as.expression(D(expression(l*((r*W13)+(p*W14)+(s*W15)+(l*W16)) - l*(r*((r*W1)+(p*W5)+(s*W9)+(l*W13)) 							+ p*((r*W2)+(p*W6)+(s*W10)+(l*W14)) + s*((r*W3)+(p*W7)+(s*W11)+(l*W15)) + l*((r*W4)+(p*W8)+(s*W12)+(l*W16)) ) ), "r"))
    Jac14<-as.expression(D(expression(l*((r*W13)+(p*W14)+(s*W15)+(l*W16)) - l*(r*((r*W1)+(p*W5)+(s*W9)+(l*W13)) 							+ p*((r*W2)+(p*W6)+(s*W10)+(l*W14)) + s*((r*W3)+(p*W7)+(s*W11)+(l*W15)) + l*((r*W4)+(p*W8)+(s*W12)+(l*W16)) ) ), "p"))
    Jac15<-as.expression(D(expression(l*((r*W13)+(p*W14)+(s*W15)+(l*W16)) - l*(r*((r*W1)+(p*W5)+(s*W9)+(l*W13)) 							+ p*((r*W2)+(p*W6)+(s*W10)+(l*W14)) + s*((r*W3)+(p*W7)+(s*W11)+(l*W15)) + l*((r*W4)+(p*W8)+(s*W12)+(l*W16)) ) ), "s"))
    Jac16<-as.expression(D(expression(l*((r*W13)+(p*W14)+(s*W15)+(l*W16)) - l*(r*((r*W1)+(p*W5)+(s*W9)+(l*W13)) 							+ p*((r*W2)+(p*W6)+(s*W10)+(l*W14)) + s*((r*W3)+(p*W7)+(s*W11)+(l*W15)) + l*((r*W4)+(p*W8)+(s*W12)+(l*W16)) ) ), "l"))
    #Inputs payoff matrix and eq values into expression, saves solutions as Jacobian matrix (Jac) 
    e1 <- environment()
    Jac<-matrix(nrow=4, ncol=4, byrow=T)
    Jac[1,1]<-eval(Jac1, env=e1); Jac[1,2]<-eval(Jac2, env=e1); Jac[1,3]<-eval(Jac3, env=e1); Jac[1,4]<-eval(Jac4, env=e1)
    Jac[2,1]<-eval(Jac5, env=e1); Jac[2,2]<-eval(Jac6, env=e1); Jac[2,3]<-eval(Jac7, env=e1); Jac[2,4]<-eval(Jac8, env=e1)
    Jac[3,1]<-eval(Jac9, env=e1); Jac[3,2]<-eval(Jac10, env=e1); Jac[3,3]<-eval(Jac11, env=e1); Jac[3,4]<-eval(Jac12, env=e1)
    Jac[4,1]<-eval(Jac13, env=e1); Jac[4,2]<-eval(Jac14, env=e1); Jac[4,3]<-eval(Jac15, env=e1); Jac[4,4]<-eval(Jac16, env=e1)
    #Calculates eigenvectors for projection matrix, returns values
    M <- Jac %*% P																					#Multiplies Jac & Identity
    if(isTRUE(any(M=="NaN"))==T){type<-c("NaN")} 
    else{EigM <- eigen(M, symmetric=F)
    if(abs(Re(EigM$values[4])) < zero){	
      EigM$values[1:3] <- sort(Re(EigM$values)[1:3], decreasing=T)
      if(abs(Re(EigM$values[1])) < zero){type<-c("Inconclusive")}
      else if(Re(EigM$values[1]) < -zero){type<-c("Sink")}
      else if(Re(EigM$values[3]) > zero){type<-c("Source")}	
      else{signs<-ifelse(Re(EigM$values[1:3]) > zero, 1, 0)
      if(sum(signs) == 1){type<-c("Saddle1")}; if(sum(signs) == 2){type<-c("Saddle2")}}}		
    else{type<-c("Validation error2")}		
    }
  }
  return(type)	
}