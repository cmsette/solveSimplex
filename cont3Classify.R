### Functions to generate eigenvalues & run test stability (3-players, continuous time replicator dynamics)
# Calculates eigenvalues of scaled Jacobian matrix, classifes equilibrium by combination of eigenvalues
# Sink (all -), Source (all +), Saddle (mix of - and +): Saddle 1-3 denotes number of positive eigen values
# NaN(?) (any is NaN, divided by 0), Inconclusive (2nd smallest is 0), Validation error (test function validation steps were > 1e -14, i.e. not 0)
# No eq (no equilibrium internal to simplex)
cont3Classify<-function(seq, W, zero){
  if(is.na(seq[4])==T){type<-c("Validation error")}
  else if(seq[4]==0){type<-c("No eq")}
  else{W1<-W[1]; W2<-W[2]; W3<-W[3]; W4<-W[4]; W5<-W[5]; W6<-W[6]; W7<-W[7]; W8<-W[8]; W9<-W[9] 
  r <- seq[1]; p <- seq[2]; s <- seq[3]																		#Defines eq
  P <- matrix( c( (2/3),(-1/3),(-1/3), (-1/3),(2/3),(-1/3), (-1/3),(-1/3),(2/3) ), ncol=3, byrow=T)			#Identity matrix
  #Calculates Jacobian matrix expressions for r: ∆r expression derived by r, p, s, respectively
  Jac1<-as.expression(D(expression(r*((r*W1)+(p*W2)+(s*W3)) - r*( r*((r*W1)+(p*W4)+(s*W7)) + p*((r*W2)+(p*W5)+(s*W8)) 					+ s*((r*W3)+(p*W6)+(s*W9)) ) ), "r"))
  Jac2<-as.expression(D(expression(r*((r*W1)+(p*W2)+(s*W3)) - r*( r*((r*W1)+(p*W4)+(s*W7)) + p*((r*W2)+(p*W5)+(s*W8)) 					+ s*((r*W3)+(p*W6)+(s*W9)) ) ), "p"))
  Jac3<-as.expression(D(expression(r*((r*W1)+(p*W2)+(s*W3)) - r*( r*((r*W1)+(p*W4)+(s*W7)) + p*((r*W2)+(p*W5)+(s*W8)) 					+ s*((r*W3)+(p*W6)+(s*W9)) ) ), "s"))
  #Calculates Jacobian matrix expressions for p: ∆p expression derived by r, p, s, respectively
  Jac4<-as.expression(D(expression(p*((r*W4)+(p*W5)+(s*W6)) - p*( r*((r*W1)+(p*W4)+(s*W7)) + p*((r*W2)+(p*W5)+(s*W8))					 	+ s*((r*W3)+(p*W6)+(s*W9)) ) ), "r"))
  Jac5<-as.expression(D(expression(p*((r*W4)+(p*W5)+(s*W6)) - p*( r*((r*W1)+(p*W4)+(s*W7)) + p*((r*W2)+(p*W5)+(s*W8))					 	+ s*((r*W3)+(p*W6)+(s*W9)) ) ), "p"))
  Jac6<-as.expression(D(expression(p*((r*W4)+(p*W5)+(s*W6)) - p*( r*((r*W1)+(p*W4)+(s*W7)) + p*((r*W2)+(p*W5)+(s*W8))					 	+ s*((r*W3)+(p*W6)+(s*W9)) ) ), "s"))
  #Calculates Jacobian matrix expressions for s: ∆s expression derived by r, p, s, respectively
  Jac7<-as.expression(D(expression(s*((r*W7)+(p*W8)+(s*W9)) - s*( r*((r*W1)+(p*W4)+(s*W7)) + p*((r*W2)+(p*W5)+(s*W8))					 	+ s*((r*W3)+(p*W6)+(s*W9)) ) ), "r"))
  Jac8<-as.expression(D(expression(s*((r*W7)+(p*W8)+(s*W9)) - s*( r*((r*W1)+(p*W4)+(s*W7)) + p*((r*W2)+(p*W5)+(s*W8))					 	+ s*((r*W3)+(p*W6)+(s*W9)) ) ), "p"))
  Jac9<-as.expression(D(expression(s*((r*W7)+(p*W8)+(s*W9)) - s*( r*((r*W1)+(p*W4)+(s*W7)) + p*((r*W2)+(p*W5)+(s*W8))					 	+ s*((r*W3)+(p*W6)+(s*W9)) ) ), "s"))
  #Inputs payoff matrix and eq values into expression, saves solutions as Jacobian matrix (Jac)
  e1 <- environment()
  Jac<-matrix(nrow=3, ncol=3, byrow=T)
  Jac[1,1]<-as.numeric(eval(Jac1, env=e1)); Jac[1,2]<-as.numeric(eval(Jac2, env=e1)); Jac[1,3]<-as.numeric(eval(Jac3, env=e1))
  Jac[2,1]<-as.numeric(eval(Jac4, env=e1)); Jac[2,2]<-as.numeric(eval(Jac5, env=e1)); Jac[2,3]<-as.numeric(eval(Jac6, env=e1))
  Jac[3,1]<-as.numeric(eval(Jac7, env=e1)); Jac[3,2]<-as.numeric(eval(Jac8, env=e1)); Jac[3,3]<-as.numeric(eval(Jac9, env=e1))
  #Calculates eigenvectors for projection matrix, returns values
  M <- Jac %*% P																					#Multiplies Jac & Identity
  if(isTRUE(any(M=="NaN"))==T){type<-c("NaN")} 
  else{EigM <- eigen(M, symmetric=F)
  if(abs(Re(EigM$values[3])) < zero){	
    EigM$values[1:2] <- sort(Re(EigM$values)[1:2], decreasing=T)
    if(abs(Re(EigM$values[1])) < zero){type<-c("Inconclusive")}
    else if(Re(EigM$values[1]) < -zero){type<-c("Sink")}
    else if(Re(EigM$values[2]) > zero){type<-c("Source")}	
    else{signs<-ifelse(Re(EigM$values[1:2]) > zero, 1, 0)
    if(sum(signs) == 1){type<-c("Saddle1")}}}		
  else{type<-c("Validation error2")}		
  }
  }
  return(type)
}