### Functions to generate eigenvalues & run test stability (2-players, continuous time replicator dynamics)
# Calculates eigenvalues of scaled Jacobian matrix, classifes equilibrium by combination of eigenvalues
# Sink (all -), Source (all +), Saddle (mix of - and +): Saddle 1-3 denotes number of positive eigen values
# NaN(?) (any is NaN, divided by 0), Inconclusive (2nd smallest is 0), Validation error (test function validation steps were > 1e -14, i.e. not 0)
# No eq (no equilibrium internal to simplex)
cont2Classify<-function(seq, W, zero){
  if(is.na(seq[3])==T){type<-c("Validation error")}
  else if(seq[3]==0){type<-c("No eq")}
  else{W1<-W[1]; W2<-W[2]; W3<-W[3]; W4<-W[4]
  r <- seq[1]; p <- seq[2]
  P <- matrix(c( (1/2),(-1/2), (-1/2),(1/2) ), ncol=2, byrow=T)
  #Calculates Jacobian matrix expressions for r & p
  Jac1<-as.expression(D(expression( r*((r*W1)+(p*W2)) - r*( r*((r*W1)+(p*W3)) + p*((r*W2)+(p*W4)) ) ), "r"))
  Jac2<-as.expression(D(expression( r*((r*W1)+(p*W2)) - r*( r*((r*W1)+(p*W3)) + p*((r*W2)+(p*W4)) ) ), "p"))
  Jac3<-as.expression(D(expression( p*((r*W3)+(p*W4)) - p*( r*((r*W1)+(p*W3)) + p*((r*W2)+(p*W4)) ) ), "r"))
  Jac4<-as.expression(D(expression( p*((r*W3)+(p*W4)) - p*( r*((r*W1)+(p*W3)) + p*((r*W2)+(p*W4)) ) ), "p"))
  #Inputs payoff matrix and eq values into expression, saves solutions as Jacobian matrix (Jac)
  e1 <- environment()
  Jac<-matrix(nrow=2, ncol=2, byrow=T)
  print(Jac)
  Jac[1,1]<-eval(Jac1, env=e1); Jac[1,2]<-eval(Jac2, env=e1); Jac[2,1]<-eval(Jac3, env=e1); Jac[2,2]<-eval(Jac4, env=e1);
  #Calculates eigenvectors for projection matrix, returns values
  M <- Jac %*% P
  if(isTRUE(any(M=="NaN"))==T){type<-c("")}
  else{EigM <- eigen(M, symmetric=F)
  if(abs(Re(EigM$values[2])) < zero){	
    if(Re(EigM$values[1]) < -zero){type<-c("Sink")}
    else if(Re(EigM$values[1]) > zero){type<-c("Source")}
    else{type<-c("Inconclusive")}}
  else{type<-c("Validation error2")}}
  }
  return(type)
}