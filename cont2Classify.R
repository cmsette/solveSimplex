### Functions to generate eigenvalues & run test stability (2-players, continuous time replicator dynamics)
# Calculates eigenvalues of scaled Jacobian matrix, classifes equilibrium by combination of eigenvalues
# Sink (all -), Source (all +), Saddle (mix of - and +): Saddle 1-3 denotes number of positive eigen values
# NaN(?) (any is NaN, divided by 0), Inconclusive (2nd smallest is 0), Validation error (test function validation steps were > 1e -14, i.e. not 0)
# No eq (no equilibrium internal to simplex)
# Error1: failed equilibrium calculation validation step in Test functions (may be resolved by increasing value of "zero" var)
# Error2: projection matrix cannot be calculated
# Error3: no zero eigenvalue calculated for projection matrix
cont2Classify <- function(seq, W, zero){
  if(is.na(seq[3])==T){type <- c("Error1")}
  else if(seq[3]==0){type <- c("None")}
  else{W1<-W[1,1]; W2<-W[1,2]; W3<-W[2,1]; W4<-W[2,2]
  r <- seq[1]; p <- seq[2]
  P <- matrix(c( (1/2),(-1/2), (-1/2),(1/2) ), ncol=2, byrow=T)
  #Calculates Jacobian matrix expressions for r & p
  Jac1 <- r*W1 + p*W2 + r*W1 - (r * (r*W1 + p*W3) + p*(r*W2 + p*W4) + r*(2*r*W1 + p*W2 + p*W3))
  Jac2 <- r*W2 - r*(r*W3 + r*W2 + 2*p*W4)
  Jac3 <- p*W3 - p*(2*r*W1 + p*W2 + p*W3)
  Jac4 <- r*W3 + p*W4 + p*W4 - (r * (r*W1 + p*W3) + p*(r*W2 + p*W4) + p*(r*W2 + r*W3 + 2*p*W4))
  Jac <- matrix(c(Jac1, Jac2, Jac3, Jac4), nrow=2, ncol=2, byrow=T)
  #Calculates eigenvectors for projection matrix, returns values
  M <- Jac %*% P
  if(isTRUE(any(M == "NaN")) == T){type<-c("Error2")}
  else{EigM <- eigen(M, symmetric=F)
  if(abs(Re(EigM$values[2])) < zero){	
    if(Re(EigM$values[1]) < -zero){type<-c("Sink")}
    else if(Re(EigM$values[1]) > zero){type<-c("Source")}
    else{type<-c("Inconclusive")}}
  else{type<-c("Error3")}}
  }
  return(type)
}