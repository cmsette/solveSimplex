### Functions to generate eigenvalues & run test stability (3-players, continuous time replicator dynamics)
# Calculates eigenvalues of scaled Jacobian matrix, classifes equilibrium by combination of eigenvalues
# Sink (all -), Source (all +), Saddle (mix of - and +): Saddle 1-3 denotes number of positive eigen values
# NaN(?) (any is NaN, divided by 0), Inconclusive (2nd smallest is 0), Validation error (test function validation steps were > 1e -14, i.e. not 0)
# No eq (no equilibrium internal to simplex)
# Error1: failed equilibrium calculation validation step in Test functions (may be resolved by increasing value of "zero" var)
# Error2: projection matrix cannot be calculated
# Error3: no zero eigenvalue calculated for projection matrix
cont3Classify <- function(seq, W, zero, vars){
  colnames(W) <- rownames(W) <- vars
  if(any(is.na(seq[1:3]))){type <- c("None")}else 
  if(is.na(seq[4])){type <- c("Error1")}else 
  if(seq[4]==0){type <- c("None")}
  else{
    W1<-W[1,1]; W2<-W[1,2]; W3<-W[1,3]; W4<-W[2,1]; W5<-W[2,2]; W6<-W[2,3]; W7<-W[3,1]; W8<-W[3,2]; W9<-W[3,3] 
    r <- seq[1]; p <- seq[2]; s <- seq[3]
    P <- matrix( c( (2/3),(-1/3),(-1/3), (-1/3),(2/3),(-1/3), (-1/3),(-1/3),(2/3) ), ncol=3, byrow=T)
    # Calculates Jacobian matrix expressions for r: ∆r expression derived by r, p, s, respectively
    Jac1 <- 2*r*W1 + p*W2 + s*W3 - (r*(3*r*W1 + p*W2 + 2*p*W4 + s*W3 + 2*s*W7) + p*(r*W2 + p*W5 + s*W8) + s*(r*W3 + p*W6 + s*W9))
    Jac2 <- r*W2 - r*(r*W2 + r*W4 + 2*p*W5 + s*W6 + s*W8)
    Jac3 <- r*W3 - r*(r*W3 + r*W7 + p*W6 + p*W8 + 2*s*W9)
    # Calculates Jacobian matrix expressions for p: ∆p expression derived by r, p, s, respectively
    Jac4 <- p*W4 - p*(2*r*W1 + p*W2 + p*W4 + s*W3 + s*W7)
    Jac5 <- r*W4 + 2*p*W5 + s*W6 - (r*(r*W1 + p*W4 + s*W7) + p*(2*r*W2 + r*W4 + 3*p*W5 + s*W6 + 2*s*W8) + s*(r*W3 + p*W6 + s*W9))
    Jac6 <- p*W6 - p*(r*W3 + r*W7 + p*W6 + p*W8 + 2*s*W9)
    # Calculates Jacobian matrix expressions for s: ∆s expression derived by r, p, s, respectively
    Jac7 <- s*W7 - s*(2*r*W1 + p*W2 + p*W4 + s*W3 + s*W7)
    Jac8 <- s*W8 - s*(r*W2 + r*W4 + 2*p*W5 + s*W6 + s*W8)
    Jac9 <- r*W7 + p*W8 + 2*s*W9 - (r*(r*W1 + p*W4 + s*W7) + p*(r*W2 + p*W5 + s*W8) + s*(2*r*W3 + r*W7 + 2*p*W6 + p*W8 + 3*s*W9))
    Jac <- matrix(c(Jac1, Jac2, Jac3, Jac4, Jac5, Jac6, Jac7, Jac8, Jac9), nrow=3, ncol=3, byrow=T)
    # Calculates eigenvectors for projection matrix, returns values
    M <- Jac %*% P
    if(isTRUE(any(M=="NaN"))==T){type <- c("Error2")} 
    else{EigM <- eigen(M, symmetric=F)
      if(abs(Re(EigM$values[3])) < zero){	
      EigM$values[1:2] <- sort(Re(EigM$values)[1:2], decreasing=T)
      if(abs(Re(EigM$values[1])) < zero){type <- c("Inconclusive")}
      else if(Re(EigM$values[1]) < -zero){type <- c("Sink")}
      else if(Re(EigM$values[2]) > zero){type <- c("Source")}	
      else{signs<-ifelse(Re(EigM$values[1:2]) > zero, 1, 0)
      if(sum(signs) == 1){type <- c("Saddle1")}}}		
      else{type <- c("Error3")}	
    }
  }; return(list(W = W, Eq = seq[1:3], Class = type, Eig = EigM))
}