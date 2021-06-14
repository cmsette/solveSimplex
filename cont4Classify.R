### Functions to generate eigenvalues & run test stability (4-players, continuous time replicator dynamics)
# Calculates eigenvalues of scaled Jacobian matrix, classifes equilibrium by combination of eigenvalues
# Sink (all -), Source (all +), Saddle (mix of - and +): Saddle 1-3 denotes number of positive eigen values
# NaN(?) (any is NaN, divided by 0), Inconclusive (2nd smallest is 0), Validation error (test function validation steps were > 1e -14, i.e. not 0)
# No eq (no equilibrium internal to simplex)
# Error1: failed equilibrium calculation validation step in Test functions (may be resolved by increasing value of "zero" var)
# Error2: projection matrix cannot be calculated
# Error3: no zero eigenvalue calculated for projection matrix
cont4Classify <- function(seq, W, zero, vars){
  if(is.na(seq[5])==T){type<-c("Error1")}
  else if(seq[5]==0){type<-c("None")}
  else{
    W1<-W[1,1]; W2<-W[1,2]; W3<-W[1,3]; W4<-W[1,4]; W5<-W[2,1]; W6<-W[2,2]; W7<-W[2,3]; W8<-W[2,4]
    W9<-W[3,1]; W10<-W[3,2]; W11<-W[3,3]; W12<-W[3,4]; W13<-W[4,1]; W14<-W[4,2]; W15<-W[4,3]; W16<-W[4,4]
    r <- seq[1]; p <- seq[2]; s <- seq[3]; l <- seq[4]
    P <- matrix( c( (3/4),(-1/4),(-1/4),(-1/4), (-1/4),(3/4),(-1/4),(-1/4), (-1/4),(-1/4),(3/4),(-1/4), (-1/4),(-1/4),(-1/4),(3/4) ), ncol=4, byrow=T)
    # Calculates Jacobian matrix expressions for r: ∆r expression derived by r, p, s, l respectively
    Jac1 <- 2*r*W1 + p*W2 + s*W3 + l*W4 - (r*(3*r*W1 + p*W2 + 2*p*W5 + s*W3 + 2*s*W9 + l*W4 + 2*l*W13) + p*(r*W2 + p*W6 + s*W10 + l*W14) + s*(r*W3 + p*W7 + s*W11 + l*W15) + l*(r*W4 + p*W8 + s*W12 + l*W16))
    Jac2 <- r*W2 - r*(r*W2 + r*W5 + 2*p*W6 + s*W7 + s*W10 + l*W8 + l*W14)
    Jac3 <- r*W3 - r*(r*W3 + r*W9 + p*W7 + p*W10 + 2*s*W11 + l*W12 + l*W15)
    Jac4 <- r*W4 - r*(r*W4 + r*W13 + p*W8 + p*W14 + s*W12 + s*W15 + 2*l*W16)
    # Calculates Jacobian matrix expressions for p: ∆p expression derived by r, p, s, l respectively
    Jac5 <-	p*W5 - p*(2*r*W1 + p*W2 + p*W5 + s*W3 + s*W9 + l*W4 + l*W13)
    Jac6 <- r*W5 + 2*p*W6 + s*W7 + l*W8 - (r*(r*W1 + p*W5 + s*W9 + l*W13) + p*(2*r*W2 + r*W5 + 3*p*W6 + s*W7 + 2*s*W10 + l*W8 + 2*l*W14) + s*(r*W3 + p*W7 + s*W11 + l*W15) + l*(r*W4 + p*W8 + s*W12 + l*W16))
    Jac7 <- p*W7 - p*(r*W3 + r*W9 + p*W7 + p*W10 + 2*s*W11 + l*W12 + l*W15)
    Jac8 <- p*W8 - p*(r*W4 + r*W13 + p*W8 + p*W14 + s*W12 + s*W15 + 2*l*W16)
    # Calculates Jacobian matrix expressions for s: ∆s expression derived by r, p, s, l respectively
    Jac9 <- s*W9 - s*(2*r*W1 + p*W2 + p*W5 + s*W3 + s*W9 + l*W4 + l*W13)
    Jac10 <- s*W10 - s*(r*W2 + r*W5 + 2*p*W6 + s*W7 + s*W10 + l*W8 + l*W14)
    Jac11 <- r*W9 + p*W10 + 2*s*W11 + l*W12 - (r*(r*W1 + p*W5 + s*W9 + l*W13) + p*(r*W2 + p*W6 + s*W10 + l*W14) + s*(2*r*W3 + r*W9 + 2*p*W7 + p*W10 + 3*s*W11 + l*W12 + 2*l*W15) + l*(r*W4 + p*W8 + s*W12 + l*W16))
    Jac12 <- s*W12 - s*(r*W4 + r*W13 + p*W8 + p*W14 + s*W12 + s*W15 + 2*l*W16)
    # Calculates Jacobian matrix expressions for l: ∆l expression derived by r, p, s, l respectively
    Jac13 <- l*W13 - l*(2*r*W1 + p*W2 + p*W5 + s*W3 + s*W9 + l*W4 + l*W13)
    Jac14 <- l*W14 - l*(r*W2 + r*W5 + 2*p*W6 + s*W7 + s*W10 + l*W8 + l*W14)
    Jac15 <- l*W15 - l*(r*W3 + r*W9 + p*W7 + p*W10 + 2*s*W11 + l*W12 + l*W15)
    Jac16 <- r*W13 + p*W14 + s*W15 + 2*l*W16 - (r*(r*W1 + p*W5 + s*W9 + l*W13) + p*(r*W2 + p*W6 + s*W10 + l*W14) + s*(r*W3 + p*W7 + s*W11 + l*W15) + l*(2*r*W4 + r*W13 + 2*p*W8 + p*W14 + 2*s*W12 + s*W15 + 3*l*W16))
    Jac <- matrix(c(Jac1, Jac2, Jac3, Jac4, Jac5, Jac6, Jac7, Jac8, Jac9, Jac10, Jac11, Jac12, Jac13, Jac14, Jac15, Jac16), nrow=4, ncol=4, byrow=T)
    # Calculates eigenvectors for projection matrix, returns values
    M <- Jac %*% P
    if(isTRUE(any(M=="NaN"))==T){type<-c("Error2")} 
    else{EigM <- eigen(M, symmetric=F)
      if(abs(Re(EigM$values[4])) < zero){	
        EigM$values[1:3] <- sort(Re(EigM$values)[1:3], decreasing=T)
        if(abs(Re(EigM$values[1])) < zero){type<-c("Inconclusive")}
        else if(Re(EigM$values[1]) < -zero){type<-c("Sink")}
        else if(Re(EigM$values[3]) > zero){type<-c("Source")}	
        else{signs<-ifelse(Re(EigM$values[1:3]) > zero, 1, 0)
        if(sum(signs) == 1){type<-c("Saddle1")}; if(sum(signs) == 2){type<-c("Saddle2")}}
      }	else{type<-c("Error3")}		
    }
    colnames(W) <- substr(vars,1,3); rownames(W) <- substr(vars,1,3)
  }; return(list(W = W, Eq = type, Eig = EigM))
}