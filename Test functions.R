### Solves for equilibrium for 2-5 player systems
# eq2-5: Take W in matrix format, calculate delta matrix, finds zero eigenvector, scales eigenvector to unity (equilibrium solution)
# eqVal: validates solution for eq3-5, 
eqVal <- function(soln){ 
  n = length(soln)
  test1 <- eigen(eqn)$value[n]  # saves corresponding eigenvalue, should = 0
  test2 <- abs(eqn %*% soln)		# multiplies delta matrix by solution, should = 0
  if(abs(Re(test1)) > zero | any(abs(test2) > zero)){return(c(rep(NA,n),NA))} # fails validation, returns NA's
  else if(all(as.matrix(Re(soln)) >= 0)){return(c(Re(soln),1))}               # valid output, returns eq
  else if(any(Re(soln) == Inf)){return(c(Re(soln),0))}                        # no internal eq, returns NA's
  else{return(c(rep(NA,n),0))}	                                              # no internal eq, returns NA's
}
# 2 strategies
eq2 <- function(W){ 
  deltas <- W[1,]-W[2,]
  soln <- c(deltas[2]/(deltas[2]-deltas[1]), 1-(deltas[2]/(deltas[2]-deltas[1])))
  if(all(deltas)==0){return(c(soln,NA))} 
  else{
    if(deltas[1]<0 && deltas[2]>0){return(c(soln,1))}       # returns eq solution (Sink)
    else if(deltas[1]>0 && deltas[2]<0){return(c(soln,1))} 	# returns eq solution (Source)
    else if(0<=deltas[1] && 0<=deltas[2]){soln<-c(1,0); return(c(soln,0))}  # dominant R 
    else if(deltas[2]<=0 && deltas[1]<=0){soln<-c(0,1); return(c(soln,0))}  # dominant P
    else{return(c(soln,NA))}}
}
# 3 strategies
eq3<-function(W, zero){ 
  deltas <- rbind(W[1,]-W[2,], W[2,]-W[3,], W[3,]-W[1,])				#creates delta matrix
  soln <- eigen(deltas)$vector; soln <- soln[,3]/sum(soln[,3])  #calculates & scales eigenvector for null set, eq
  return(eqVal(soln))
}
# 4 strategies
eq4<-function(W, zero){ 
  deltas <- rbind(W[1,]-W[2,], W[2,]-W[3,], W[3,]-W[4,], W[4,]-W[1,]) #creates delta matrix
  soln <- eigen(eqn)$vector; soln <- soln[,4]/sum(soln[,4])			      #calculates & scales eigenvector for null set,
  return(eqVal(soln))
}
# 5 strategies
test5<-function(W, zero){ 
  deltas <- rbind(W[1,]-W[2,], W[2,]-W[3,], W[3,]-W[4,], W[4,]-W[5,], W[5,]-W[1,])  #creates delta matrix
  soln <- eigen(eqn)$vector; soln <- soln[,5]/sum(soln[,5])						              #calculates & scales eigenvector for null set
  return(eqVal(soln))
}