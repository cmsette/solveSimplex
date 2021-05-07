### Solves equilibrium for 2-5 player systems
# Calculates delta matrix, finds zero eigenvector (equilibrium), then scales eigenvector to unity (simplex solution)		
test2<-function(W){ 
  eqn<-c(W[1,2]-W[2,2], W[2,1]-W[1,1])
  soln<-c(eqn[1]/(eqn[1]+eqn[2]), 1-(eqn[1]/(eqn[1]+eqn[2])))
  if(all(eqn)==0){return(c(soln,NA))} else{
    if(eqn[1]>0 && eqn[2]>0){return(c(soln,1))} 								#returns eq solution (Sink)
    else if(eqn[1]<0 && eqn[2]<0){return(c(soln,1))} 							#returns eq solution (Source)
    else if(eqn[1]<=0 && 0<=eqn[2]){soln<-c(0,1); return(c(soln,0))} 			#dominant P
    else if(eqn[2]<=0 && 0<=eqn[1]){soln<-c(1,0); return(c(soln,0))}				#dominant R
    else{return(c(soln,NA))}}
}
###Equilibrium function for 3-side games
test3<-function(W, zero){ 
  eqn<-matrix(c(W[1,1]-W[2,1], W[2,1]-W[3,1], W[3,1]-W[1,1],  W[1,2]-W[2,2], W[2,2]-W[3,2], W[3,2]-W[1,2], 								W[1,3]-W[2,3], W[2,3]-W[3,3], W[3,3]-W[1,3]), ncol=3, byrow=F)								#creates delta matrix
  soln<-eigen(eqn)$vector; soln<-soln[,3]/sum(soln[,3])						#calculates & scales eigenvector for null set, eq
  test1<-eigen(eqn)$value[3]													#saves corresponding eigenvalue, should = 0
  test2<-abs(eqn %*% soln)													#multiplies delta matrix by solution, should = 0
  if(abs(Re(test1)) > zero | any(abs(test2)  > zero)){return(c(rep(NA,3),NA))} else 				#validation
    if(all(as.matrix(Re(soln)) >= 0)){return(c(Re(soln),1))} else{return(c(rep(NA,3),0))}			#returns eq soln
}
###Equilibrium function for 4-side games
test4<-function(W, zero){ 
  eqn<-matrix(c(W[1,1]-W[2,1], W[2,1]-W[3,1], W[3,1]-W[4,1], W[4,1]-W[1,1], 																W[1,2]-W[2,2], W[2,2]-W[3,2], W[3,2]-W[4,2], W[4,2]-W[1,2],																			W[1,3]-W[2,3], W[2,3]-W[3,3], W[3,3]-W[4,3], W[4,3]-W[1,3],																			W[1,4]-W[2,4], W[2,4]-W[3,4], W[3,4]-W[4,4], W[4,4]-W[1,4]), ncol=4, byrow=F)				#creates delta matrix
  soln<-eigen(eqn)$vector; soln<-soln[,4]/sum(soln[,4])						#calculates & scales eigenvector for null set,
  test1<-eigen(eqn)$value[4]													#saves corresponding eigenvalue, should = 0
  test2<-abs(eqn %*% soln)													#multiplies delta matrix by solution, should = 0
  if(abs(Re(test1)) > zero | any(abs(test2) > zero)){return(c(rep(NA,4),NA))} else 				#validation 
    if(all(as.matrix(Re(soln)) >= 0)){return(c(Re(soln),1))} else{return(c(rep(NA,4),0))}			#returns eq soln
}
###Equilibrium function for 5-side games
test5<-function(W, zero){ 
  eqn<-matrix(c(W[1,1]-W[2,1], W[2,1]-W[3,1], W[3,1]-W[4,1], W[4,1]-W[5,1], W[5,1]-W[1,1],												W[1,2]-W[2,2], W[2,2]-W[3,2], W[3,2]-W[4,2], W[4,2]-W[5,2], W[5,2]-W[1,2],															W[1,3]-W[2,3], W[2,3]-W[3,3], W[3,3]-W[4,3], W[4,3]-W[5,3], W[5,3]-W[1,3],															W[1,4]-W[2,4], W[2,4]-W[3,4], W[3,4]-W[4,4], W[4,4]-W[5,4], W[5,4]-W[1,4],															W[1,5]-W[2,5], W[2,5]-W[3,5], W[3,5]-W[4,5], W[4,5]-W[5,5], W[5,5]-W[1,5]), ncol=5, byrow=F)		#creates delta
  soln<-eigen(eqn)$vector; soln<-soln[,5]/sum(soln[,5])						#calculates & scales eigenvector for null set
  test1<-eigen(eqn)$value[5]													#saves corresponding eigenvalue, should = 0
  test2<-abs(eqn %*% soln)													#multiplies delta matrix by solution, should = 0
  if(abs(Re(test1)) > zero | any(abs(test2) > zero)){return(c(rep(NA,5),NA))} else 			#validation
    if(all(as.matrix(Re(soln)) >= 0)){return(c(Re(soln),1))} else{return(c(rep(NA,5),0))}		#returns eq soln
}