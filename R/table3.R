### Function to generate solution tables (2--players), called by solveSimplex
table3 <- function(Wmatrix, gen_time, zero, sig_dig, vars){
  svars <- substr(vars,1,2)
  colnames(Wmatrix) <- rownames(Wmatrix) <- vars
  if(gen_time == "continuous"){}
  else if(gen_time == "discrete"){stop("discrete generation time not yet implemented")}
  else{stop("invalid generation time")}
  # solves 3-strategy equilibrium
  eq3out <- eq3(Wmatrix, zero)
  if(is.na(eq3out[1])){colnames(Wmatrix) <- rownames(Wmatrix) <- vars; classed <- list(W = Wmatrix, Eq = eq3out[1:3], Class = "None", Eig = NA)}
  else{classed <- cont3Classify(eq3out, Wmatrix, zero, vars)}
  # solves 2-strategy full games, generates 2-strategy eq vectors
  eq2RP <- table2(Wmatrix[-3,-3], gen_time, zero, sig_dig, vars[c(1,2)]); eq2RPout <- c(eq2RP[[2]][1:2],0,1)
  eq2RS <- table2(Wmatrix[-2,-2], gen_time, zero, sig_dig, vars[c(1,3)]); eq2RSout <- c(eq2RS[[2]][1],0,eq2RS[[2]][2],1)
  eq2PS <- table2(Wmatrix[-1,-1], gen_time, zero, sig_dig, vars[c(2,3)]); eq2PSout <- c(0,eq2PS[[2]][1:2],1)
  # solves 3(2) partial games
  eq3RP <- cont3Classify(eq2RPout, Wmatrix, zero, vars)
  eq3RS <- cont3Classify(eq2RSout, Wmatrix, zero, vars)
  eq3PS <- cont3Classify(eq2PSout, Wmatrix, zero, vars)
  # full game equilibrium table with intransitivity, apostasis, anti-apostasis
  RPS_Eq <- data.frame("R" = c(eq3out[1], eq2RPout[1], eq2RSout[1], eq2PSout[1]), 
                       "P" = c(eq3out[2], eq2RPout[2], eq2RSout[2], eq2PSout[2]), 
                       "S" = c(eq3out[3], eq2RPout[3], eq2RSout[3], eq2PSout[3]),
                       "Eq" = c(classed[[3]], eq3RP[[3]], eq3RS[[3]], eq3PS[[3]]))
  colnames(RPS_Eq) <- c(vars[1], vars[2], vars[3], "Eq")
  rownames(RPS_Eq) <- c(paste(svars[1],svars[2],svars[3],sep="."), paste(svars[1],svars[2],"0",sep="."), paste(svars[1],"0",svars[3],sep="."), paste("0",svars[2],svars[3],sep="."))
  Int <- c(testIntransitive(Wmatrix), rep("",3))
  Apo <- c(testApostatic(Wmatrix, substr(vars,1,2)), rep("",3))
  A_Apo <- c(testAntiApostatic(Wmatrix, substr(vars,1,2)), rep("",3))
  RPS_Eq <- cbind(RPS_Eq, Int, Apo, A_Apo)
  table_full <- list(W_matrix = classed[[1]], Eq3 = classed[[2]], Equilibria = RPS_Eq)
  # assemble eigendecomposition list for full game
  Eig <- list(classed[[4]], eq3RP[[4]], eq3RS[[4]], eq3PS[[4]]) 
  names(Eig) <- c(paste(svars[1],svars[2],svars[3],sep="."), paste(svars[1],".",svars[2],".0",sep=""), paste(svars[1],".0.",svars[3],sep=""), paste("0.",svars[2],".",svars[3],sep=""))
  # 2-face games equilibrium table
  RPS2_Eq <- data.frame("R" = c(eq2RPout[1], eq2RSout[1], NA), 
                        "P" = c(eq2RPout[2], NA, eq2PSout[2]), 
                        "S" = c(NA, eq2RSout[3], eq2PSout[3]),
                        "Eq" = unlist(c(eq2RP[[3]][3], eq2RS[[3]][3], eq2PS[[3]][3])))
  colnames(RPS2_Eq) <- c(vars[1],vars[2],vars[3],"Eq")
  rownames(RPS2_Eq) <- c(paste(svars[1],svars[2],"_",sep="."), paste(svars[1],"_",svars[3],sep="."), paste("_",svars[2],svars[3],sep="."))
  # assemble output table for face games
  Eig_Edges <- list(eq3RP[[4]], eq3RS[[4]], eq3PS[[4]])
  names(Eig_Edges) <- c(paste(svars[1],".",svars[2],sep=""), paste(svars[1],".",svars[3],sep=""), paste(svars[2],".",svars[3],sep=""))
  # output table
  table_out <- list(table_full, Faces = RPS2_Eq, Eig = list("Eigendecomposition" = Eig, "Eigendecomposition_Faces" = Eig_Edges))
  names(table_out) <- c(paste(vars[1],vars[2],vars[3],sep="."), "Faces(2)", "Eigendecomposition")
  return(table_out)
}