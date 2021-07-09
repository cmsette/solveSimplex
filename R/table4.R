### Function to generate solution tables (2--players), called by solveSimplex
table4 <- function(Wmatrix, gen_time, zero, sig_dig, vars){
  svars <- substr(vars,1,2)
  colnames(Wmatrix) <- rownames(Wmatrix) <- vars
  if(gen_time == "continuous"){}
  else if(gen_time == "discrete"){stop("discrete generation time not yet implemented")}
  else{stop("invalid generation time")}
  # solves 4-strategy equilibrium
  eq4out <- eq4(Wmatrix, zero)
  if(is.na(eq4out[1])){classed <- list(W = Wmatrix, Eq = eq4out[1:4], Class = "None", Eig = NA)}
  else{classed <- cont4Classify(eq4out, Wmatrix, zero, vars)}
  # solves 3-strategy and 2-strategy full games, generates 3- and 2-strategy eq vectors 
  eq3RPS <- table3(Wmatrix[-4,-4], gen_time, zero, sig_dig, vars[c(1,2,3)]); eq4RPSout <- c(eq3RPS[[1]][[2]][1:3],0,1)
  eq3RPL <- table3(Wmatrix[-3,-3], gen_time, zero, sig_dig, vars[c(1,2,4)]); eq4RPLout <- c(eq3RPL[[1]][[2]][1:2],0,eq3RPL[[1]][[2]][3],1)
  eq3RSL <- table3(Wmatrix[-2,-2], gen_time, zero, sig_dig, vars[c(1,3,4)]); eq4RSLout <- c(eq3RSL[[1]][[2]][1],0,eq3RSL[[1]][[2]][2:3],1)
  eq3PSL <- table3(Wmatrix[-1,-1], gen_time, zero, sig_dig, vars[c(2,3,4)]); eq4PSLout <- c(0,eq3PSL[[1]][[2]][1:3],1)
  # generates 2-strategy eq vectors 
  eq4RPout <- unlist(c(eq3RPS[[1]][[3]][2,1:3],0,1)); eq4RSout <- unlist(c(eq3RPS[[1]][[3]][3,1:3],0,1))
  eq4RLout <- unlist(c(eq3RPL[[1]][[3]][3,1:2],0,eq3RPL[[1]][[3]][3,3],1)); eq4PSout <- unlist(c(eq3RPS[[1]][[3]][4,1:3],0,1))
  eq4PLout <- unlist(c(0,eq3PSL[[1]][[3]][3,1:3],1)); eq4SLout <- unlist(c(0,eq3PSL[[1]][[3]][4,1:3],1))
  # solves 4(3) partial games
  eq4RPS <- cont4Classify(eq4RPSout, Wmatrix, zero, vars)
  eq4RPL <- cont4Classify(eq4RPLout, Wmatrix, zero, vars)
  eq4RSL <- cont4Classify(eq4RSLout, Wmatrix, zero, vars)
  eq4PSL <- cont4Classify(eq4PSLout, Wmatrix, zero, vars)
  # solves 4(2) partial games
  eq4RP <- cont4Classify(eq4RPout, Wmatrix, zero, vars)
  eq4RS <- cont4Classify(eq4RSout, Wmatrix, zero, vars)
  eq4RL <- cont4Classify(eq4RLout, Wmatrix, zero, vars)
  eq4PS <- cont4Classify(eq4PSout, Wmatrix, zero, vars)
  eq4PL <- cont4Classify(eq4PLout, Wmatrix, zero, vars)
  eq4SL <- cont4Classify(eq4SLout, Wmatrix, zero, vars)
  # full game equilibrium table with intransitivity, apostasis, anti-apostasis
  RPSL_Eq <- data.frame("R" = c(eq4out[1], eq4RPSout[1], eq4RPLout[1], eq4RSLout[1], eq4PSLout[1], eq4RPout[1], eq4RSout[1], eq4RLout[1], eq4PSout[1], eq4PLout[1], eq4SLout[1]), 
                        "P" = c(eq4out[2], eq4RPSout[2], eq4RPLout[2], eq4RSLout[2], eq4PSLout[2], eq4RPout[2], eq4RSout[2], eq4RLout[2], eq4PSout[2], eq4PLout[2], eq4SLout[2]), 
                        "S" = c(eq4out[3], eq4RPSout[3], eq4RPLout[3], eq4RSLout[3], eq4PSLout[3], eq4RPout[3], eq4RSout[3], eq4RLout[3], eq4PSout[3], eq4PLout[3], eq4SLout[3]),
                        "L" = c(eq4out[4], eq4RPSout[4], eq4RPLout[4], eq4RSLout[4], eq4PSLout[4], eq4RPout[4], eq4RSout[4], eq4RLout[4], eq4PSout[4], eq4PLout[4], eq4SLout[4]),
                        "Eq" = c(classed[[3]], eq4RPS[[3]], eq4RPL[[3]], eq4RSL[[3]], eq4PSL[[3]], eq4RP[[3]], eq4RS[[3]], eq4RL[[3]], eq4PS[[3]], eq4PL[[3]], eq4SL[[3]]))
  colnames(RPSL_Eq) <- c(vars[1], vars[2], vars[3], vars[4], "Eq")
  rownames(RPSL_Eq) <- c(paste(svars[1],svars[2],svars[3],svars[4],sep="."), 
                          paste(svars[1],svars[2],svars[3],"0",sep="."), paste(svars[1],svars[2],"0",svars[4],sep="."), paste(svars[1],"0",svars[3],svars[4],sep="."), paste("0",svars[2],svars[3],svars[4],sep="."),
                          paste(svars[1],svars[2],"0","0",sep="."), paste(svars[1],"0",svars[3],"0",sep="."), paste(svars[1],"0","0",svars[4],sep="."), 
                          paste("0",svars[2],svars[3],"0",sep="."), paste("0",svars[2],"0",svars[4],sep="."), paste("0","0",svars[3],svars[4],sep="."))
  Int <- c(testIntransitive(Wmatrix), testIntransitive(Wmatrix[-4,-4]), testIntransitive(Wmatrix[-3,-3]), testIntransitive(Wmatrix[-2,-2]), testIntransitive(Wmatrix[-1,-1]), rep("",6))
  Apo <- c(testApostatic(Wmatrix, svars), testApostatic(Wmatrix[-4,-4], svars[-4]), testApostatic(Wmatrix[-3,-3], svars[-3]), testApostatic(Wmatrix[-2,-2], svars[-2]), testApostatic(Wmatrix[-1,-1], svars[-1]), rep("",6))
  A_Apo <- c(testAntiApostatic(Wmatrix, svars), testAntiApostatic(Wmatrix[-4,-4], svars[-4]), testAntiApostatic(Wmatrix[-3,-3], svars[-3]), testAntiApostatic(Wmatrix[-2,-2], svars[-2]), testAntiApostatic(Wmatrix[-1,-1], svars[-1]), rep("",6))
  RPSL_Eq <- cbind(RPSL_Eq, Int, Apo, A_Apo)
  table_full <- list(W_matrix = classed[[1]], Eq4 = classed[[2]], Equilibria = RPSL_Eq)
  # assemble eigendecomposition list for full game
  Eig <- list(classed[[4]], eq4RPS[[4]], eq4RPL[[4]], eq4RSL[[4]], eq4PSL[[4]], eq4RP[[4]], eq4RS[[4]], eq4RL[[4]], eq4PS[[4]], eq4PL[[4]], eq4SL[[4]]) 
  names(Eig) <- c(paste(svars[1],svars[2],svars[3],svars[4],sep="."), 
    paste(svars[1],svars[2],svars[3],0,sep="."), paste(svars[1],svars[2],0,svars[4],sep="."), paste(svars[1],0,svars[3],svars[4],sep="."), paste(0,svars[2],svars[3],svars[4],sep="."),
    paste(svars[1],svars[2],"0.0",sep="."), paste(svars[1],"0",svars[3],"0",sep="."), paste(svars[1],"0.0",svars[4],sep="."), paste("0",svars[2],svars[3],"0",sep="."), paste("0",svars[2],"0",svars[4],sep="."), paste("0.0",svars[3],svars[4],sep="."))
  # 3-face games equilibrium table
  table_3faces <- c(eq3RPS[[1]][3], eq3RPL[[1]][3], eq3RSL[[1]][3], eq3PSL[[1]][3])
  names(table_3faces) <- c(paste(svars[1],svars[2],svars[3],"_",sep="."), paste(svars[1],svars[2],"_",svars[4],sep="."), paste(svars[1],"_",svars[3],svars[4],sep="."), paste("_",svars[2],svars[3],svars[4],sep="."))
  Eig_3Edges = c(eq3RPS[[3]][1], eq3RPL[[3]][1], eq3RSL[[3]][1], eq3PSL[[3]][1])
  names(Eig_3Edges) <- names(table_3faces)
  # 2-face games equilibrium table
  RPSL2_Eq <- data.frame("R" = c(eq4RPout[1], eq4RSout[1], eq4RLout[1], NA, NA, NA), 
                         "P" = c(eq4RPout[2], NA, NA, eq4PSout[2], eq4PLout[2],NA), 
                         "S" = c(NA, eq4RSout[3], NA, eq4PSout[3], NA, eq4SLout[3]),
                         "L" = c(NA, NA, eq4RLout[4], NA, eq4PLout[4], eq4SLout[4]),
                         "Eq" = c(eq3RPS[[2]][1,4], eq3RPS[[2]][2,4], eq3RPL[[2]][2,4], eq3RPS[[2]][3,4], eq3PSL[[2]][2,4], eq3PSL[[2]][3,4]))
  colnames(RPSL2_Eq) <- c(vars[1],vars[2],vars[3],vars[4],"Eq")
  rownames(RPSL2_Eq) <- c(paste(svars[1],svars[2],"_","_",sep="."), paste(svars[1],"_",svars[3],"_",sep="."), paste(svars[1],"_","_",svars[4],sep="."), 
                          paste("_",svars[2],svars[3],"_",sep="."), paste("_",svars[2],"_",svars[4],sep="."), paste("_","_",svars[3],svars[4],sep="."))
  Eig_2Edges <- c(eq3RPS[[3]][[2]][1], eq3RPS[[3]][[2]][2], eq3RPL[[3]][[2]][2], eq3RPS[[3]][[2]][3], eq3PSL[[3]][[2]][2], eq3PSL[[3]][[2]][3])
  # output table
  table_out <- list(table_full, Faces = list(table_3faces, RPSL2_Eq), Eig = list("Eigendecomposition" = Eig, "Eigendecomposition_Faces(3)" = Eig_3Edges, "Eigendecomposition_Faces(2)" = Eig_2Edges))
  names(table_out) <- c(paste(vars[1],vars[2],vars[3],vars[4],sep="."), "Face_Equilibria", "Eigendecomposition"); names(table_out[[2]]) = c("Faces(3)", "Faces(2)")
  return(table_out)
}