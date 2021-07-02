### Function to generate solution tables (2--players), called by solveSimplex
table5 <- function(Wmatrix, gen_time, zero, sig_dig, vars){
  svars <- substr(vars,1,2)
  if(gen_time == "continuous"){}
  else if(gen_time == "discrete"){stop("discrete generation time not yet implemented")}
  else{stop("invalid generation time")}
  # solves 4-strategy equilibrium
  eq5out <- eq5(Wmatrix, zero)
  if(is.na(eq4out[1])){classed <- list(W = Wmatrix, Eq = eq4out[1:5], Class = "None", Eig = NA)}
  else{classed <- cont5Classify(eq5out, Wmatrix, zero, vars)}
  # solves 4-strategy, 3-strategy and 2-strategy full games, generates 4-strategy eq vectors 
  eq4RPSL <- table4(Wmatrix[-5,-5], gen_time, zero, sig_dig, vars[c(1,2,3,4)]); eq5RPSLout <- c(eq4RPSL[[1]][[2]][1:4],0,1)
  eq4RPSK <- table4(Wmatrix[-4,-4], gen_time, zero, sig_dig, vars[c(1,2,3,5)]); eq5RPSKout <- c(eq4RPSK[[1]][[2]][1:3],0,eq4RPSK[[1]][[2]][4],1)
  eq4RPLK <- table4(Wmatrix[-3,-3], gen_time, zero, sig_dig, vars[c(1,2,4,5)]); eq5RPLKout <- c(eq4RPLK[[1]][[2]][1:2],0,eq4RPLK[[1]][[2]][3:4],1)
  eq4RSLK <- table4(Wmatrix[-2,-2], gen_time, zero, sig_dig, vars[c(1,3,4,5)]); eq5RSLKout <- c(eq4RSLK[[1]][[2]][1],0,eq4RSLK[[1]][[2]][2:4],1)
  eq4PSLK <- table4(Wmatrix[-1,-1], gen_time, zero, sig_dig, vars[c(2,3,4,5)]); eq5PSLKout <- c(0,eq4PSLK[[1]][[2]][1:4],1)
  # generates 3-strategy eq vectors 
  eq5RPSout <- unlist(c(eq4RPSL[[1]][[3]][2,1:4],0,1)); eq5RPLout <- unlist(c(eq4RPSL[[1]][[3]][3,1:4],0,1))
  eq5RPKout <- unlist(c(eq4RPSK[[1]][[3]][3,1:3],0,eq4RPSK[[1]][[3]][3,4],1)); eq5RSLout <- unlist(c(eq4RSLK[[1]][[3]][2,1],0,eq4RSLK[[1]][[3]][2,2:4],1))
  eq5RSKout <- unlist(c(eq4RPSK[[1]][[3]][4,1:3],0,eq4RPSK[[1]][[3]][3,4],1)); eq5RLKout <- unlist(c(eq4RPLK[[1]][[3]][4,1:2],0,eq4RPLK[[1]][[3]][4,3:4],1))
  eq5PSLout <- unlist(c(0,eq4PSLK[[1]][[3]][2,1:4],1)); eq5PSKout <- unlist(c(0,eq4PSLK[[1]][[3]][3,1:4],1))
  eq5PLKout <- unlist(c(0,eq4PSLK[[1]][[3]][4,1:4],1)); eq5SLKout <- unlist(c(0,eq4PSLK[[1]][[3]][5,1:4],1))
  # generates 2-strategy eq vectors 
  eq5RPout <- unlist(c(eq4RPSL[[1]][[3]][6,1:4],0,1)); eq5RSout <- unlist(c(eq4RPSL[[1]][[3]][7,1:4],0,1))
  eq5RLout <- unlist(c(eq4RPSL[[1]][[3]][8,1:4],0,1)); eq5RKout <- unlist(c(eq4RSLK[[1]][[3]][8,1],0,eq4RSLK[[1]][[3]][8,2:4],1))
  eq5PSout <- unlist(c(eq4RPSL[[1]][[3]][9,1:4],0,1)); eq5PLout <- unlist(c(eq4RPSL[[1]][[3]][10,1:4],0,1))
  eq5PKout <- unlist(c(0,eq4PSLK[[1]][[3]][8,1:4],1)); eq5SLout <- unlist(c(eq4RPSL[[1]][[3]][11,1:4],0,1))
  eq5SKout <- unlist(c(0,eq4PSLK[[1]][[3]][10,1:4],1)); eq5LKout <- unlist(c(0,eq4PSLK[[1]][[3]][11,1:4],1))
  # solves 5(4) partial games
  eq5RPSL <- cont5Classify(eq5RPSLout, Wmatrix, zero, vars); eq5RPSK <- cont5Classify(eq5RPSKout, Wmatrix, zero, vars)
  eq5RPLK <- cont5Classify(eq5RPLKout, Wmatrix, zero, vars); eq5RSLK <- cont5Classify(eq5RSLKout, Wmatrix, zero, vars)
  eq5PSLK <- cont5Classify(eq5PSLKout, Wmatrix, zero, vars)
  # solves 5(3) partial games
  eq5RPS <- cont5Classify(eq5RPSout, Wmatrix, zero, vars); eq5RPL <- cont5Classify(eq5RPLout, Wmatrix, zero, vars)
  eq5RPK <- cont5Classify(eq5RPKout, Wmatrix, zero, vars); eq5RSL <- cont5Classify(eq5RSLout, Wmatrix, zero, vars)
  eq5RSK <- cont5Classify(eq5RSKout, Wmatrix, zero, vars); eq5RLK <- cont5Classify(eq5RLKout, Wmatrix, zero, vars)
  eq5PSL <- cont5Classify(eq5PSLout, Wmatrix, zero, vars); eq5PSK <- cont5Classify(eq5PSKout, Wmatrix, zero, vars)
  eq5PLK <- cont5Classify(eq5PLKout, Wmatrix, zero, vars); eq5SLK <- cont5Classify(eq5SLKout, Wmatrix, zero, vars)
  # solves 5(2) partial games
  eq5RP <- cont5Classify(eq5RPout, Wmatrix, zero, vars); eq5RS <- cont5Classify(eq5RSout, Wmatrix, zero, vars)
  eq5RL <- cont5Classify(eq5RLout, Wmatrix, zero, vars); eq5RK <- cont5Classify(eq5RKout, Wmatrix, zero, vars)
  eq5PS <- cont5Classify(eq5PSout, Wmatrix, zero, vars); eq5PL <- cont5Classify(eq5PLout, Wmatrix, zero, vars)
  eq5PK <- cont5Classify(eq5PKout, Wmatrix, zero, vars); eq5SL <- cont5Classify(eq5SLout, Wmatrix, zero, vars)
  eq5SK <- cont5Classify(eq5SKout, Wmatrix, zero, vars); eq5LK <- cont5Classify(eq5LKout, Wmatrix, zero, vars)
  # full game equilibrium table with intransitivity, apostasis, anti-apostasis
  RPSLK_Eq <- data.frame("R" = c(eq5out[1], eq5RPSLout[1], eq5RPSKout[1], eq5RPLKout[1], eq5RSLKout[1], eq5PSLKout[1],
                                 eq5RPSout[1], eq5RPLout[1], eq5RPKout[1], eq5RSLout[1], eq5RSKout[1], eq5RLKout[1], eq5PSLout[1], eq5PSKout[1], eq5PLKout[1], eq5SLKout[1], 
                                 eq5RPout[1], eq5RSout[1], eq5RLout[1], eq5RKout[1], eq5PSout[1], eq5PLout[1], eq5PKout[1], eq5SLout[1], eq5SKout[1], eq5LKout[1]), 
                         "P" = c(eq5out[2], eq5RPSLout[2], eq5RPSKout[2], eq5RPLKout[2], eq5RSLKout[2], eq5PSLKout[2],
                                 eq5RPSout[2], eq5RPLout[2], eq5RPKout[2], eq5RSLout[2], eq5RSKout[2], eq5RLKout[2], eq5PSLout[2], eq5PSKout[2], eq5PLKout[2], eq5SLKout[2], 
                                 eq5RPout[2], eq5RSout[2], eq5RLout[2], eq5RKout[2], eq5PSout[2], eq5PLout[2], eq5PKout[2], eq5SLout[2], eq5SKout[2], eq5LKout[2]), 
                         "S" = c(eq5out[3], eq5RPSLout[3], eq5RPSKout[3], eq5RPLKout[3], eq5RSLKout[3], eq5PSLKout[3],
                                 eq5RPSout[3], eq5RPLout[3], eq5RPKout[3], eq5RSLout[3], eq5RSKout[3], eq5RLKout[3], eq5PSLout[3], eq5PSKout[3], eq5PLKout[3], eq5SLKout[3], 
                                 eq5RPout[3], eq5RSout[3], eq5RLout[3], eq5RKout[3], eq5PSout[3], eq5PLout[3], eq5PKout[3], eq5SLout[3], eq5SKout[3], eq5LKout[3]), 
                         "L" = c(eq5out[4], eq5RPSLout[4], eq5RPSKout[4], eq5RPLKout[4], eq5RSLKout[4], eq5PSLKout[4],
                                 eq5RPSout[4], eq5RPLout[4], eq5RPKout[4], eq5RSLout[4], eq5RSKout[4], eq5RLKout[4], eq5PSLout[4], eq5PSKout[4], eq5PLKout[4], eq5SLKout[4], 
                                 eq5RPout[4], eq5RSout[4], eq5RLout[4], eq5RKout[4], eq5PSout[4], eq5PLout[4], eq5PKout[4], eq5SLout[4], eq5SKout[4], eq5LKout[4]), 
                         "K" = c(eq5out[5], eq5RPSLout[5], eq5RPSKout[5], eq5RPLKout[5], eq5RSLKout[5], eq5PSLKout[5],
                                 eq5RPSout[5], eq5RPLout[5], eq5RPKout[5], eq5RSLout[5], eq5RSKout[5], eq5RLKout[5], eq5PSLout[5], eq5PSKout[5], eq5PLKout[5], eq5SLKout[5], 
                                 eq5RPout[5], eq5RSout[5], eq5RLout[5], eq5RKout[5], eq5PSout[5], eq5PLout[5], eq5PKout[5], eq5SLout[5], eq5SKout[5], eq5LKout[5]),
                         "Eq" = c(classed[[3]], eq5RPSL[[3]], eq5RPSK[[3]], eq5RPLK[[3]], eq5RSLK[[3]], eq5PSLK[[3]],
                                  eq5RPS[[3]], eq5RPL[[3]], eq5RPK[[3]], eq5RSL[[3]], eq5RSK[[3]], eq5RLK[[3]], eq5PSL[[3]], eq5PSK[[3]], eq5PLK[[3]], eq5SLK[[3]], 
                                  eq5RP[[3]], eq5RS[[3]], eq5RL[[3]], eq5RK[[3]], eq5PS[[3]], eq5PL[[3]], eq5PK[[3]], eq5SL[[3]], eq5SK[[3]], eq5LK[[3]]))
  colnames(RPSLK_Eq) <- c(vars[1], vars[2], vars[3], vars[4], vars[5], "Eq")
  rownames(RPSLK_Eq) <- c(paste(svars[1],svars[2],svars[3],svars[4],svars[5],sep="."), 
                         paste(svars[1],svars[2],svars[3],svars[4],"0",sep="."), paste(svars[1],svars[2],svars[3],"0",svars[5],sep="."), paste(svars[1],svars[2],"0",svars[4],svars[5],sep="."), 
                         paste(svars[1],"0",svars[3],svars[4],svars[5],sep="."), paste("0",svars[2],svars[3],svars[4],svars[5],sep="."),
                         paste(svars[1],svars[2],svars[3],"0","0",sep="."), paste(svars[1],svars[2],"0",svars[4],"0",sep="."), paste(svars[1],svars[2],"0","0",svars[5],sep="."), 
                         paste(svars[1],"0",svars[3],svars[4],"0",sep="."), paste(svars[1],"0",svars[3],"0",svars[5],sep="."), paste(svars[1],"0","0",svars[4],svars[5],sep="."),
                         paste("0",svars[2],svars[3],svars[4],"0",sep="."), paste("0",svars[2],svars[3],"0",svars[5],sep="."), paste("0",svars[2],"0",svars[4],svars[5],sep="."), 
                         paste("0","0",svars[3],svars[4],svars[5],sep="."), 
                         paste(svars[1],svars[2],"0","0","0",sep="."), paste(svars[1],"0",svars[3],"0","0",sep="."), paste(svars[1],"0","0",svars[4],"0",sep="."), 
                         paste(svars[1],"0","0","0",svars[5],sep="."), paste("0",svars[2],svars[3],"0","0",sep="."), paste("0",svars[2],"0",svars[4],"0",sep="."), 
                         paste("0",svars[2],"0","0",svars[5],sep="."), paste("0","0",svars[3],svars[4],"0",sep="."), paste("0","0",svars[3],"0",svars[5],sep="."), 
                         paste("0","0","0",svars[4],svars[5],sep="."))
  Int <- c(testIntransitive(Wmatrix), testIntransitive(Wmatrix[-5,-5]), testIntransitive(Wmatrix[-4,-4]), testIntransitive(Wmatrix[-3,-3]), testIntransitive(Wmatrix[-2,-2]), testIntransitive(Wmatrix[-1,-1]),
           testIntransitive(Wmatrix[-c(4,5),-c(4,5)]), testIntransitive(Wmatrix[-c(3,5),-c(3,5)]), testIntransitive(Wmatrix[-c(3,4),-c(3,4)]), testIntransitive(Wmatrix[-c(2,5),-c(2,5)]), testIntransitive(Wmatrix[-c(2,4),-c(2,4)]),
           testIntransitive(Wmatrix[-c(2,3),-c(2,3)]), testIntransitive(Wmatrix[-c(1,5),-c(1,5)]), testIntransitive(Wmatrix[-c(1,4),-c(1,4)]), testIntransitive(Wmatrix[-c(1,3),-c(1,3)]), testIntransitive(Wmatrix[-c(1,2),-c(1,2)]), rep("",10))
  Apo <- c(testApostatic(Wmatrix), testApostatic(Wmatrix[-5,-5], svars), testApostatic(Wmatrix[-4,-4], svars), testApostatic(Wmatrix[-3,-3], svars), testApostatic(Wmatrix[-2,-2], svars), testApostatic(Wmatrix[-1,-1], svars),
           testApostatic(Wmatrix[-c(4,5),-c(4,5)], svars), testApostatic(Wmatrix[-c(3,5),-c(3,5)], svars), testApostatic(Wmatrix[-c(3,4),-c(3,4)], svars), testApostatic(Wmatrix[-c(2,5),-c(2,5)], svars), testApostatic(Wmatrix[-c(2,4),-c(2,4)], svars),
           testApostatic(Wmatrix[-c(2,3),-c(2,3)], svars), testApostatic(Wmatrix[-c(1,5),-c(1,5)], svars), testApostatic(Wmatrix[-c(1,4),-c(1,4)], svars), testApostatic(Wmatrix[-c(1,3),-c(1,3)], svars), testApostatic(Wmatrix[-c(1,2),-c(1,2)], svars), rep("",10))
  A_Apo <- c(testAntiApostatic(Wmatrix), testAntiApostatic(Wmatrix[-5,-5], svars), testAntiApostatic(Wmatrix[-4,-4], svars), testAntiApostatic(Wmatrix[-3,-3], svars), testAntiApostatic(Wmatrix[-2,-2], svars), testAntiApostatic(Wmatrix[-1,-1], svars),
           testAntiApostatic(Wmatrix[-c(4,5),-c(4,5)], svars), testAntiApostatic(Wmatrix[-c(3,5),-c(3,5)], svars), testAntiApostatic(Wmatrix[-c(3,4),-c(3,4)], svars), testAntiApostatic(Wmatrix[-c(2,5),-c(2,5)], svars), testAntiApostatic(Wmatrix[-c(2,4),-c(2,4)], svars),
           testAntiApostatic(Wmatrix[-c(2,3),-c(2,3)], svars), testAntiApostatic(Wmatrix[-c(1,5),-c(1,5)], svars), testAntiApostatic(Wmatrix[-c(1,4),-c(1,4)], svars), testAntiApostatic(Wmatrix[-c(1,3),-c(1,3)], svars), testAntiApostatic(Wmatrix[-c(1,2),-c(1,2)], svars), rep("",10))
  RPSLK_Eq <- cbind(RPSLK_Eq, Int, Apo, A_Apo)
  table_full <- list(W_matrix = classed[[1]], Eq5 = classed[[2]], Equilibria = RPSLK_Eq)
  # assemble eigendecomposition list for full game
  Eig <- list(classed[[4]], eq5RPSL[[4]], eq5RPSK[[4]], eq5RPLK[[4]], eq5RSLK[[4]], eq5PSLK[[4]],
              eq5RPS[[4]], eq5RPL[[4]], eq5RPK[[4]], eq5RSL[[4]], eq5RSK[[4]], eq5RLK[[4]], eq5PSL[[4]], eq5PSK[[4]], eq5PLK[[4]], eq5SLK[[4]],
              eq5RP[[4]], eq5RS[[4]], eq5RL[[4]], eq5RK[[4]], eq5PS[[4]], eq5PL[[4]], eq5PK[[4]], eq5SL[[4]], eq5SK[[4]], eq5LK[[4]]) 
  names(Eig) <- c(paste(svars[1],svars[2],svars[3],svars[4],svars[5],sep="."), 
                  paste(svars[1],svars[2],svars[3],svars[4],0,sep="."), paste(svars[1],svars[2],svars[3],0,svars[5],sep="."), paste(svars[1],svars[2],0,svars[4],svars[5],sep="."), paste(svars[1],0,svars[3],svars[4],svars[5],sep="."), paste(0,svars[2],svars[3],svars[4],svars[5],sep="."),  
                  paste(svars[1],svars[2],svars[3],"0.0",sep="."), paste(svars[1],svars[2],0,svars[4],0,sep="."), paste(svars[1],svars[2],"0.0",svars[5],sep="."), paste(svars[1],0,svars[3],svars[4],0,sep="."), paste(svars[1],0,svars[3],0,svars[5],sep="."), 
                  paste(svars[1],"0.0",svars[4],svars[5],sep="."), paste(0,svars[2],svars[3],svars[4],0,sep="."), paste(0,svars[2],svars[3],0,svars[5],sep="."), paste(0,svars[2],0,svars[4],svars[5],sep="."), paste("0.0",svars[3],svars[4],svars[5],sep="."), 
                  paste(svars[1],svars[2],"0.0.0",sep="."), paste(svars[1],0,svars[3],"0.0",sep="."), paste(svars[1],"0.0",svars[4],0,sep="."), paste(svars[1],"0.0.0",svars[5],sep="."), paste(0,svars[2],svars[3],"0.0",sep="."), 
                  paste(0,svars[2],0,svars[4],0,sep="."), paste(0,svars[2],"0.0",svars[5],sep="."), paste("0.0",svars[3],svars[4],0,sep="."), paste("0.0",svars[3],0,svars[5],sep="."), paste("0.0.0",svars[4],svars[5],sep="."))
  # 4-face games equilibrium table
  table_4faces <- c(eq4RPSL[[1]][3], eq4RPSK[[1]][3], eq4RPLK[[1]][3], eq4RSLK[[1]][3], eq4PSLK[[1]][3])
  names(table_4faces) <- c(paste(svars[1],svars[2],svars[3],svars[4],"_",sep="."), paste(svars[1],svars[2],svars[3],"_",svars[5],sep="."), paste(svars[1],svars[2],"_",svars[4],svars[5],sep="."), paste(svars[1],"_",svars[3],svars[4],svars[5],sep="."), paste("_",svars[2],svars[3],svars[4],svars[5],sep="."))
  Eig_4Edges = c(eq4RPSL[[3]][1], eq4RPSK[[3]][1], eq4RPLK[[3]][1], eq4RSLK[[3]][1], eq4PSLK[[3]][1])
  names(Eig_4Edges) <- names(table_4faces)
  # 3-face games equilibrium table
  table_3faces <- c(eq4RPSL[[2]][[1]][1], eq4RPSL[[2]][[1]][2], eq4RPSK[[2]][[1]][2], eq4RPSL[[2]][[1]][3], eq4RPSK[[2]][[1]][3],
                    eq4RSLK[[2]][[1]][3], eq4PSLK[[2]][[1]][1], eq4PSLK[[2]][[1]][2], eq4PSLK[[2]][[1]][3], eq4PSLK[[2]][[1]][4])
  names(table_3faces) <- c(paste(svars[1],svars[2],svars[3],"_","_",sep="."), paste(svars[1],svars[2],"_",svars[4],"_",sep="."), paste(svars[1],svars[2],"_","_",svars[5],sep="."), paste(svars[1],"_",svars[3],svars[4],"_",sep="."), paste(svars[1],"_",svars[3],"_",svars[5],sep="."), 
                           paste(svars[1],"_","_",svars[4],svars[5],sep="."), paste("_",svars[2],svars[3],svars[4],"_",sep="."), paste("_",svars[2],svars[3],"_",svars[5],sep="."), paste("_",svars[2],"_",svars[4],svars[5],sep="."), paste("_","_",svars[3],svars[4],svars[5],sep="."))
  Eig_3Edges = c(eq4RPSL[[3]][[2]][1], eq4RPSL[[3]][[2]][2], eq4RPSK[[3]][[2]][2], eq4RPSL[[3]][[2]][3], eq4RPSK[[3]][[2]][3],
                 eq4RSLK[[3]][[2]][3], eq4PSLK[[3]][[2]][1], eq4PSLK[[3]][[2]][2], eq4PSLK[[3]][[2]][3], eq4PSLK[[3]][[2]][4])
  names(Eig_3Edges) <- names(table_3faces)
  # 2-face games equilibrium table
  RPSLK2_Eq <- data.frame("R" = c(eq5RPout[1], eq5RSout[1], eq5RLout[1], eq5RKout[1], rep(NA,6)), 
                         "P" = c(eq5RPout[2], rep(NA,3), eq5PSout[2], eq5PLout[2], eq5PKout[2], rep(NA,3)), 
                         "S" = c(NA, eq5RSout[3], rep(NA,2), eq5PSout[3], rep(NA,2), eq5SLout[3], eq5SKout[3], NA),
                         "L" = c(rep(NA,2), eq5RLout[4], rep(NA,2), eq5PLout[4], NA, eq5SLout[4], NA, eq5LKout[4]),
                         "K" = c(rep(NA,3), eq5RKout[5], rep(NA,2), eq5PKout[5], NA, eq5SKout[5], eq5LKout[5]),
                         "Eq" = c(eq4RPSL[[2]][[2]][1,5], eq4RPSL[[2]][[2]][2,5], eq4RPSL[[2]][[2]][3,5], eq4RSLK[[2]][[2]][3,5], eq4PSLK[[2]][[2]][1,5],
                                  eq4PSLK[[2]][[2]][2,5], eq4PSLK[[2]][[2]][3,5], eq4PSLK[[2]][[2]][4,5], eq4PSLK[[2]][[2]][5,5], eq4PSLK[[2]][[2]][6,5]))
  colnames(RPSLK2_Eq) <- c(vars[1],vars[2],vars[3],vars[4],vars[5],"Eq")
  rownames(RPSLK2_Eq) <- c(paste(svars[1],svars[2],"_","_","_",sep="."), paste(svars[1],"_",svars[3],"_","_",sep="."), paste(svars[1],"_","_",svars[4],"_",sep="."), paste(svars[1],"_","_","_",svars[5],sep="."), paste("_",svars[2],svars[3],"_","_",sep="."), 
                           paste("_",svars[2],"_",svars[4],"_",sep="."), paste("_",svars[2],"_","_",svars[5],sep="."), paste("_","_",svars[3],svars[4],"_",sep="."), paste("_","_",svars[3],"_",svars[5],sep="."), paste("_","_","_",svars[4],svars[5],sep="."))
  Eig_2Edges <- c(eq4RPSL[[3]][[3]][1], eq4RPSL[[3]][[3]][2], eq4RPSL[[3]][[3]][3], eq4RSLK[[3]][[3]][3], eq4PSLK[[3]][[3]][1],
                  eq4PSLK[[3]][[3]][2], eq4PSLK[[3]][[3]][3], eq4PSLK[[3]][[3]][4], eq4PSLK[[3]][[3]][5], eq4PSLK[[3]][[3]][6])
  # output table
  table_out <- list(table_full, Faces = list(table_4faces, table_3faces, RPSLK2_Eq), Eig = list(All = Eig, "Faces(4)" = Eig_4Edges, "Faces(3)" = Eig_3Edges, "Faces(2)" = Eig_2Edges))
  names(table_out) <- c(paste(vars[1],vars[2],vars[3],vars[4],vars[5],sep="."), "Face_Equilibria", "Eigendecomposition"); names(table_out[[2]]) = c("Faces(4)", "Faces(3)", "Faces(2)")
  return(table_out)
}