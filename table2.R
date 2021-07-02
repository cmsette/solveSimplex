### Function to generate solution tables (2--players), called by solveSimplex
table2 <- function(Wmatrix, gen_time, zero, sig_dig, vars){
  # solves 2-strategy equilibrium
  eq2out <- eq2(Wmatrix)
  colnames(Wmatrix) <- rownames(Wmatrix) <- vars
  if(is.na(eq2out[1])){classed <- list(W = Wmatrix, Eq = eq2out[1:2], Class = "None", Eig = NA)}else 
  if(eq2out[1] == 1){classed <- list(W = Wmatrix, Eq = eq2out[1:2], Class = "Sink", Eig = NA)}else 
  if(eq2out[2] == 1){classed <- list(W = Wmatrix, Eq = eq2out[1:2], Class = "Sink", Eig = NA)}else{
    if(gen_time == "continuous"){classed <- cont2Classify(eq2out, Wmatrix, zero, vars)}
    else if(gen_time == "discrete"){stop("discrete generation time not yet implemented")}
    else{stop("invalid generation time")}
  }
  # output array
  RP_Eq <- data.frame("R"=classed[[2]][1], "P"=classed[[2]][2], "Eq"=classed[[3]])
  row.names(RP_Eq) <- c(paste(vars[1], vars[2], sep="."))
  colnames(RP_Eq) <- c(vars[1],vars[2],"Eq")
  table_out <- list(classed[[1]], classed[[2]], RP_Eq, classed[[4]])
  names(table_out) <- c("W", "Eq", paste(vars[1],vars[2],sep="."), "Eigendecomposition")
  return(table_out)
}