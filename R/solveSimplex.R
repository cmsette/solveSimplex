### Defines function to analyze simplex (handles 2- through 5-players, discrete & continuous)
# W: vector of payoff values, by row
# gentime: defines replicator dynamics, "continuous" or "discrete" time steps
# zero=1e-14: uses a value of 1e-14 instead of 0 in validation steps
# sig_dig=7: default prints output with 7 significant figures
# colnames=c("R", "P", "S", "L", "K"): user can change variable names displayed
# deconstruct=T: output includes analysis of face games as independent games

solveSimplex <- function(W, gen_time="continuous", zero=1e-14, sig_dig=4, names=c("R", "P", "S", "L", "K"), deconstruct=T, debug=F){
  # tests for numerical W, puts W in matrix format, check variable names vector length
  if(sqrt(length(W)) %in% c(2,3,4,5)){
    if(class(W)[1] == "numeric" && sapply(W, FUN = function(x) is.numeric(x)==T)){Wmatrix = matrix(W, nrow=sqrt(length(W)), byrow=T)} 
    else if(class(W)[1] == "matrix" && mapply(W, FUN = function(x) is.numeric(x)==T)){Wmatrix = W} 
    else{stop("non-numeric payoff values")}
  } else{stop("simplex dimension ")}
  if(length(names) < sqrt(length(W))){names=c(names, c("R","P","S","L","K")[1:(sqrt(length(W))-length(names))])}
  if(length(names) > sqrt(length(W))){names=names[1:sqrt(length(W))]}
  # analyzes game by number of competitors  
  if(sqrt(length(W)) == 2){
    table_out <- table2(Wmatrix, gen_time, zero, sig_dig, names)
    if(debug==F){ if(deconstruct==T){return(table_out[-c(2,4)])}else{return(table_out[-c(2,4)])} }
    else{ if(deconstruct==T){return(table_out[-2])}else{return(table_out[-2])} }
  }
  if(sqrt(length(W)) == 3){
    table_out <- table3(Wmatrix, gen_time, zero, sig_dig, names)
    if(debug==F){ 
      if(deconstruct==T){return(c(table_out[[1]][1], table_out[[1]][3], table_out[2]))}
      else{return(c(table_out[[1]][1], table_out[[1]][3]))} }
    else{ 
      if(deconstruct==T){return(c(table_out[[1]][1], table_out[[1]][3], table_out[2], table_out[[3]]))}
      else{return(c(table_out[[1]][1], table_out[[1]][3], table_out[[3]][1]))} }
  }
  if(sqrt(length(W)) == 4){
    table_out <- table4(Wmatrix, gen_time, zero, sig_dig, names)
    if(debug==F){ 
      if(deconstruct==T){return(c(table_out[[1]][1], table_out[[1]][3], table_out[2]))}
      else{return(c(table_out[[1]][1], table_out[[1]][3]))} }
    else{ 
      if(deconstruct==T){return(c(table_out[[1]][1], table_out[[1]][3], table_out[2], table_out[[3]]))}
      else{return(c(table_out[[1]][1], table_out[[1]][3], table_out[[3]][1]))} }
  }
  if(sqrt(length(W)) == 5){
    table_out <- table5(Wmatrix, gen_time, zero, sig_dig, names)
    if(debug==F){ 
      if(deconstruct==T){return(c(table_out[[1]][1], table_out[[1]][3], table_out[2]))}
      else{return(c(table_out[[1]][1], table_out[[1]][3]))} }
    else{ 
      if(deconstruct==T){return(c(table_out[[1]][1], table_out[[1]][3], table_out[2], table_out[[3]]))}
      else{return(c(table_out[[1]][1], table_out[[1]][3], table_out[[3]][1]))} }
  }
}