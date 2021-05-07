### Defines function to analyze simplex (handles 2- through 5-players, discrete & continuous)
# W: vector of payoff values, by row
# gentime: defines replicator dynamics, "continuous" or "discrete" time steps
# zero=1e-14: uses a value of 1e-14 instead of 0 in validation steps
# sig_dig=7: default prints output with 7 significant figures
# deconstruct=T: output includes independednt analysis of face games

solveSimplex<-function(W, gen_time="continuous", zero=1e-14, sig_dig=7, deconstruct=T){
  options(warn=-1)
  if(length(W) == 4){
    W_out<-matrix(W, ncol=2, byrow=T); rownames(W_out)<-colnames(W_out)<-c("R", "P")
    table_out<-table2(W, gen_time, zero, sig_dig, "RP")
    return(list(W=W_out, gen_time=gen_time, table=table_out))
  }
  else if(length(W) == 9){
    W_out<-matrix(W, ncol=3, byrow=T); rownames(W_out)<-colnames(W_out)<-c("R", "P", "S")
    if(deconstruct == T){
      table_out<-table3(W, gen_time, zero, sig_dig)
      table_out_RP<-table2(W[c(1,2, 4,5)], gen_time, zero, sig_dig, "RP")
      table_out_RS<-table2(W[c(1,3, 7,9)], gen_time, zero, sig_dig, "RS")
      table_out_PS<-table2(W[c(5,6, 8,9)], gen_time, zero, sig_dig, "PS")
      table_out_2<-rbind(table_out_RP, table_out_RS, table_out_PS)
      colnames(table_out_2)<-c("S1","S2","Equilibrium")
      return(list(W=W_out, gen_time=gen_time, table=table_out, edges=table_out_2))
    }else{
      table_out<-table3(W, gen_time, zero, sig_dig)
      return(list(W=W_out, gen_time=gen_time, table=table_out))
    }
  }
  else if(length(W) == 16){
    W_out<-matrix(W, ncol=4, byrow=T); rownames(W_out)<-colnames(W_out)<-c("R", "P", "S", "L")
    if(deconstruct == T){
      table_out<-table4(W, gen_time, zero, sig_dig)
      table_out_RPS<-table3(W[c(1,2,3, 5,6,7, 9,10,11)], gen_time, zero, sig_dig, "RPS")
      table_out_RPL<-table3(W[c(1,2,4, 5,6,8, 13,14,16)], gen_time, zero, sig_dig, "RPL")
      table_out_RSL<-table3(W[c(1,3,4, 9,11,12, 13,15,16)], gen_time, zero, sig_dig, "RSL")
      table_out_PSL<-table3(W[c(6,7,8, 10,11,12, 14,15,16)], gen_time, zero, sig_dig, "PSL")
      table_out_RP<-table2(W[c(1,2, 5,6)], gen_time, zero, sig_dig, "RP")
      table_out_RS<-table2(W[c(1,3, 9,11)], gen_time, zero, sig_dig, "RS")
      table_out_RL<-table2(W[c(1,4, 13,16)], gen_time, zero, sig_dig, "RL")
      table_out_PS<-table2(W[c(6,7, 10,11)], gen_time, zero, sig_dig, "PS")
      table_out_PL<-table2(W[c(6,8, 14,16)], gen_time, zero, sig_dig, "PL")
      table_out_SL<-table2(W[c(11,12, 15,16)], gen_time, zero, sig_dig, "SL")
      table_out_2<-rbind(table_out_RP, table_out_RS, table_out_RL, table_out_PS, table_out_PL, table_out_SL)
      colnames(table_out_2)<-c("S1","S2","Equilibrium")
      return(list(W=W_out, gen_time=gen_time, table=table_out, RPS=table_out_RPS, RPL=table_out_RPL, RSL=table_out_RSL, 						PSL=table_out_PSL, edges=table_out_2))
    }else{
      table_out<-table4(W, gen_time, zero, sig_dig)
      return(list(W=W_out, gen_time=gen_time, table=table_out))
    }
  }
  else if(length(W) == 25){
    W_out<-matrix(as.numeric(W), ncol=5, byrow=T); rownames(W_out)<-colnames(W_out)<-c("R", "P", "S", "L", "K")
    if(deconstruct == T){
      table_out<-table5(W, gen_time, zero, sig_dig)
      table_out_RPSL<-table4(W[c(1,2,3,4, 6,7,8,9, 11,12,13,14, 16,17,18,19)], gen_time, zero, sig_dig, "RPSL")
      table_out_RPSK<-table4(W[c(1,2,3,5, 6,7,8,10, 11,12,13,15, 21,22,23,25)], gen_time, zero, sig_dig, "RPSK")
      table_out_RPLK<-table4(W[c(1,2,4,5, 6,7,9,10, 16,17,19,20, 21,22,24,25)], gen_time, zero, sig_dig, "RPLK")
      table_out_RSLK<-table4(W[c(1,3,4,5, 11,13,14,15, 16,18,19,20, 21,23,24,25)], gen_time, zero, sig_dig, "RSLK")
      table_out_PSLK<-table4(W[c(7,8,9,10, 12,13,14,15, 17,18,19,20, 22,23,24,25)], gen_time, zero, sig_dig, "PSLK")
      table_out_RPS<-table3(W[c(1,2,3, 6,7,8, 11,12,13)], gen_time, zero, sig_dig, "RPS")
      table_out_RPL<-table3(W[c(1,2,4, 6,7,9, 16,17,19)], gen_time, zero, sig_dig, "RPL")
      table_out_RPK<-table3(W[c(1,2,5, 6,7,10, 21,22,25)], gen_time, zero, sig_dig, "RPK")
      table_out_RSL<-table3(W[c(1,3,4, 11,13,14, 16,18,19)], gen_time, zero, sig_dig, "RSL")
      table_out_RSK<-table3(W[c(1,3,5, 11,13,15, 21,23,25)], gen_time, zero, sig_dig, "RSK")
      table_out_RLK<-table3(W[c(1,4,5, 16,19,20, 21,24,25)], gen_time, zero, sig_dig, "RLK")
      table_out_PSL<-table3(W[c(7,8,9, 12,13,14, 17,18,19)], gen_time, zero, sig_dig, "PSL")
      table_out_PSK<-table3(W[c(7,8,10, 12,13,15, 22,23,25)], gen_time, zero, sig_dig, "PSK")
      table_out_PLK<-table3(W[c(7,9,10, 17,19,20, 22,24,25)], gen_time, zero, sig_dig, "PLK")
      table_out_SLK<-table3(W[c(13,14,15, 18,19,20, 23,24,25)], gen_time, zero, sig_dig, "SLK")
      table_out_RP<-table2(W[c(1,2, 6,7)], gen_time, zero, sig_dig, "RP")
      table_out_RS<-table2(W[c(1,3, 11,13)], gen_time, zero, sig_dig, "RS")
      table_out_RL<-table2(W[c(1,4, 16,19)], gen_time, zero, sig_dig, "RL")
      table_out_RK<-table2(W[c(1,5, 21,25)], gen_time, zero, sig_dig, "RK")
      table_out_PS<-table2(W[c(7,8, 12,13)], gen_time, zero, sig_dig, "PS")
      table_out_PL<-table2(W[c(7,9, 17,19)], gen_time, zero, sig_dig, "PL")
      table_out_PK<-table2(W[c(7,10, 22,25)], gen_time, zero, sig_dig, "PK")
      table_out_SL<-table2(W[c(13,14, 18,19)], gen_time, zero, sig_dig, "SL")
      table_out_SK<-table2(W[c(13,15, 23,25)], gen_time, zero, sig_dig, "SK")
      table_out_LK<-table2(W[c(19,20, 24,25)], gen_time, zero, sig_dig, "LK")
      table_out_2<-rbind(table_out_RP, table_out_RS, table_out_RL, table_out_RK, table_out_PS, table_out_PL, table_out_PK, 						table_out_SL, table_out_SK, table_out_LK)
      colnames(table_out_2)<-c("S1","S2","Equilibrium")
      return(list(W=W_out, gen_time=gen_time, table=table_out, RPSL=table_out_RPSL, 	RPSK=table_out_RPSK, RPLK=table_out_RPLK, 					RSLK=table_out_RSLK, PSLK=table_out_PSLK, RPS=table_out_RPS, RPL=table_out_RPL, RPK=table_out_RPK, RSL=table_out_RSL, 					RSK=table_out_RSK, RLK=table_out_RLK, PSL=table_out_PSL, PSK=table_out_PSK, PLK=table_out_PLK, SLK=table_out_SLK, 						edges=table_out_2))
    }else{
      table_out<-table5(W, gen_time, zero, sig_dig)
      return(list(W=W_out, gen_time=gen_time, table=table_out))
    }
  }
  else{print("Error in simplex dimension")}
  options(warn=0)
}