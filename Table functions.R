### Functions to generate tables (2- through 5-players)
# Called by solveSimplex
table2<-function(W, gen_time, zero, sig_dig, vars="RP"){
  WRP<-matrix(as.numeric(W), ncol=2, byrow=T)
  out_2simp<-test2(WRP)
  if(gen_time == "continuous"){Equilibrium<-cont2Classify(out_2simp, W, zero)}
  else if(gen_time == "discrete"){Equilibrium<-disc2Classify(out_2simp, W, zero)}
  else{print("Invalid generation time")}
  if(is.na(out_2simp[1]) == T){}else if(out_2simp[1] == 1){Equilibrium<-substring(vars,1,1)}else if(out_2simp[2] == 1){Equilibrium<-substring(vars,2)}
  table_out_2<-matrix(c(format(out_2simp[1:2], digits=sig_dig), Equilibrium), ncol=3)
  rownames(table_out_2)<-c(vars)
  colnames(table_out_2)<-c("R","P","Equilibrium")
  table_out_2<-as.data.frame(table_out_2)
  return(table_out_2)
}
table3<-function(W, gen_time, zero, sig_dig, vars="RPS"){
  WRPS<-matrix(as.numeric(W), ncol=3, byrow=T)
  WRP<-matrix(W[c(1,2, 4,5)], ncol=2, byrow=T)
  WRS<-matrix(W[c(1,3, 7,9)], ncol=2, byrow=T)
  WPS<-matrix(W[c(5,6, 8,9)], ncol=2, byrow=T)
  out_3simp<-test3(WRPS, zero)
  out_3simpRP<-test2(WRP); out_3simpRS<-test2(WRS); out_3simpPS<-test2(WPS)
  if(is.na(out_3simpRP[3]) == T || out_3simpRP[3] == 0){out_3simpRP[1:2]<-c(NA, NA)}
  if(is.na(out_3simpRS[3]) == T || out_3simpRS[3] == 0){out_3simpRS[1:2]<-c(NA, NA)}
  if(is.na(out_3simpPS[3]) == T || out_3simpPS[3] == 0){out_3simpPS[1:2]<-c(NA, NA)}
  eq_3simp<-matrix(c(out_3simp, out_3simpRP[1:2],0,out_3simpRP[3], out_3simpRS[1],0,out_3simpRS[2:3], 0,out_3simpPS[1:3], 1,0,0,1, 		0,1,0,1, 0,0,1,1), nrow=7, ncol=4, byrow=T)
  rownames(eq_3simp)<-c(vars, substring(vars,1,2), paste(substring(vars,c(1,3),c(1,3)), collapse=""), substring(vars,2,3), 				substring(vars,1,1), substring(vars,2,2), substring(vars,3,3))
  colnames(eq_3simp)<-c(substring(vars,1,1), substring(vars,2,2), substring(vars,3,3),"Stability")
  if(gen_time == "continuous"){Equilibrium<-apply(eq_3simp, 1, cont3Classify, W, zero)}
  else if(gen_time == "discrete"){Equilibrium<-apply(eq_3simp, 1, disc3Classify, W, zero)}
  else{print("Invalid generation time")}
  Neg_frequency<-c(testApostatic(WRPS, c("R","P","S")),NA,NA,NA,NA,NA,NA)
  Pos_frequency<-c(testAntiApostatic(WRPS, c("R","P","S")),NA,NA,NA,NA,NA,NA)
  Intransitivity<-c(testIntransitive(WRPS),NA,NA,NA,NA,NA,NA)
  table_out_3<-data.frame(format(eq_3simp[,c(1:3)], digits=sig_dig), Equilibrium, Neg_frequency, Pos_frequency, Intransitivity)
}
table4<-function(W, gen_time, zero, sig_dig, vars="RPSL"){
  WRPSL<-matrix(as.numeric(W), ncol=4, byrow=T)
  WRPS<-matrix(W[c(1,2,3, 5,6,7, 9,10,11)], ncol=3, byrow=T)
  WRPL<-matrix(W[c(1,2,4, 5,6,8, 13,14,16)], ncol=3, byrow=T)
  WRSL<-matrix(W[c(1,3,4, 9,11,12, 13,15,16)], ncol=3, byrow=T)
  WPSL<-matrix(W[c(6,7,8, 10,11,12, 14,15,16)], ncol=3, byrow=T)
  WRP<-matrix(W[c(1,2, 5,6)], ncol=2, byrow=T)
  WRS<-matrix(W[c(1,3, 9,11)], ncol=2, byrow=T)
  WRL<-matrix(W[c(1,4, 13,16)], ncol=2, byrow=T)
  WPS<-matrix(W[c(6,7, 10,11)], ncol=2, byrow=T)
  WPL<-matrix(W[c(6,8, 14,16)], ncol=2, byrow=T)
  WSL<-matrix(W[c(11,12, 15,16)], ncol=2, byrow=T)
  out_4simp<-test4(WRPSL, zero)
  out_4simpRPS<-test3(WRPS, zero); out_4simpRPL<-test3(WRPL, zero); out_4simpRSL<-test3(WRSL, zero); out_4simpPSL<-test3(WPSL, zero)
  out_4simpRP<-test2(WRP); out_4simpRS<-test2(WRS); out_4simpRL<-test2(WRL); out_4simpPS<-test2(WPS); out_4simpPL<-test2(WPL); 		out_4simpSL<-test2(WSL)
  if(is.na(out_4simpRP[3]) == T || out_4simpRP[3] == 0){out_4simpRP[1:2]<-c(NA, NA)}
  if(is.na(out_4simpRS[3]) == T || out_4simpRS[3] == 0){out_4simpRS[1:2]<-c(NA, NA)}
  if(is.na(out_4simpRL[3]) == T || out_4simpRL[3] == 0){out_4simpRL[1:2]<-c(NA, NA)}
  if(is.na(out_4simpPS[3]) == T || out_4simpPS[3] == 0){out_4simpPS[1:2]<-c(NA, NA)}
  if(is.na(out_4simpPL[3]) == T || out_4simpPL[3] == 0){out_4simpPL[1:2]<-c(NA, NA)}
  if(is.na(out_4simpSL[3]) == T || out_4simpSL[3] == 0){out_4simpSL[1:2]<-c(NA, NA)}
  eq_4simp<-matrix(c(out_4simp, out_4simpRPS[1:3],0,out_4simpRPS[4], out_4simpRPL[1:2],0,out_4simpRPL[3:4], 								out_4simpRSL[1],0,out_4simpRSL[2:4], 0,out_4simpPSL, out_4simpRP[1:2],0,0,out_4simpRP[3], 											out_4simpRS[1],0,out_4simpRS[2],0,out_4simpRS[3], out_4simpRL[1],0,0,out_4simpRL[2:3], 0,out_4simpPS[1:2],0,out_4simpPS[3], 		0,out_4simpPL[1],0,out_4simpPL[2:3], 0,0,out_4simpSL[1:3], 1,0,0,0,1, 0,1,0,0,1, 0,0,1,0,1, 0,0,0,1,1), nrow=15, ncol=5, 			byrow=T)
  rownames(eq_4simp)<-c(vars, substring(vars,1,3), paste(substring(vars,c(1:2,4),c(1:2,4)), collapse=""), 								paste(substring(vars,c(1,3:4),c(1,3:4)), collapse=""), substring(vars,2,4), substring(vars,1,2), 									paste(substring(vars,c(1,3),c(1,3)), collapse=""), paste(substring(vars,c(1,4),c(1,4)), collapse=""), substring(vars,2,3), 			paste(substring(vars,c(2,4),c(2,4)), collapse=""), substring(vars,3,4), substring(vars,1,1), substring(vars,2,2), 					substring(vars,3,3), substring(vars,4,4))
  colnames(eq_4simp)<-c(substring(vars,1,1), substring(vars,2,2), substring(vars,3,3), substring(vars,4,4),"Stability")
  if(gen_time == "continuous"){Equilibrium<-apply(eq_4simp, 1, cont4Classify, W, zero)}
  else if(gen_time == "discrete"){Equilibrium<-apply(eq_4simp, 1, disc4Classify, W, zero)}
  else{print("Invalid generation time")}
  Neg_frequency<-c(testApostatic(WRPSL, c("R","P","S","L")), testApostatic(WRPS, c("R","P","S")), 										testApostatic(WRPL, c("R","P","L")), testApostatic(WRSL, c("R","S","L")), 															testApostatic(WPSL, c("P","S","L")),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  Pos_frequency<-c(testAntiApostatic(WRPSL, c("R","P","S","L")), testAntiApostatic(WRPS, c("R","P","S")), 								testAntiApostatic(WRPL, c("R","P","L")), testAntiApostatic(WRSL, c("R","S","L")), 													testAntiApostatic(WPSL, c("P","S","L")),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  Intransitivity<-c(testIntransitive(WRPSL),testIntransitive(WRPS),testIntransitive(WRPL),testIntransitive(WRSL),							testIntransitive(WPSL),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  table_out_4<-data.frame(format(eq_4simp[,c(1:4)], digits=sig_dig), Equilibrium, Neg_frequency, Pos_frequency, Intransitivity)
}
table5<-function(W, gen_time, zero, sig_dig, vars="RPSLK"){
  WRPSLK<-matrix(as.numeric(W), ncol=5, byrow=T)
  WRPSL<-matrix(W[c(1,2,3,4, 6,7,8,9, 11,12,13,14, 16,17,18,19)], ncol=4, byrow=T)
  WRPSK<-matrix(W[c(1,2,3,5, 6,7,8,10, 11,12,13,15, 21,22,23,25)], ncol=4, byrow=T)
  WRPLK<-matrix(W[c(1,2,4,5, 6,7,9,10, 16,17,19,20, 21,22,24,25)], ncol=4, byrow=T)
  WRSLK<-matrix(W[c(1,3,4,5, 11,13,14,15, 16,18,19,20, 21,23,24,25)], ncol=4, byrow=T)
  WPSLK<-matrix(W[c(7,8,9,10, 12,13,14,15, 17,18,19,20, 22,23,24,25)], ncol=4, byrow=T)
  WRPS<-matrix(W[c(1,2,3, 6,7,8, 11,12,13)], ncol=3, byrow=T)
  WRPL<-matrix(W[c(1,2,4, 6,7,9, 16,17,19)], ncol=3, byrow=T)
  WRPK<-matrix(W[c(1,2,5, 6,7,10, 21,22,25)], ncol=3, byrow=T)
  WRSL<-matrix(W[c(1,3,4, 11,13,14, 16,18,19)], ncol=3, byrow=T)
  WRSK<-matrix(W[c(1,3,5, 11,13,15, 21,23,25)], ncol=3, byrow=T)
  WRLK<-matrix(W[c(1,4,5, 16,19,20, 21,24,25)], ncol=3, byrow=T)
  WPSL<-matrix(W[c(7,8,9, 12,13,14, 17,18,19)], ncol=3, byrow=T)
  WPSK<-matrix(W[c(7,8,10, 12,13,15, 22,23,25)], ncol=3, byrow=T)
  WPLK<-matrix(W[c(7,9,10, 17,19,20, 22,24,25)], ncol=3, byrow=T)
  WSLK<-matrix(W[c(13,14,15, 18,19,20, 23,24,25)], ncol=3, byrow=T)
  WRP<-matrix(W[c(1,2, 6,7)], ncol=2, byrow=T)
  WRS<-matrix(W[c(1,3, 11,13)], ncol=2, byrow=T)
  WRL<-matrix(W[c(1,4, 16,19)], ncol=2, byrow=T)
  WRK<-matrix(W[c(1,5, 21,25)], ncol=2, byrow=T)
  WPS<-matrix(W[c(7,8, 12,13)], ncol=2, byrow=T)
  WPL<-matrix(W[c(7,9, 17,19)], ncol=2, byrow=T)
  WPK<-matrix(W[c(7,10, 22,25)], ncol=2, byrow=T)
  WSL<-matrix(W[c(13,14, 18,19)], ncol=2, byrow=T)
  WSK<-matrix(W[c(13,15, 23,25)], ncol=2, byrow=T)
  WLK<-matrix(W[c(19,20, 24,25)], ncol=2, byrow=T)
  out_5simp<-test5(WRPSLK, zero)
  out_5simpRPSL<-test4(WRPSL, zero); out_5simpRPSK<-test4(WRPSK, zero); out_5simpRPLK<-test4(WRPLK, zero); 								out_5simpRSLK<-test4(WRSLK, zero); out_5simpPSLK<-test4(WPSLK, zero)
  out_5simpRPS<-test3(WRPS, zero); out_5simpRPL<-test3(WRPL, zero); out_5simpRPK<-test3(WRPK, zero); out_5simpRSL<-test3(WRSL, zero); 		out_5simpRSK<-test3(WRSK, zero); out_5simpRLK<-test3(WRLK, zero); out_5simpPSL<-test3(WPSL, zero); 									out_5simpPSK<-test3(WPSK, zero); out_5simpPLK<-test3(WPLK, zero); out_5simpSLK<-test3(WSLK, zero)	
  out_5simpRP<-test2(WRP); out_5simpRS<-test2(WRS); out_5simpRL<-test2(WRL); out_5simpRK<-test2(WRK); out_5simpPS<-test2(WPS);			out_5simpPL<-test2(WPL); out_5simpPK<-test2(WPK); out_5simpSL<-test2(WSL); out_5simpSK<-test2(WSK); out_5simpLK<-test2(WLK)
  if(is.na(out_5simpRP[3]) == T || out_5simpRP[3] == 0){out_5simpRP[1:2]<-c(NA, NA)}
  if(is.na(out_5simpRS[3]) == T || out_5simpRS[3] == 0){out_5simpRS[1:2]<-c(NA, NA)}
  if(is.na(out_5simpRL[3]) == T || out_5simpRL[3] == 0){out_5simpRL[1:2]<-c(NA, NA)}
  if(is.na(out_5simpRK[3]) == T || out_5simpRK[3] == 0){out_5simpRK[1:2]<-c(NA, NA)}
  if(is.na(out_5simpPS[3]) == T || out_5simpPS[3] == 0){out_5simpPS[1:2]<-c(NA, NA)}
  if(is.na(out_5simpPL[3]) == T || out_5simpPL[3] == 0){out_5simpPL[1:2]<-c(NA, NA)}
  if(is.na(out_5simpPK[3]) == T || out_5simpPK[3] == 0){out_5simpPK[1:2]<-c(NA, NA)}
  if(is.na(out_5simpSL[3]) == T || out_5simpSL[3] == 0){out_5simpSL[1:2]<-c(NA, NA)}
  if(is.na(out_5simpSK[3]) == T || out_5simpSK[3] == 0){out_5simpSK[1:2]<-c(NA, NA)}
  if(is.na(out_5simpLK[3]) == T || out_5simpLK[3] == 0){out_5simpLK[1:2]<-c(NA, NA)}
  eq_5simp<-matrix(c(out_5simp, out_5simpRPSL[1:4],0,out_5simpRPSL[5], out_5simpRPSK[1:3],0,out_5simpRPSK[4:5],							out_5simpRPLK[1:2],0,out_5simpRPLK[3:5], out_5simpRSLK[1],0,out_5simpRSLK[2:5], 0,out_5simpPSLK[1:5], 								out_5simpRPS[1:3],0,0,out_5simpRPS[4], out_5simpRPL[1:2],0,out_5simpRPL[3],0,out_5simpRPL[4], 										out_5simpRPK[1:2],0,0,out_5simpRPK[3:4], out_5simpRSL[1],0,out_5simpRSL[2:3],0,out_5simpRSL[4],										out_5simpRSK[1],0,out_5simpRSK[2],0,out_5simpRSK[3:4], out_5simpRLK[1],0,0,out_5simpRLK[2:4], 										0,out_5simpPSL[1:3],0,out_5simpPSL[4], 0,out_5simpPSK[1:2],0,out_5simpPSK[3:4], 0,out_5simpPLK[1],0,out_5simpPLK[2:4], 				0,0,out_5simpSLK, out_5simpRP[1:2],0,0,0,out_5simpRP[3], out_5simpRS[1],0,out_5simpRS[2],0,0,out_5simpRS[3],						out_5simpRL[1],0,0,out_5simpRL[2],0,out_5simpRL[3], out_5simpRK[1],0,0,0,out_5simpRK[2:3], 											0,out_5simpPS[1:2],0,0,out_5simpPS[3], 0,out_5simpPL[1],0,out_5simpPL[2],0,out_5simpPL[3], 											0,out_5simpPK[1],0,0,out_5simpPK[2:3], 0,0,out_5simpSL[1:2],0,out_5simpSL[3], 0,0,out_5simpSK[1],0,out_5simpSK[2:3], 				0,0,0,out_5simpLK[1:3], 1,0,0,0,0,1, 0,1,0,0,0,1, 0,0,1,0,0,1, 0,0,0,1,0,1, 0,0,0,0,1,1), nrow=31, ncol=6, byrow=T)
  rownames(eq_5simp)<-c("RPSLK","RPSL","RPSK","RPLK","RSLK","PSLK","RPS","RPL","RPK","RSL","RSK","RLK","PSL","PSK","PLK","SLK", 	"RP","RS","RL","RK","PS","PL","PK","SL","SK","LK","R","P","S","L","K")
  colnames(eq_5simp)<-c("R","P","S","L","K","Stability")
  if(gen_time == "continuous"){Equilibrium<-apply(eq_5simp, 1, cont5Classify, W, zero)}
  else if(gen_time == "discrete"){Equilibrium<-apply(eq_5simp, 1, disc5Classify, W, zero)}
  else{print("Invalid generation time")}
  Neg_frequency<-c(testApostatic(WRPSLK, c("R","P","S","L","K")), testApostatic(WRPSL, c("R","P","S","L")), 								testApostatic(WRPSK, c("R","P","S","K")), testApostatic(WRPLK, c("R","P","L","K")), testApostatic(WRSLK, c("R","S","L","K")), 		testApostatic(WPSLK, c("P","S","L","K")), testApostatic(WRPS, c("R","P","S")), testApostatic(WRPL, c("R","P","L")), 				testApostatic(WRPK, c("R","P","K")), testApostatic(WRSL, c("R","S","L")), testApostatic(WRSK, c("R","S","K")), 						testApostatic(WRLK, c("R","L","K")), testApostatic(WPSL, c("P","S","L")), testApostatic(WPSK, c("P","S","K")), 						testApostatic(WPLK, c("P","L","K")), testApostatic(WSLK, c("S","L","K")), NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  Pos_frequency<-c(testAntiApostatic(WRPSLK, c("R","P","S","L","K")), testAntiApostatic(WRPSL, c("R","P","S","L")), 						testAntiApostatic(WRPSK, c("R","P","S","K")), testAntiApostatic(WRPLK, c("R","P","L","K")), 										testAntiApostatic(WRSLK, c("R","S","L","K")), testAntiApostatic(WPSLK, c("P","S","L","K")), 										testAntiApostatic(WRPS, c("R","P","S")), testAntiApostatic(WRPL, c("R","P","L")), 													testAntiApostatic(WRPK, c("R","P","K")), testAntiApostatic(WRSL, c("R","S","L")), 													testAntiApostatic(WRSK, c("R","S","K")), testAntiApostatic(WRLK, c("R","L","K")), 													testAntiApostatic(WPSL, c("P","S","L")), testAntiApostatic(WPSK, c("P","S","K")), 													testAntiApostatic(WPLK, c("P","L","K")), testAntiApostatic(WSLK, c("S","L","K")), NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  Intransitivity<-c(testIntransitive(WRPSLK), testIntransitive(WRPSL), testIntransitive(WRPSK), 											testIntransitive(WRPLK), testIntransitive(WRSLK), testIntransitive(WPSLK), testIntransitive(WRPS), 									testIntransitive(WRPL), testIntransitive(WRPK), testIntransitive(WRSL), testIntransitive(WRSK), testIntransitive(WRLK), 			testIntransitive(WPSL), testIntransitive(WPSK), 	testIntransitive(WPLK), testIntransitive(WSLK), 								NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
  table_out_5<-data.frame(format(eq_5simp[,c(1:5)], digits=sig_dig), Equilibrium, Neg_frequency, Pos_frequency, Intransitivity)
}