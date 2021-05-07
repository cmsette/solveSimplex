### Functions to describe games (positive & negative frequency-dependence, intransitivity)
# Tests whether game has negative frequency dependence
# Negative: values along major axis smallest in column
testApostatic<-function(W, Wvec){
  ApoOut<-NULL
  if(nrow(W)>=3){
    if(any(which(W[,1]==min(W[,1]))==1)){ApoOut<-paste(ApoOut, Wvec[1])}
    if(any(which(W[,2]==min(W[,2]))==2)){ApoOut<-paste(ApoOut, Wvec[2])}
    if(any(which(W[,3]==min(W[,3]))==3)){ApoOut<-paste(ApoOut, Wvec[3])}
  }
  if(nrow(W)>=4){if(any(which(W[,4]==min(W[,4]))==4)){ApoOut<-paste(ApoOut, Wvec[4])}}
  if(nrow(W)==5){if(any(which(W[,5]==min(W[,5]))==5)){ApoOut<-paste(ApoOut, Wvec[5])}}
  if(is.null(ApoOut)){ApoOut<-"none"}
  return(ApoOut)
}
# Tests whether game has positive frequency dependence
# Positive: values along major axis largest in column
testAntiApostatic<-function(W, Wvec){
  AntiApoOut<-NULL
  if(nrow(W)>=3){
    if(any(which(W[,1]==max(W[,1]))==1)){AntiApoOut<-paste(AntiApoOut, Wvec[1])}
    if(any(which(W[,2]==max(W[,2]))==2)){AntiApoOut<-paste(AntiApoOut, Wvec[2])}
    if(any(which(W[,3]==max(W[,3]))==3)){AntiApoOut<-paste(AntiApoOut, Wvec[3])}
  }
  if(nrow(W)>=4){if(any(which(W[,4]==max(W[,4]))==4)){AntiApoOut<-paste(AntiApoOut, Wvec[4])}}
  if(nrow(W)==5){if(any(which(W[,5]==max(W[,5]))==5)){AntiApoOut<-paste(AntiApoOut, Wvec[5])}}
  if(is.null(AntiApoOut)){AntiApoOut <-"none"}
  return(AntiApoOut)
}
# Tests for cometitive intranstitivity
# Finds position of best response (highest payoff per row) - is intransitive if positions are all different (5-player: 2 sets)
rowMax<-function(row){which(row==max(row))}
testIntransitive<-function(W){
  bestR1<-apply(W, 1, rowMax)
  if(nrow(W)<5){if(length(unique(bestR1))==length(bestR1)){IntOut<-"Intransitive"}else{IntOut<-"none"}}else 
    if(nrow(W)==5){
      test<-try(bestR2<-c(bestR1[1,], bestR1[2,]), silent=T)
      if(class(test)=="try-error"){if(length(unique(bestR1))==length(bestR1)){IntOut<-"Intransitive"}else{IntOut<-"none"}} else 
      {bestR2<-c(bestR1[1,], bestR1[2,]); if(length(unique(bestR2))*2==length(bestR2)){IntOut<-"Intransitive"}else{IntOut<-"none"}}}
  return(IntOut)
}