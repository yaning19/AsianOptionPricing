
GeoMeanPayoff<-as.numeric()
Premium<-as.numeric()
asianArithmeticPricingMC<-function(S0,K,r,T,sigma,delta,N,call,AveragePrice){
  
  ##FOR AVERAGE PRICE
  
  if (AveragePrice==1){
    St<-matrix(nrow = N+1,ncol=10000)  ##THIS WILL HOLD ALL STOCK PRICE PATHS 
    dt=T/N                             ##EVERY COLUMN IS ONE PATH
                                       ##FIRST ROW IS S0, LAST ROW IS ST
    
    for (j in 1:10000){                ##10000 MC SIMULATIONS
      
      St[1,]=S0
      for (i in 1:N){
        St[i+1,j]<-St[i,j]*exp((r-delta-0.5*sigma^2)*dt+sigma*sqrt(dt)*rnorm(1))
      }
      
      if (N==1){
        Ave_St<-St[N+1,]                 ##IF N=1, THEN THE AVERAGE IS JUST ST
      }
      if (N>1){
      Ave_St<-colMeans(St[c(2:(N+1)),])  ##IF N>1, CALCULATE ARITHMETIC MEAN OF STOCK PRICE
      }
      
      ##FOR CALL OPTION
      
      if (call==1){
        Premium<-pmax(Ave_St-K,0)*exp(-r*T)
      }
      
      ##FOR PUT OPTION
      
      if(call==0){
        Premium<-pmax(K-Ave_St,0)*exp(-r*T)
      }
      j<-j+1 
    }
    Ave_Premium<-mean(Premium)
    return(Ave_Premium)
  }
  
  ##FOR AVERAGE STIKE PRICE (SAME AS PREVIOUS ONE EXCEPT FOR PRICE CALCULATION)
  
  if (AveragePrice==0){
    St<-matrix(nrow = N+1,ncol=10000)
    dt=T/N
    
    for (j in 1:10000){
      
      St[1,]=S0
      for (i in 1:N){
        St[i+1,j]<-St[i,j]*exp((r-delta-0.5*sigma^2)*dt+sigma*sqrt(dt)*rnorm(1))
      }
      
      if (N==1){
        Ave_St<-St[N+1,]                   ##IF N=1, THEN THE AVERAGE IS JUST ST
      }
      if (N>1){
        Ave_St<-colMeans(St[c(2:(N+1)),])  ##IF N>1, CALCULATE ARITHMETIC OF STOCK PRICE
      }
    
      ##FOR CALL OPTION
      
      if (call==1){
        Premium<-pmax(St[N+1,]-Ave_St,0)*exp(-r*T)
      }
      
      ##FOR PUT OPTION
      
      if(call==0){
        Premium<-pmax(Ave_St-St[N+1,],0)*exp(-r*T)
      }
      j<-j+1 
    }
    Ave_Premium<-mean(Premium)
    return(Ave_Premium)
  } 
}

