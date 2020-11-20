
AsianGeometricPricingCF<-function(S0,K,r,T,sigma,delta,N,AveragePrice,call){
  
  ##FOR AVERAGE OPTION PRICE
  
  if (AveragePrice==1){
    deltastar<-0.5*(r*(N-1)/N+(delta+sigma^2/2*(N+1)/N-sigma^2*(N+1)*(2*N+1)/(N^2*6)))
    sigmastar<-sigma/N*sqrt((N+1)*(2*N+1)/6)
    d1<-(log(S0/K)+(r-deltastar+0.5*sigmastar^2)*T)/(sigmastar*sqrt(T))
    d2<-d1-(sigmastar*sqrt(T))
    
 ##FOR CALL OPTION
    
    if (call==1){
      pricecall<-S0*exp(-deltastar*T)*pnorm(d1)-K*exp(-r*T)*pnorm(d2)
      return(pricecall)
    }
##FOR PUT OPTION
    
    if (call==0) {
      priceput<-K*exp(-r*T)*pnorm(-d2)-S0*exp(-deltastar*T)*pnorm(-d1)
      return(priceput)
    }
  }
  
  ##FOR AVERAGE STRIKE PRICE 
  
   if (AveragePrice==0){
      rstar<-0.5*(r*(N-1)/N+(delta+sigma^2/2*(N+1)/N-sigma^2*(N+1)*(2*N+1)/(N^2*6)))
      rho<-0.5*sqrt(6*(N+1)/(2*N+1))
      sigmahat<-sigma*sqrt(1+(N+1)*(2*N+1)/(6*N^2)-2*rho*sqrt((N+1)*(2*N+1)/(6*N^2)))
      d1<-(log(S0/K)+(rstar-delta+0.5*sigmahat^2)*T)/(sigmahat*sqrt(T))
      d2<-d1-(sigmahat*sqrt(T))
      
  ##FOR CALL OPTION
      
      if (call==1){
        strikecall<-S0*exp(-delta*T)*pnorm(d1)-K*exp(-rstar*T)*pnorm(d2)
        return(strikecall)
      }
  ##FOR PUT OPTION
      
      if (call==0) {
        strikeput<-K*exp(-rstar*T)*pnorm(-d2)-S0*exp(-delta*T)*pnorm(-d1)
        return(strikeput)
      }
    }
  }
