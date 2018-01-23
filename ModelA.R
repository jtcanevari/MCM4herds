# First we define our one-step function.
SIR.onestep <- function (x, params) { #function to calculate one step of stochastic SIR
  t   <- x[1] #local variable for time
  SNP <- x[2] #local variable for susceptibles
  SP  <- x[3]
  INP <- x[4] #local variable for infecteds
  IP  <- x[5]
  RNP <- x[6] #local variable for recovereds
  RP  <- x[7]
  E   <- x[8] #local variable for environmental contamination
  N <- SNP+SP+INP+IP+RNP+RP #total population size (subject to demographic change)
  
  with( #use with as in deterministic model to simplify code
    as.list(params),
    {
      rates <- c(SNP*Qc.A(t),SP*Qp.A(t),(beta*SNP*E)/N,(beta*SP*E)/N, gamma*INP,INP*Qc.A(t),IP*Qp.A(t),RNP*Qc.A(t),RP*Qp.A(t))
      changes <- matrix(c(-1,1,0,0,0,0,
                          1,-1,0,0,0,0,
                          -1,0,1,0,0,0,
                          0,-1,0,1,0,0,
                          0,0,-1,0,1,0,
                          0,0,-1,1,0,0,
                          0,0,0,-1,1,0,
                          0,0,0,0,-1,1,
                          0,0,0,0,1,-1),
                        ncol=6, byrow=TRUE)
      # tau <- rexp(n=1,rate=sum(rates)) # exponential waiting time
      
      tau <- rexp(n=1,rate=sum(rates)+exp(-10)) # exponential waiting time. #adding a very small number to avoid Nas
      if (tau < 7 ) {
        U <- runif(1) #uniform random deviate #???
        m <- min(which(cumsum(rates)>=U*sum(rates)))
        # m <- sample(length(rates),1,prob=rates) #Using Hooker's method
        x <- x[2:7] + changes[m,]
        if (E>0) {E = E - E*muE*tau} #when E >0 it'll dacay at specified rate
        if (E<0) {E <- 0} #but can never be <0
        if (m == 7) {E <- E + epsilon}}
      if (tau >= 7) {
        tau = 0.2
        x <- x[2:7]
        if (E>0) {E = E - E*muE*tau} #when E >0 it'll dacay at specified rate
        if (E<0) {E <- 0} #but can never be <0
      }
      
      t <- t + tau
      # E = E - E*muE*tau # when E >0 it'll dacay at specified rate, when E = 0 => term = 0
      
      return(out <- c(t, x, E))
    }
  )
}

# Next we write our function for simulating a whole epidemic.

ModelA <- function (x, params) { #function to simulate stochastic SIR
  output <- array(dim=c(1,8)) #set up array to store results
  colnames(output) <- c("time","SNP","SP","INP","IP","RNP","RP","E") #name variables
  output[1,] <- x #first record of output is initial condition
  k=1
  stop<- params$stop
  while (as.logical((output[k,4]+output[k,5]+output[k,8] > 0) & output[k,1]<stop)){
    x <- SIR.onestep(x,params)
    output <- rbind(output,x)
    k=k+1
    cat(output[k,1], "\n", sep=""); flush.console()  # print progress of a loop
  }
  output #return output
}
