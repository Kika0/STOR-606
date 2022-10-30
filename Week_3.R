

MySim <- function(x, n=1, RandomSeed=-1){
  # simulates the (s,S) inventory example of Koenig & Law
  # x in {1,2,...,1600} is the system index
  # n = number of replications
  # RandomSeed sets the initial seed
  # output is average cost for 30 periods
  
  littleS <- ceiling(x/40)
  bigS <- littleS + x - (littleS - 1)*40
  
  if (RandomSeed > 0){set.seed(RandomSeed)}
  
  Y <- rep(0, n)
  for (j in 1:n){
    InvtPos <- bigS
    Cost <- 0
    for (period in 1:30){
      Demand = rpois(1,25)
      if (InvtPos < littleS){
        INext <- bigS
        Cost <- Cost + 32 +3*(bigS - InvtPos)
      }
      else{
        INext <- InvtPos
      }
      if (INext - Demand >= 0){
        Cost <- Cost + INext - Demand
      }
      else{
        Cost <- Cost + 5*(Demand - INext) 
      }
      InvtPos <- INext - Demand
    }
    Y[j] <- Cost/30
  }
  -Y
}
