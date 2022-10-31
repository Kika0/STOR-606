## Code to be used for STOR 606

## Time to Failure Example: Lab 1

set.seed(12345)

Failure <- function(){
  # Failure event
  # Update state and schedule future events
  S <<- S - 1
  if (S == 1){
    NextFailure <<- Clock + ceiling(6*runif(1,min = 0, max=1))
    NextRepair <<- Clock + 2.5
  }
  # Update area under S(t) curve
  Area <<- Area + Slast*(Clock - Tlast)
  Tlast <<- Clock
  Slast <<- S
}
Repair <- function(){
  # Repair event
  # Update state and schedule future events
  S <<- S + 1
  if (S == 1){
    NextFailure <<- Clock + ceiling(6*runif(1,min = 0, max=1))
    NextRepair <<- Clock + 2.5
  }
  # Update area under S(t) curve
  Area <<- Area + Slast*(Clock - Tlast)
  Tlast <<- Clock
  Slast <<- S
}

Timer <- function(){
  # Determine next event and advance time
  if (NextFailure < NextRepair){
    TimerEvent <- "Failure"
    Clock <<- NextFailure
    NextFailure <<- Inf
  }
  else{
    TimerEvent <- "Repair"
    Clock <<- NextRepair
    NextRepair <<- Inf
  }
  TimerEvent
}

TTF <- function(Starting.seed){
  # Program to generate a sample path for the TTF example
  set.seed(Starting.seed)
  .Random.seed <<- .Random.seed
  # Initialize the state and statistical variables
  S <<- 2
  Slast <<- 2
  Clock <<- 0
  Tlast <<- 0
  Area <<- 0
  # Schedule the initial failure event
  NextFailure <<- ceiling(6*runif(1,min = 0, max=1))
  NextRepair <<- Inf
  # Advance time and execute events until the system fails
  while (S != 0){
    NextEvent <<- Timer()
    switch(NextEvent,
           Failure = Failure(),
           Repair = Repair())
  }
  list(FailureTime = Clock, AveFunctional = Area/Clock)
}

  #-------------------------------------------------------  
  
  ## Nonstationary Poisson Process: Lab 4
  
  #read.csv("ArrivalTimes.csv", header=TRUE)
  
  NSPP <- function(Times, Tend, k){
    # Function to generate arrival times from a NSPP
    # using the interpolated integrated rate function
    # of Leemis (1991) and inversion.
    # Returns both interarrival times (A) and arrival times (S)
    # T = vector of arrival times across k sample paths
    #   on [0, Tend] maybe not sorted
    C <- length(Times)
    Times <- sort(c(0, Times, Tend))
    n <- 1
    A <- NULL
    S <- NULL
    Stilde <- rexp(1, rate = 1)
    while (Stilde <= C/k){
      m <- floor((C+1)/C*k*Stilde)
      S <- c(S, 
             Times[m+1] + (Times[m+2] - Times[m+1])*((C+1)/C*k*Stilde - m))
      if (n == 1){
        A <- S[1]
      }
      else{
        A <- c(A, S[n] - S[n-1])
      }
      n <- n + 1
      Atilde <- rexp(1, rate = 1)
      Stilde <- Stilde + Atilde
    }
    list(Arrivals = S, Interarrivals = A)
  }
  
  #------------------------------------------------------- 
  
  ## SAN and IU: Lab 6
  
  SAN <- function(Means){
    # Simulation of stochastic activity network
    # with given Means for the exponential activity times
    X <- rexp(5, rate=1/Means)
    Y <- max(X[1]+X[4], X[1]+X[3]+X[5], X[2]+X[5])
    Y
  }

#  SANData <- read.csv("SANData.csv", header=TRUE)

set.seed(12345)
MySANData <- cbind(rexp(1000, rate=1/10),rexp(1000, rate=1/15),rexp(1000, rate=1/10),
                   rexp(1000, rate=1/10),rexp(1000, rate=1/15))


  
  IU <- function(SANData, n, b){
    # Solution to SAN input uncertainty exercise
    # first fit to data then run the nominal simulation
    Means <- apply(SANData, 2, mean)
    Y <- NULL
    for (j in 1:n){
      Y <- rbind(Y, SAN(Means))
    }
    Ybar <- mean(Y)
    VYbar <- var(Y)/n
    # next estimate the effect of input uncertainty
    Yboot <- NULL
    for (i in 1:b){
      Means <- NULL
      for (k in 1:5){
        Means <- c(Means, mean(sample(SANData[,k], length(SANData[,k]), replace=TRUE)))
      }
      Y <- NULL
      for (j in 1:n){
        Y <- rbind(Y, SAN(Means))
      }
      Yboot <- cbind(Yboot, Y)
    }
    S2T <- n*var(apply(Yboot, 2, mean))
    S2S <- sum(apply(Yboot, 2, var))/b
    S2I = (S2T - S2S)/n
    list(Ybar = Ybar, VYbar = VYbar, S2I = S2I, S2T = S2T, S2S = S2S)
  }
  
  
  #------------------------------------------------------- 
  
  
  ## AR(1) Steady State Example and MSER: Lab 7
  
  AR1 <- function(mu, phi, sigma, Y0, m){
    # Simulation of the AR(1) surrogate process
    # Generates m+1 observations from Y0 to Ym
    Y <- Y0
    X <- rnorm(m+1, mean = 0, sd = sigma)
    for (i in 2:(m+1)){
      Y <- c(Y, mu + phi*(Y[i-1] - mu) + X[i])  
    }
    Y  
  }
  
  AR1MSE <- function(mu, phi, sigma, Y0, n, m){
    # plots the asymptotic MSE for the AR(1) example
    # as a function of deletion d
    MSE <- NULL
    d2 <- floor(m/2)
    for (d in 0:d2){
      MSE <- c(MSE, 
               ((Y0 - mu)^2) * (phi^(2*d + 2))/( ((m-d)^2) * ((1-phi)^2))
               + sigma^2/(n*(m-d)*(1-phi)^2))
    }
    plot(0:d2, MSE, type="l", xlab="d", ylab="MSE")
    dstar = which.min(MSE) - 1
    points(dstar, MSE[dstar+1], type = "p", pch=19)
    list(MSE = MSE, d = dstar)
  }
  
  MSER <- function(Y){
    # Function to compute the MSER deletion point
    m <- length(Y)
    s <- 0
    q <- 0
    MSER <- NULL
    for (d in (m-1):0){
      s <- s + Y[d+1]
      q <- q + Y[d+1]^2
      MSER <- c( (q-s^2/(m-d))/(m-d)^2, MSER)    
    }
    m2 <- ceiling(m/2) + 1
    d <- which.min(MSER[1:m2]) - 1
    list(d = d, MSER = MSER)
  }  
  
  #------------------------------------------------------- 
  
  ## SAN, Subset Selection and KN: Lab 8
  
  SAN <- function(Means){
    # Simulation of stochastic activity network
    # with given Means for the exponential activity times
    X <- rexp(5, rate=1/Means)
    Y <- max(X[1]+X[4], X[1]+X[3]+X[5], X[2]+X[5])
    Y
  }
  
  X <- matrix(c(0.5,1,1,1,1, 1,0.5,1,1,1, 1,1,0.5,1,1, 0.3,1,1,0.4,1,
                1,1,1,1,0.5),ncol=5, byrow=F)
  
  
  SANSubset <- function(X, n, alpha){
    # function to do subset selection for SAN exercise
    # first do the simulations
    k <- ncol(X)
    Yall <- NULL
    for (i in 1:k){
      Means <- X[,i]
      Y <- NULL
      for (j in 1:n){
        Y <- rbind(Y, SAN(Means))
      }
      Yall <- cbind(Yall, Y)
    }
    # next do subset selection
    Ybar <- apply(Yall, 2, mean)
    S2 <- apply(Yall, 2, var)/n
    tval <- qt((1-alpha)^(1/(k-1)), df = n-1)
    Subset <- 1:k
    for (i in 1:k){
      for (j in 1:k){
        if (Ybar[i] > (Ybar[j]+tval*sqrt(S2[i] + S2[j]))){
          Subset[i] <- 0
        }
      }
    }
    list(Subset = Subset, Ybar = Ybar, S2 = S2)
  }
  

  KN <- function(n0, alpha, delta){
    # function do to KN without CRN on SAN problem
    # k = number of systems = 5
    # n0 = first-stage sample size
    # 1-alpha = desired PCS
    # delta = indifference-zone parameter
    # built for max so maximize -SAN
    X <- matrix(c(0.5,1,1,1,1, 1,0.5,1,1,1, 1,1,0.5,1,1, 0.3,1,1,0.4,1,
                  1,1,1,1,0.5),ncol=5, byrow=F)
    k <- 5
    II <- 1:k
    Active <- rep(TRUE, k)
    Elim <- rep(0, k)
    # get n0 from each system and compute variances
    Yn0 <- matrix(0, nrow=k, ncol=n0)
    for (i in 1:k){
      for (j in 1:n0){
        Yn0[i,j] <- SAN(X[,i])
      }
    }
    S2 <- var(t(Yn0))
    
    # start sequential
    eta <- 0.5*( (2*alpha/(k-1))^(-2/(n0-1)) -1)
    h2 <- 2*eta*(n0-1)
    Ysum <- apply(Yn0, 1, sum)
    r <- n0
    # main elimination loop
    while(sum(Active)> 1){
      r <- r + 1
      ATemp <- Active
      for(i in II[Active]){
        Ysum[i] <- Ysum[i] + SAN(X[,i])
      }
      for(i in II[Active]){
        for(l in II[Active]){
          S2diff <- S2[i,i] + S2[l,l] - 2*S2[i,l]
          W <- max(0, (delta/2)*(h2*S2diff/delta^2 - r))
          if(Ysum[i] > Ysum[l] + W){
            ATemp[i] <- FALSE
            Elim[i] <- r
            break
          }
        }
      }
      Active <- ATemp
    }
    list(Best = II[Active], n = r, Elim=Elim)
  }

  #-------------------------------------------------------   
  
  
  ## Stochastic Approximation Example: Lab 9
  
  SA <- function(x0, cc, tau, ll, b, a, n, steps){
    # Implements stochastic approximation (SA) using LR derivatives 
    # for the SAN problem of Nelson (2013).
    # Plots one sample path of optimization progress
    # input
    #   x0 = starting value of x
    #   cc = cost coefficients
    #   tau = nominal mean activity times
    #   ll = lower bounds on mean activity times
    #   b = budget
    #   a = gain coefficient for SA
    #   n = number of reps at each SA iteration
    #   steps = number of optimization iterations
    # output
    #   xpath = vector of x values across iteration
    #   thetapath = vector of objective function values across iterations 
    
    x <- x0
    xpath <- NULL
    thetapath <- NULL
    
    for (i in 1:steps){
      # estimate gradient wrt x
      Out <- SANGrad(n, x)
      xnew <- x - (a/i)*Out$G
      cost <- sum(cc*(tau - xnew))
      # if infeasible, project to feasible
      if (cost > b){
        tt <- (cost - b)/sum(cc^2)
        x <- xnew + tt*cc
      }
      else{
        x <- xnew
      }
      # enforce lower bounds
      for (j in 1:5){
        x[j] <- max(x[j], ll[j])
      }
      xpath <- rbind(xpath, t(x))
      thetapath <- rbind(thetapath, mean(Out$Y))
    }
    par(mfrow=c(2,1))
    plot(1:steps, thetapath, xlab = "iteration", ylab="thetahat(x)", type="l")
    matplot(1:steps, xpath, type="l", xlab = "iteration", ylab="x")
    par(mfrow=c(1,1))
    list(xpath = xpath, thetapath = thetapath)
  }
  
  
  SANGrad <- function(n, x){
    # generates reps of the SAN with exponential activity times
    # returning LR gradient estimates
    # input
    #   n = number of replications
    #   x = vector of exponential MEANS for activities 1,2,...,5
    # output
    #   Y = vector of times to complete the project
    #   A = n x 5 matrix of generated activity times
    #   G = 5 x 1 vector of gradient estimates at theta
    
    A <- cbind(rexp(n, rate=1/x[1]), rexp(n, rate=1/x[2]), rexp(n, rate=1/x[3]),
               rexp(n, rate=1/x[4]), rexp(n, rate=1/x[5]))
    AA <- cbind(A[,1]+A[,4], A[,1]+A[,3]+A[,5], A[,2]+A[,5])
    Y <- as.matrix(apply(AA, 1, max))
    
    # LR gradient calculations
    TT <- matrix(1/x, ncol=5, nrow=n, byrow=T)
    G <- (t((A*TT^2 - TT)) %*% Y)/n
    list(Y = Y, A = A, G = G)
  }