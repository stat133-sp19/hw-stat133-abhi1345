#Private Aux Functions

#Auxiliary Mean Function
#Calculates mean from n and p
#Formula: np
aux_mean <- function(trials, prob){
  return(trials*prob)
}

#Auxiliary Variance Function
#Calculates variance from n and p
#Formula: sqrt(np(1-np))
aux_variance <- function(trials, prob){
  return(trials*prob*(1-prob))
}

#Auxiliary Mode Function
#Calculates mode from n and p
#Formula: int(p+np)
aux_mode <- function(trials, prob){
  calc <- trials*prob + prob
  if (as.integer(calc) == calc) {
    return(c(calc, calc-1))
  }
  return(as.integer(calc))
}

#Auxiliary Skewness Function
#Calculates skewness from n and p
#Formula: (1-2p) / sqrt(np(1-p))
aux_skewness <- function(trials, prob){
  top <- 1 - 2*prob
  bottom <- sqrt(trials*prob*(1-prob))
  return(top / bottom)
}

#Auxiliary Kurtosis Function
#Calculates kurtosis from n and p
#Formula: (1 - 6p(1-p)) / (np(1-p))
aux_kurtosis <- function(trials, prob){
  top <- 1 - 6*prob*(1-prob)
  bottom <- trials*prob*(1-prob)
  return(top / bottom)
}
