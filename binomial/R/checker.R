#Private Checker Functions

#Probability Checker Function
#Checks that input, prob, is in [0, 1]
check_prob <- function(prob) {
  if (class(prob) == "numeric" & prob <= 1 & prob >= 0){
    return(TRUE)
  }
  stop("Invalid Prob Value")
}

#Check Trials Function
#Checks that input is nonnegative integer
check_trials <- function(trials) {
  if (as.integer(trials) == trials & trials >= 0){
    return(TRUE)
  }
  stop("Invalid Trials Value")
}

#Success Checker Function
#Ensures that each success is between 0 and trials, inclusive
check_success <- function(successes, trials) {
  if (any(successes > trials)) {
    stop("Success can't be greater than trials.")
  }
  if (any(successes < 0)) {
    stop("Invalid Success value.")
  }
  return(TRUE)
}
