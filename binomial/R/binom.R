library(testthat)
#Main Functions

#' @title Binomial Choose Function
#' @description Computes number of successful combinations from given trials and successes.
#'
#' @param n number of trials
#' @param k number of successes
#'
#' @return Number of combinations
#' @export
#' @examples
#' bin_choose(n = 5, k = 2)
#' bin_choose(5, 0)
#' bin_choose(5, 1:3)
bin_choose <- function(n, k) {
  if (any(k > n)) {
    stop("K must be less than or equal to n.")
  }
  return(factorial(n) / (factorial(k)*factorial(n-k)))
}

#' @title Binomial Probability Function
#' @description Calculates probability of given successes in given trials, using given binomial probability.
#' @param success number of successes
#' @param trials number of trials
#' @param prob success probability
#' @return Probability of successes in trials with given prob.
#' @export
#' @examples
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#' bin_probability(success = 55, trials = 100, prob = 0.45)
bin_probability <- function(success, trials, prob) {
  check_success(success, trials)
  check_prob(prob)
  check_trials(trials)

  choose <- bin_choose(trials, success)
  return(choose*prob^success*(1-prob)^(trials-success))
}


#' @title Binomial Distribution Function
#' @description Creates binomial distribution, using given binomial probability and trials.
#' @param trials number of trials
#' @param prob success probability
#' @return Dataframe with binomial distribution
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
#' bin_distribution(trials = 10, prob = 0.3)
bin_distribution <- function(trials, prob) {
  check_prob(prob)
  check_trials(trials)

  success <- 0:trials
  probability <- bin_probability(success, trials, prob)
  answer <- data.frame(list(success = success, probability = probability))
  class(answer) <- c("bindis", "data.frame")
  return(answer)
}

#' @export
plot.bindis <- function(bindis) {
  l <- nrow(bindis)
  return(barplot(bindis$probability, rep(2, l), names.arg = 1:l, xlab = "probability", ylab = "successes"))
}

#' @title Binomial Cumulative Distribution Function
#' @description Creates binomial distribution with cumulative probabilities, using given binomial probability and trials.
#' @param trials number of trials
#' @param prob success probability
#' @return Dataframe with binomial cumulative distribution
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
#' bin_cumulative(trials = 10, prob = 0.3)
bin_cumulative <- function(trials, prob) {
  check_prob(prob)
  check_trials(trials)
  table <- bin_distribution(trials, prob)
  l = nrow(table)
  table$cumulative <- rep(0, l)
  table$cumulative[1] = table$probability[1]
  for (i in 2:l) {
    table$cumulative[i] = table$cumulative[i-1] + table$probability[i]
  }
  class(table) <- c("bincum", "data.frame")
  return(table)
}

#' @export
plot.bincum <- function(bincum) {
  return(plot(bincum$success, bincum$cumulative, type = "b", xlab = "successes", ylab = "probability"))
}

bin_variable <-function(trials, prob) {
  check_trials(trials)
  check_prob(prob)

  n <- c("trials", "prob")
  x <- c(trials, prob)
  names(x) <- n
  class(x) <- "binvar"
  return(x)
}

#' @export
print.binvar <- function(binvar) {
  print("Binomial Variable")
  print("", quote = FALSE)
  print("Parameters", quote = FALSE)
  print(paste("- number of trials:", binvar['trials']), quote = FALSE)
  print(paste("- prob of success:", binvar['prob']), quote = FALSE)

}

#' @export
summary <- function(binvar) {
  answer <- c(
    binvar['trials'],
    binvar['prob'],
    aux_mean(binvar['trials'], binvar['prob']),
    aux_variance(binvar['trials'], binvar['prob']),
    aux_mode(binvar['trials'], binvar['prob']),
    aux_skewness(binvar['trials'], binvar['prob']),
    aux_kurtosis(binvar['trials'], binvar['prob'])
  )
  class(answer) <- "binvar"
  return(answer)
}

#' @export
print.summary.binvar <- function(binvar) {
  print("Summary Binomial")
  print("", quote = FALSE)
  print("Parameters", quote = FALSE)
  print(paste("- number of trials: "), quote = FALSE)
  print(paste("- probability of success: "), quote = FALSE)
  print("", quote = FALSE)
  print("Measures")
}


#' @title Binomial Mean Function
#' @description Computes binomial mean by calling auxiliary mean function.
#' @param trials number of trials
#' @param prob success probability
#' @return mean, as a numeric value.
#' @export
#' @examples
#' bin_mean(10, 0.3)
bin_mean <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}

#' @title Binomial Variance Function
#' @description Computes variance for binomial variable with given trials and prob. Uses auxiliary function.
#' @param trials number of trials
#' @param prob success probability
#' @return variance, as a numeric value.
#' @export
#' @examples
#' bin_variance(10, 0.3)
bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}

#' @title Binomial Mode Function
#' @description Computes mode for binomial variable with given trials and prob. Uses auxiliary function.
#' @param trials number of trials
#' @param prob success probability
#' @return mode, as a numeric value. 2 modes if value is an exact integer.
#' @export
#' @examples
#' bin_mode(10, 0.3)
bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}

#' @title Binomial Skewness Function
#' @description Computes skewness for binomial variable with given trials and prob. Uses auxiliary function.
#' @param trials number of trials
#' @param prob success probability
#' @return skewness, as a numeric value.
#' @export
#' @examples
#' bin_skewness(10, 0.3)
bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))
}

#' @title Binomial Kurtosis Function
#' @description Computes kurtosis for binomial variable with given trials and prob. Uses auxiliary function.
#' @param trials number of trials
#' @param prob success probability
#' @return kurtosis, as a numeric value. 2 modes if value is an exact integer.
#' @export
#' @examples
#' bin_kurtosis(10, 0.3)
bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}


