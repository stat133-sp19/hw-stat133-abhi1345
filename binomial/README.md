# Overview
"binomial" is a minimal R package that provides functions to simulate a binomial probability distribution.

binom_var
bin_choose computes the number of successful combinations given n and k.
bin_probability computes the probability of k successes in n trials.
bin_distribution generates a binomial distribution from n and p.
bin_cumulative generates a bin_distribution with cumulative probabilities.
bin_variable creates a variable of class "binvar" with trials and prob information.
There are various binomial functions for measures of summary, such as bin_mean, bin_variance, bin_mode, bin_skewness, and bin_skewness.

# Motivation
This package has been developed for Stat 133 to demonstrate proficiency in R.

# Installation
Install the development version from GitHub via the package "devtools":

# development version from GitHub:
#install.packages("devtools")

# install "binomial" (without vignettes)
```r
devtools::install_github("gastonstat/cointoss")
```
# install "binomial" (with vignettes)
```r
devtools::install_github("gastonstat/cointoss", build_vignettes = TRUE)
Usage
library(binomial)
```

# default binom var
```r
bin1 <- bin_variable(5, .5)
bin1
[1] "Binomial Variable"
[1]
[1] Parameters
[1] - number of trials: 5
[1] - prob of success: 0.5
```

# Measures of center
```r
bin_mean(5, .5)
[1] 2.5
bin_variance(5, .5)
[1] 1.25
bin_skewness(5, .5)
[1] 0
bin_mode(5, .5)
[1] 3 2
bin_kurtosis(5, .5)
[1] -0.4
```
