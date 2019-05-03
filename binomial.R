library(ggplot2)

# 1.1) Private Checker Functions

# Function check_prob()
#This function tests whether an input prob is a valid probability value
check_prob <- function(prob) {
  if (prob > 1 | prob < 0) {
    stop('p has to be a number betwen 0 and 1')
  }
  else {
    return(TRUE)
  }
}

# Function check_trials()
# This function tests whether an input trials is a valid value for a number of trials (i.e. n is a non-negative integer)
check_trials <- function(trials){
  if (trials <0) {
    stop('invalid trials value')
  } else {
    return(TRUE)
  }
}

# Function check_success()
# This function tests the success input is a valid value for number of successes
check_success <- function(success, trials){
  if(is.vector(success) == TRUE & sum(success <= trials) == length(success))
    return(TRUE)
  else
    stop('invalid success value')
}


# 1.2) Private Auxiliary Functions
#This function calculates the  expected value or mean of a binomial distribution
aux_mean <- function(trials,prob) {
  multiply_trials_prob <- trials*prob
  return(multiply_trials_prob)
}

#This function calculates the variance, or measure of distribution of the binomial distribution
aux_variance <- function(trials,prob) {
  variance_trials_prob <- trials*prob*(1-prob)
  return(variance_trials_prob)
}


# This function calculates m, the most likely number of success in n independent trials with probability p of success
aux_mode <- function(trials,prob) {
  mode_trials_prob <- floor(trials*prob + prob)
  return(mode_trials_prob)
}

# This function calculates the skewness, or a measure of the asymmetry of the probability distribution of a random variable about its mean
aux_skewness <- function(trials,prob) {
  skewness_trials_prob <- (1-(2*prob))/(sqrt(trials*prob*(1-prob)))
  return(skewness_trials_prob)
}

#This function calculates the measure of the "tailedness" of the probability distribution
aux_kurtosis <- function(trials,prob) {
  kurtosis_trials_prob <- ((1-6*prob)*(1-prob))/((trials*prob))*(1-prob)
  return(kurtosis_trials_prob)
}



# 1.3) Function bin_choose()

#' @title bin choose function
#' @description Calculates the number of combinations in which k successes can occur in n trials
#' @param n number of trials
#' @param k number of successes
#' @return vector of tosses
#' @export
#' @examples bin_choose(n = 5, k = 2)
bin_choose <- function(n, k){
  if (k > n)
    stop('k cannot be greater than n')
  else
    return(factorial(n)/(factorial(k)*factorial(n-k)))
}


# 1.4) Function bin_probability()
#' @title Coin toss function
#' @description Simulates tossing a coin a given number of times
#' @param success number of successes
#' @param k number of successes
#' @param prob probability
#' @return vector of tosses
#' @export
#' @examples bin_probability(success = 2, trials = 5, prob = 0.5)
bin_probability <- function(success, trials, prob) {
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  return(bin_choose(trials, success)*(prob^(success))*((1-prob)^(trials-(success))))
}



# 1.5) Function bin_distribution()

#' @title bin distribution function
#' @description provides a data frame with the probability distribution
#' @param trials number of trials
#' @param prob probability
#' @return a data frame with the probability distribution: sucesses in the first column, probability in the second column
#' @export
#' @examples bin_distribution(trials = 5, prob = 0.5)
bin_distribution <- function(trials, prob) {
  data1 <- data.frame(success = 0:trials, probability = bin_probability(0:trials, trials, prob))
  class(data1) = c('bindis', 'data.frame')
  return(data1)
}


# Function plot.bindis()
#' @export
plot.bindis <- function(yes){
  barplot(yes$probability, names.arg = yes$success, xlab = "successes", ylab = "probability")
}

dis1 <- bin_distribution(trials = 5, prob = 0.5)
plot(dis1)


# 1.6) Function bin_cumulative()
#' @title bin cumulative function
#' @description returns data frame with e probability distribution and the cumulative probabilities
#' @param trials number of trials
#' @param prob probability
#' @return data frame with sucesses in the first column, probability in the second column, and cumulative in the third column
#' @export
#' @examples bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials, prob){
  success = 0:trials
  data2 <- data.frame(success,
                      probability = bin_probability(success, trials, prob),
                      cumulative = cumsum(bin_probability(success, trials, prob)))
  class(data2) = c('bincum', 'data.frame')
  return(data2)
}


#' @export
plot.bincum <- function(cum1){
  plot(cum1$success, cum1$cumulative, type = "o")
  lines(cum1$success, cum1$cumulative)
}

dis2 <- bin_cumulative(trials = 5, prob = 0.5)
plot(dis2)



# 1.7) Function bin_variable()

#' @title bin variable function
#' @description returns variables of the binomial distribution
#' @param trials number of trials
#' @param prob probability
#' @return return an object of class "binvar"
#' @export
#' @examples bin_variable(3, .5)
bin_variable <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  result <- list("trials" = trials, "prob" = prob)
  class(result) = c('binvar')
  return(result)
}



#' @export
print.binvar <- function(x){
  cat('"Binomial Variable"\n\n')
  cat("Parameters\n", append = TRUE)
  cat("-number of trials:", x$trials, append = TRUE)
  cat("\n-probability:", x$prob, append = TRUE)
}
bin1 <- bin_variable(trials = 10, p = 0.3)
bin1



#' @export
summary.binvar <- function(x,...){
  object1 <- list(trials = x$trials, prob = x$prob, mean = aux_mean(x$trials, x$prob), variance = aux_variance(x$trials, x$prob), mode = aux_mode(x$trials, x$prob), skewness = aux_skewness(x$trials, x$prob), kurtosis = aux_kurtosis(x$trials, x$prob) )
  class(object1) = "summary.binvar"

  return(object1)
}
bin_summary <- bin_variable(trials = 10, p = 0.3)
bin_summary1 <- summary(bin_summary)
bin_summary1


#' @export
print.summary.binvar <- function(x){
  cat('"Summary Binomial"\n\n')
  cat("Parameters\n", append = TRUE)
  cat("-number of trials:", x$trials, append = TRUE)
  cat("\n-prob of success:", x$prob, append = TRUE)
  cat('\n\n"Measures"')
  cat("\n-mean:", aux_mean(x$trials, x$prob), append = TRUE)
  cat("\n-variance:", aux_variance(x$trials, x$prob), append = TRUE)
  cat("\n-mode:", aux_mode(x$trials, x$prob), append = TRUE)
  cat("\n-skewness:", aux_skewness(x$trials, x$prob), append = TRUE)
  cat("\n-kurtosis:", aux_kurtosis(x$trials, x$prob), append = TRUE)
}

bin1 <- bin_variable(trials = 10, p = 0.3)
binsum1 <- summary(bin1)
binsum1

# 1.8) Functions of measures

bin_mean <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}

bin_mean(10, 0.3)

bin_variance <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}

bin_mode <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}

bin_skewness <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))
}

bin_kurtosis <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}

