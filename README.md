# Workout 03
## Submission by: Shefali Sastry

## R Package Binomial: Overview

#### This Workout was created for the STAT 133 class. The work here is a package called `binomial` which I created for our Workout 3 assignment.

## Motivation
 
#### This Binomial package calculates the probabilities of a Binomial random variable because it allows us to find the number of successes in a fixed number of random trials performed under identical conditions (assuming a constant probability of success on each trial).

## Installation
##### Development version from GitHub:
`install.packages("devtools")` 

##### Install "binomial" (without vignettes)
`devtools::install_github("shefalisastry/workout03")`

##### Install "cointoss" (with vignettes)
`devtools::install_github("shefalisastry/workout03", build_vignettes = TRUE)`

## Usage: What this Package Contains

#### This specific package contains 15 functions that the user can use, 10 of which are main functions, and 5 of which are method functions that support these main functions due to their plotting capabilities. Here is a list of what is provided:

* Main Functions
    * `bin_choose()`
    * `bin_probability()`
    * `bin_distribution()`, Method: `plot.bindis()`
    * `bin_cumulative()`, Method: `plot.bincum()`
    * `bin_var()`, Method: `print.binvar()`
    * Summary/Auxiliary functions: `bin_mean()`, `bin_variance()`, `bin_mode()`, `bin_skewness()`, `bin_kurtosis()`
    
* Method Functions
    * `plot.bindis()`, 
    * `plot.bincum()`
    * `print.binvar()`
    * `summary.binvar()`
    * `print.summary.binvar()`


### Workout 3 Project Deliverable: [R Binomial Package](https://github.com/shefalisastry/workout03)

