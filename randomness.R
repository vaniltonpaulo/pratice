# hey this a file regarding Randomness



########Aufgabe 1

# Write a function that simulates a "broken" coin:
# With probability 0.7, the coin lands on "heads".
# With probability 0.2, the coin lands on "tails".
# With probability 0.1, the coin lands on its "edge".
#
# Input: No input.
# Returns: a `character(1)` one of "heads", "tails", or "edge", with the
#   probabilities listed above.
ex01BrokenCoin <- function() {
  # your code
  #This is not from Binder but Best choice
  coin <- c("heads", "tails", "edge")
  sample(coin, size = 1, prob = c(0.7, 0.2, 0.1))
  
  #Alternative
  x <- runif(1)
  if(x < 0.1) return("edge")
  if(x < 0.3) return("tails")
  "heads"
  
}





# "Bootstrapping" is an approach to estimating distributions of statistics
# in which random artificial data is generated, often by randomly drawing with
# replacement from observed samples, and then used to calculate the statistic.
#
# Write a function that estimates the 50% confidence interval ("CI") of the
# regression coefficients of a simple linear model.
#
# The linear model is fitted to a dataset given to the function, which resembles
# the `cars` dataset that comes bundled with R. You can use
# > model <- lm(dist ~ speed, data)
# to fit a linear model which regresses the breaking distance against speed,
# and then use
# > coefficients(model)
# to get the model's coefficients (there are 2 coefficients in this model: the
# intercept and the coefficient of the `speed` feature).
#
# To make a bootstrap estimate of the CI, you should repeatedly generate random
# new datasets and fit a new model for each dataset. Take the model coefficients
# of each of the resulting models and calculate the 25% and the 75% quantile for
# each coefficient (e.g. by using the `quantile()` function).
#
# The random datasets should be created by randomly sampling rows from the
# original data *with replacement*; the number of rows in the sampled datasets
# should be the same as the number of rows in the original dataset. The
# expression
# > sample.int(nrow(data), size = nrow(data), replace = TRUE)
# might come in handy.
#
# Input:
#  - data: a `data.frame` with numeric columns `"speed"`, `"dist"`, similar to
#   the `cars` dataset that comes bundled with R.
#  - replicates: an integer valued `numeric(1)` greater than 4, indicating the
#   number of bootstrap samples to use.
# Return value: A 2x2 `matrix`. The first row should contain the 50% CI of the
# regression intercept, the second row should contain the 50% CI of the
# coefficient of the `speed` feature. The first column should contain the
# lower end of the CIs (i.e. the 25% quantiles of sampled coefficients), the
# second row the bootstrap estimate of the 75% quantile (the second row should
# therefore contain larger values than the first).
# The `replicate()` function may be helpful in this exercise (and some of the
# following ones).
#
# Your code should *not* set a seed (and should therefore return slightly
# different probabilistic results with every invocation).
#
# An example of the expected output -- the actual output is random, of course,
# but should get values that are close to this:
# > ex02BootstrapCars(cars, 1000)
#            [,1]       [,2]
# [1,] -21.246444 -13.657819
# [2,]   3.648419   4.195206
# (This indicates a 50% CI of the intercept between -21.2 and -13.7, and a 50%
# CI of the coefficient of the `speed` feature between 3.6 and 4.2.)
ex02BootstrapCars <- function(data, replicates) {
  assertDataFrame(data, types = "numeric", ncols = 2)
  assertNames(colnames(data), permutation.of = c("speed","dist"))
  assertNumeric(data$peed, any.missing = FALSE)
  assertNumeric(data$peed, any.missing = FALSE)
  assertInt(replicates, lower = 5, tol = 0)
 
  #our functions are the exact same
}


########Aufgabe 2


# You are studying the effect that a specific food supplement has on the growth
# of mice. You do this by feeding the supplement to a small number of mice and
# measuring their weight in grams. You compare this to the weight that mice of
# the same species have after a control diet. You reject the null hypothesis of
# no effect if the p-value of the difference of average weights between the
# groups is below a threshold.
# There are not many mice in the treatment group, and there is another
# limitation: The weight of the treatment-group is measured in integer grams,
# Therefore, if a mouse actually weighs 10.33244524 grams, its reported value is
# rounded to 10.
#
# Write a function that simulates this experiment under the null-hypothesis,
# that the weight of the treatment group mice equals the weight of the control
# group mice on average, but is normally distributed around that average.
#
# Your lab has a large number of control mice, and you therefore know the
# average control group weight quite precisely; this is also the average
# treatment group weight under the null hypothesis. You can therefore simulate
# the experiment by drawing random mouse weights with a given mean and standard
# deviation. You need to incorporate the rounding in your simulations!
#
# Input values:
# - `weight.avg` : `numeric(1)` average weight of the control mice.
# - `weight.stddev`: standard deviation of mouse weights to use.
# - `treatment.size`: number of mice in the treatment group.
# Return value: Vector of length `treatment.size` of (positive, integer valued)
# simulated mouse weights.
ex01MouseWeightSim <- function(weight.avg, weight.stdev, treatment.size) {
  assertcoun
  assertNumber(weight.avg)
  assertNumber(weight.stdev)
  assertInt(treatment.size, tol = 0, lower = 1)
  #or
  #assertCount(treatment.size, positive = TRUE)
  
  #Again nothing different from my own solution
  result <- rnorm(treatment.size, mean = weight.avg, sd = weight.stdev)
  #positive, integer valued --> whole numbers
  round(result, 0)
}

# Continuation of ex01:
#
# Write a function that, given experimental data, calculates an approximate
# p-value.
#
# Wikipedia:
# > The p-value is the probability of obtaining test results at least as extreme
# > as the results actually observed, under the assumption that the null
# > hypothesis is correct.
#
# Do this by simulating experiments: Simulate experiments under the
# null-hypothesis (i.e. by calling your solution for ex01). Do this
# `simulation.rounds` times. See how often the difference between
# `control.weight.avg` and the average of mice in a simulated experiment is
# greater or equal to the actually observed difference between
# `control.weight.avg` and the average of `treatment.weights`.
#
# This is an estimate of the `p`-value, since it counts the fraction of cases
# under the null-hypothesis, in which the difference in averages between control
# and treatment group is at least as much as the observed difference.
# Input values:
# - `control.weight.avg` : `numeric(1)` average weight of the control mice. As
#   mentioned before, this value is known precisely.
# - `weight.stdev`: standard deviation of mouse weights to use.
# - `treatment.weights`: integer `numeric`: vector of weights of treatment mice.
# - `simulation.rounds`: number of experiments to simulate.
# Return return the estimated p-value as a `numeric(1)`.
ex02MouseWeightPVal <- function(control.weight.avg, weight.stdev, treatment.weights, simulation.rounds) {
  #this one was hard because of the abs and the mean
  assertNumber(control.weight.avg)
  assertNumber(weight.stdev)
  assertIntegerish(treatment.weights, tol = 0, any.missing = FALSE)
  assertInt(simulation.rounds, lower = 1, tol = 0)
  # your code
  
  #Like the previous exercise we have the same solution
  
  result <- abs(control.weight.avg - mean(treatment.weights))
  
  experiment <- replicate(simulation.rounds, abs(control.weight.avg - mean(
    ex01MouseWeightSim(control.weight.avg, weight.stdev, length(treatment.weights)))))
  sum(experiment >= result) / simulation.rounds
}





########Aufgabe 3





# You are playing an online card game that uses R to simulate shuffling of
# cards. The card game uses 52 cards, ranked 2 to 10 together with Jack (J),
# Queen (Q), King (K) and Ace (A); There are four suits named Clubs (C),
# Diamonds (D), Hearts (H), Spades (S). The cards are represented by strings
# naming the suit first, followed by the rank, e.g. "C4" (Four of Clubs) or "DA"
# (Ace of Diamonds). The unshuffled deck is thus:
cards.ordered <- paste0(
  rep(c("C", "D", "H", "S"), 13),
  rep(c(2:10, "J", "Q", "K", "A"), each = 4)
)
# To get a shuffled deck, the online game uses the following function. The seed
# is based on the time elapsed since 1970-01-01, 00:00:00 in milliseconds;
# however, because the seed is limited to numbers below about 2e9, only the last
# 9 digits of that time are used (i.e. the rest from a division by 1e9 is used).
# The time elapsed since 1970-01-01, 00:00:00 is a very typical unit for time
# measurement in computer systems: `as.numreic(Sys.time())`, for example,
# returns the number of *seconds*. Our demo code here therefore multiplies this
# with 1000.
shuffleCards <- function() {
  milliseconds <- as.numeric(Sys.time()) * 1000
  set.seed(milliseconds %% 1e9)
  cards.ordered[sample.int(52)]
}
# At the beginning of each game, the first five cards of the shuffled deck are
# revealed, e.g.
# > cards.shuffled <- shuffleCards()
# > cards.shuffled[1:5]
#
# Write a function that infers the entire shuffled deck (`cards.shuffled` in the
# example) from the revealed cards and an estimate of when the `shuffleCards()`
# call was executed on the server. You do not know the exact time of the server
# on which the cards are shuffled, and there seems to be some variation caused
# by network delays and caching. However, you are reasonably sure that you can
# estimate the server's clock by plus/minus 2 seconds. You can therefore try out
# different candidate seeds that the server could have used, set it, and do the
# same operation as `shuffleCards()`, to see if you get the same first 5 cards.
#
# Unlike in some other exercises, you explicitly need to call `set.seed()` in
# this exercise.
#
# Inputs:
#  - `cards.shuffled.head` (`character(5)`): A vector of length 5 containing a
#   subset of `cards.ordered`, indicating the first five cards revealed in the
#   game.
#  - `time.estimate` (`POSIXct`): An estimate of the time at which the
#   `shuffleCards()` function was executed. (Do not call `Sys.time()` in your
#   solution, because there could be a delay between when the cards are shuffled
#   and when the first five cards are revealed). The time used is to create
#   `cards.shuffled.head` is off by at most plus or minus 2 seconds (2000
#   milliseconds). You can use `assertPOSIXct()` to check for validity of this
#   argument.
# Return value: A `character(52)` vector containing a guess at the complete
# deck. If no possible seed in the possible time window around `time.estimate`
# could have generated `cards.shuffled.head`, then an error should be thrown.
#
# Your function can make use of the `cards.ordered` variable defined above.
#
# If your function works, then you can try it out by running the following two
# lines in quick succession:
# > cs <- shuffleCards()
# > ex01InferCards(cs[1:5], Sys.time())
# which should return the same vector as `cs`.
ex01InferCards <- function(cards.shuffled.head, time.estimate) {
  #Again same solution
  
  
  assertCharacter(cards.shuffled.head, len = 5, any.missing = FALSE)
  assertPOSIXct(time.estimate, len = 1, any.missing = FALSE)
  #your code
  time.estimate <- as.numeric(time.estimate) * 1000
  for (i in seq(time.estimate - 2000, time.estimate + 2000)) {
    set.seed(i %% 1e9)
    mycards <- cards.ordered[sample.int(52)]
    if (identical(mycards[1:5], cards.shuffled.head)) {
      return(mycards)
    }
  }
  stop("nothing found")
}
