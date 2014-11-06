
# Coda Discrete Utils

Utility to plot and interpret the output of JAGS/BUGS (i.e. coda files). Note that the 'Discrete' part of the name is from historical reasons: many of the utilily functions in this package are actually applicable to continuous random variables as well.


Installation
------------

Assuming ``devtools`` is installed, type:

```r
require(devtools)
install_github('alexandrebouchard/coda-discrete-utils')
```


Usage
-----

Let's say you have a model like that (``dice-model.bugs``):

```r
model {
  # Here we define 'face_probabilities', a vector, 
  # where for example face_probabilities[2] represents the probability to get a 2 on the dice.
  face_probabilities[1] <- 1/6
  face_probabilities[2] <- 1/6
  face_probabilities[3] <- 1/6
  face_probabilities[4] <- 1/6
  face_probabilities[5] <- 1/6
  face_probabilities[6] <- 1/6

  # Here we define two independent dice, D1 and D2, with the probability of each face given by face_probabilities
  D1 ~ dcat(face_probabilities)
  D2 ~ dcat(face_probabilities)
  
  # Finally, we construct a third random variable S by adding the two independent dice.
  S <- D1 + D2
}
```

To create a PMC and CDF for this model, use the following R script:

```r
require(rjags)
require(coda.discrete.utils)

# This reads in the model you wrote in the file dice-model.bugs.
dice.model <- jags.model('dice-model.bugs')

dice.samples <- 
  jags.samples(dice.model,
               c('D1', 'S'), 
               1000000) # The number of times we want to repeat this experiment
               
 # This plots the Probability mass functions (PMFs) of the variables D1 and T listed in jags.samples above
coda.pmf(dice.samples) 

# Same for CDF
coda.cdf(dice.samples)
```

Note that you can also print the tables used to create the plots with the option ``show.table = TRUE``:

```r
coda.pmf(dice.samples, show.table = TRUE)``
coda.cdf(dice.samples, show.table = TRUE)``
```

As an additional utility, you can also approximate the expectation and variance of random variables sampled by JAGS (not necessarily discrete in this case) using:

```r
require(rjags)
require(coda.discrete.utils)

# This reads in the model you wrote in the file dice-model.bugs.
dice.model <- jags.model('dice-model.bugs')

dice.samples <- 
  jags.samples(dice.model,
               c('D1', 'S'), 
               1000000) # The number of times we want to repeat this experiment
               
# This outputs the expectations
coda.expectation(dice.samples) 

# This outputs the variances
coda.variance(dice.samples)
```

The package also contains similar utilities for continuous random variables (redundant from other packages such as ggmcmc, but added so that all the functions behave similarly, i.e. create separate files for each variable). 

Suppose you have the following trivial bugs model:

```r
model {
  
  X ~ dnorm(0, 1)
  Y ~ dnorm(0, 2)

}
```

you can now use the following to print density and joint densities estimates:

```r
require(rjags)
require(coda.discrete.utils)

model <- jags.model(
  'model.bugs')

samples <- 
  jags.samples(model,
               c('X', 'Y'), 
               10000) 
               
coda.density2d(samples, "X", "Y")
coda.density(samples)
```
