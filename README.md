
# Coda Discrete Utils

Utility to plot a PMF and CDF from the output of JAGS/BUGS (i.e. coda files).


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