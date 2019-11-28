* Changes in 0.2.7.2: Remove dependence on log-domain. Raise lower bound for base to 4.9.

* Changes in 0.2.7.1: Add PDF instance for Poisson.

* Changes in 0.2.7.0: Add Simplex, fix logBetaPdf, fix binomialPdf and
  binomialCdf to actually use the numerically stable method!

* Changes in 2.6.1: now supports probability density functions and log
  probability density functions via the PDF class, similar to R and
  initially just for the Beta, Binomial, Normal and Uniform
  distributions. The log Binomial probability density function uses
  *Fast and Accurate Computation of Binomial Probabilities* by
  Catherine Loader (this is what is implemented in R and Octave) to
  minimize the occurrence of underflow.

* Changes in 0.2.4.0: Added a Lift instance that resolves a common
  overlapping-instance issue in user code.

* Changes in 0.2.3.1: Should now build on GHC 7.6

* Changes in 0.2.3.0: Added stretched exponential distribution,
  contributed by Ben Gamari.

* Changes in 0.2.2.0: Bug fixes in
  Data.Random.Distribution.Categorical.

* Changes in 0.2.1.1: Changed some one-field data types to newtypes,
  updated types for GHC 7.4's removal of Eq and Show from the context
  of Num, and added RVarT versions of random variables in
  Data.Random.List
