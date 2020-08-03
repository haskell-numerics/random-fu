Random-fu
=========

Random-number generation based on modeling random variables as first-class entities.  This library currently consists of 3 major parts:

rvar
----

This package defines the central datatype of the system, a monad transformer called `RVarT` which extends an arbitrary monad with non-backtracking nondeterminism.  In particular, the `RVar` type (an alias for `RVar Identity`) models pure random variables.

random-source
-------------

This package provides the backend for `RVarT`; arbitrary sources of entropy.  Its design is still in major flux.

random-fu
---------

This package provides an end-user interface that defines random variables following several standard distributions as well as some convenient interfaces for sampling them.

Usage
=====

To use the system, you'll typically want import at least two modules: `Data.Random` for the main interface and a supported entropy source, such as `System.Random.MWC` from the `mwc-random` package.  You may also want to import one or more of the extra distributions provided in the `Data.Random.Distribution` heirarchy (uniform and normal are exported by `Data.Random` automatically).  Then, you can define random variables using `do` notation, sample them using `sampleFrom`, etc.  For example:

    import Data.Random
    import System.Random.MWC (create)
    
    logNormal :: Double -> Double -> RVar Double
    logNormal mu sigmaSq = do
        x <- normal mu sigmaSq
        return (exp x)
    
    main = do
        mwc <- create
        y <- sampleFrom mwc (logNormal 5 1)
        print y

Installation
============

Get the latest release from Hackage:

    cabal install random-fu

Or a bleeding-edge version from github:

    git clone https://github.com/mokus0/random-fu.git
    cd random-fu
    (cd random-source; cabal install)
    (cd rvar;          cabal install)
    (cd random-fu;     cabal install)

Status
======

| Language | CircleCI |
|:--------:|:------:|
| ![GitHub top language](https://img.shields.io/github/languages/top/haskell-numerics/random-fu.svg) | [![Build Status](https://circleci.com/gh/haskell-numerics/random-fu.svg?style=svg)](https://circleci.com/gh/haskell-numerics/random-fu) |
