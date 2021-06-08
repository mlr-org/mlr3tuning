
# mlr3tuning

Package website: [release](https://mlr3tuning.mlr-org.com/) |
[dev](https://mlr3tuning.mlr-org.com/dev/)

<!-- badges: start -->

[![tic](https://github.com/mlr-org/mlr3tuning/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mlr3tuning/actions)
[![CRAN
Status](https://www.r-pkg.org/badges/version-ago/mlr3tuning)](https://cran.r-project.org/package=mlr3tuning)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
[![CodeFactor](https://www.codefactor.io/repository/github/mlr-org/mlr3tuning/badge)](https://www.codefactor.io/repository/github/mlr-org/mlr3tuning)
<!-- badges: end -->

This package provides hyperparameter tuning for
[mlr3](https://mlr3.mlr-org.com). It offers various tuning methods
e.g. grid search, random search and generalized simulated annealing and
different termination criteria can be set and combined. `AutoTuner`
provides a convenient way to perform nested resampling in combination
with `mlr3`. The package is build on
[bbotk](https://github.com/mlr-org/bbotk) which provides a common
framework for optimization.

## Resources

  - mlr3book chapters on [tuning search
    spaces](https://mlr3book.mlr-org.com/searchspace.html),
    [hyperparameter tuning](https://mlr3book.mlr-org.com/tuning.html)
    and [nested
    resampling](https://mlr3book.mlr-org.com/nested-resampling.html).
  - mlr3gallery
    [posts](https://mlr3gallery.mlr-org.com/#category:mlr3tuning)
  - [cheatsheet](https://cheatsheets.mlr-org.com/mlr3tuning.pdf)

## Installation

Install the last release from CRAN:

``` r
install.packages("mlr3tuning")
```

Install the development version from GitHub:

``` r
remotes::install_github("mlr-org/mlr3tuning")
```

## Example

``` r
library("mlr3tuning")
```

    ## Loading required package: mlr3

    ## Loading required package: paradox

``` r
# load learner and set search space
learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))

# hyperparameter tuning on the pima indians diabetes data set
instance = tune(
  method = "random_search",
  task =  tsk("pima"),
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  term_evals = 10,
  batch_size = 5
)

# best performing hyperparameter configuration
instance$result
```

    ##           cp learner_param_vals  x_domain classif.ce
    ## 1: -3.438787          <list[2]> <list[1]>  0.1953125

``` r
# all evaluated hyperparameter configuration
as.data.table(instance$archive)
```

    ##            cp classif.ce  x_domain_cp runtime_learners           timestamp batch_nr      resample_result
    ##  1: -6.157045  0.1992188 0.0021185043            0.009 2021-06-08 13:52:18        2 <ResampleResult[20]>
    ##  2: -6.206153  0.1992188 0.0020169813            0.011 2021-06-08 13:52:18        2 <ResampleResult[20]>
    ##  3: -4.542023  0.1953125 0.0106518336            0.021 2021-06-08 13:52:17        1 <ResampleResult[20]>
    ##  4: -2.977903  0.1953125 0.0508994680            0.010 2021-06-08 13:52:17        1 <ResampleResult[20]>
    ##  5: -7.881995  0.2109375 0.0003774793            0.010 2021-06-08 13:52:18        2 <ResampleResult[20]>
    ##  6: -3.438787  0.1953125 0.0321036084            0.010 2021-06-08 13:52:17        1 <ResampleResult[20]>
    ##  7: -2.571322  0.1953125 0.0764343975            0.009 2021-06-08 13:52:18        2 <ResampleResult[20]>
    ##  8: -5.779360  0.1992188 0.0030906915            0.010 2021-06-08 13:52:17        1 <ResampleResult[20]>
    ##  9: -8.371245  0.2109375 0.0002314272            0.009 2021-06-08 13:52:17        1 <ResampleResult[20]>
    ## 10: -7.655174  0.2109375 0.0004735875            0.010 2021-06-08 13:52:18        2 <ResampleResult[20]>
