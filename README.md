# mlr3tuning

Package website: [release](https://mlr3tuning.mlr-org.com/) | [dev](https://mlr3tuning.mlr-org.com/dev/)

<!-- badges: start -->
[![tic](https://github.com/mlr-org/mlr3tuning/workflows/tic/badge.svg?branch=master)](https://github.com/mlr-org/mlr3tuning/actions)
[![CRAN Status](https://www.r-pkg.org/badges/version-ago/mlr3tuning)](https://cran.r-project.org/package=mlr3tuning)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
[![CodeFactor](https://www.codefactor.io/repository/github/mlr-org/mlr3tuning/badge)](https://www.codefactor.io/repository/github/mlr-org/mlr3tuning)
<!-- badges: end -->

This package provides hyperparameter tuning for
[mlr3](https://mlr3.mlr-org.com). It offers various tuning methods e.g. grid
search, random search and generalized simulated annealing and different
termination criteria can be set and combined. 'AutoTuner' provides a convenient
way to perform nested resampling in combination with 'mlr3'. The package is
build on [bbotk](https://github.com/mlr-org/bbotk) which provides a common
framework for optimization.

## Installation

CRAN version

```r
install.packages("mlr3tuning")
```

Development version

```r
remotes::install_github("mlr-org/mlr3tuning")
```

## Example

```r
library("mlr3")
library("mlr3tuning")
library("paradox")

task = tsk("pima")
learner = lrn("classif.rpart")
resampling = rsmp("holdout")
measure = msr("classif.ce")

# Create the search space with lower and upper bounds
search_space = ParamSet$new(list(
  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
  ParamInt$new("minsplit", lower = 1, upper = 10)
))

# Define termination criterion
terminator = trm("evals", n_evals = 20)

# Create tuning instance
instance = TuningInstanceSingleCrit$new(task = task,
  learner = learner,
  resampling = resampling,
  measure = measure,
  terminator = terminator,
  search_space = search_space)

# Load tuner
tuner = tnr("grid_search", resolution = 5)

# Trigger optimization
tuner$optimize(instance)

# View results
instance$result
```

## Resources

Further documentation can be found in the
[mlr3book](https://mlr3book.mlr-org.com/tuning.html) and the [mlr3tuning
cheatsheet](https://cheatsheets.mlr-org.com/mlr3tuning.pdf). Tutorials are
available in the [mlr3gallery](https://mlr3gallery.mlr-org.com/).

