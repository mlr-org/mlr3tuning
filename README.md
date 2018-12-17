# mlr3tuning

[![Build Status Linux](https://travis-ci.org/mlr-org/mlr3tuning.svg?branch=master)](https://travis-ci.org/mlr-org/mlr3tuning)
[![Build Status Windows](https://ci.appveyor.com/api/projects/status/github/mlr-org/mlr3tuning?branch=master&svg=true)](https://ci.appveyor.com/project/mlr-org/mlr3tuning)
[![CRAN](https://www.r-pkg.org/badges/version/mlr3tuning)](https://cran.r-project.org/package=mlr3tuning)
[![Coverage Status](https://coveralls.io/repos/github/mlr-org/mlr3tuning/badge.svg?branch=master)](https://coveralls.io/github/mlr-org/mlr3tuning?branch=master)

Extends the [mlr3](https://mlr3.mlr-org.com) package with tuning.

## Installation

```r
devtools::install_github("mlr-org/mlr3tuning")
```

## Usage

### Fitness Functions

An object of the class `FitnessFunction` contains all relevant informations that are necessary to conduct tuning (`mlr3::Task`, `mlr3::Learner`,`mlr3::Resampling`, `mlr3::Measure`s, `paradox::ParamSet`). After defining a fitness function, we can use it to predict the generalization error of a specific learner configuration
defined by it's hyperparameter (using `$eval()`). The `FitnessFunction` class is the basis for further tuning strategies, i.e., grid or random search.

See vignette `tuning-01-fitness-function`. 

### Terminator

### Tuner

