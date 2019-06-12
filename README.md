# mlr3tuning

[![Build Status Linux](https://travis-ci.org/mlr-org/mlr3tuning.svg?branch=master)](https://travis-ci.org/mlr-org/mlr3tuning) 
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) [![CRAN](https://www.r-pkg.org/badges/version/mlr3tuning)](https://cran.r-project.org/package=mlr3tuning) 
[![Coverage Status](https://coveralls.io/repos/github/mlr-org/mlr3tuning/badge.svg?branch=master)](https://coveralls.io/github/mlr-org/mlr3tuning?branch=master)

Extends the [mlr3](https://mlr3.mlr-org.com) package with tuning.

## Installation

```r
remotes::install_github("mlr-org/mlr3tuning")
```

### Implemented Tuning Strategies

| Method                          | mlr3 class          |
|---------------------------------|---------------------|
| Random Search                   | `TunerRandomSearch` |
| Grid Search                     | `TunerGridSearch`   |
| Generalized Simulated Annealing | `TunerGenSA`        |

### Implemented Stopping Criteria

| Strategy                        | mlr3 class              |
|---------------------------------|-------------------------|
| Evaluations                     | `TerminatorEvaluations` |    
| Runtime                         | `TerminatorRuntime`     | 
| Performance                     | `TerminatorPerformance` | 

## Documentation

All documentation lives in the [mlr3book](https://mlr3book.mlr-org.com/).
For tuning, the following sections exist:

- [Overview of tuning in _mlr3_](https://mlr3book.mlr-org.com/tuning.html)
- [Tuning in nested resampling](https://mlr3book.mlr-org.com/nested-resampling.html#execution)
- [Adding a new tuner to mlr3](https://mlr3book.mlr-org.com/learners-1.html)
