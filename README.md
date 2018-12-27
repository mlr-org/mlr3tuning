# mlr3tuning

[![Build Status Linux](https://travis-ci.org/mlr-org/mlr3tuning.svg?branch=master)](https://travis-ci.org/mlr-org/mlr3tuning)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)[![Build Status Windows](https://ci.appveyor.com/api/projects/status/github/mlr-org/mlr3tuning?branch=master&svg=true)](https://ci.appveyor.com/project/mlr-org/mlr3tuning)
[![CRAN](https://www.r-pkg.org/badges/version/mlr3tuning)](https://cran.r-project.org/package=mlr3tuning)
[![Coverage Status](https://coveralls.io/repos/github/mlr-org/mlr3tuning/badge.svg?branch=master)](https://coveralls.io/github/mlr-org/mlr3tuning?branch=master)

Extends the [mlr3](https://mlr3.mlr-org.com) package with tuning.

## Installation

```r
devtools::install_github("mlr-org/mlr3tuning")
```



## Functionality

### Implemented Tuning Strategies

-   Random Search: `TunerRandomSearch`
-   Grid Search: `TunerGridSearch`

### Implemented Stopping Criteria

-   Iterations: `TerminatorIterations`
-   Evaluations: `TerminatorEvaluations`


## Vignettes

-   Introduction to Fitness Functions: `tuning-01-fitness-function`
-   Introduction to Tuner: `tuning-02-tuner`
-   Defining Custom Tuner: `tuning-03-custom-tuner`
