
# mlr3tuning <img src="man/figures/logo.png" align="right" width = "120" />

Package website: [release](https://mlr3tuning.mlr-org.com/) \|
[dev](https://mlr3tuning.mlr-org.com/dev/)

<!-- badges: start -->

[![r-cmd-check](https://github.com/mlr-org/mlr3tuning/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/mlr-org/mlr3tuning/actions/workflows/r-cmd-check.yml)
[![CRAN
Status](https://www.r-pkg.org/badges/version-ago/mlr3tuning)](https://cran.r-project.org/package=mlr3tuning)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
<!-- badges: end -->

*mlr3tuning* is the hyperparameter optimization package of the
[mlr3](https://mlr-org.com/) ecosystem. It features highly configurable
search spaces via the [paradox](https://github.com/mlr-org/paradox)
package and finds optimal hyperparameter configurations for any mlr3
[learner](https://github.com/mlr-org/mlr3learners). mlr3tuning works
with several optimization algorithms e.g. Random Search, Iterated
Racing, Bayesian Optimization (in
[mlr3mbo](https://github.com/mlr-org/mlr3mbo)) and Hyperband (in
[mlr3hyperband](https://github.com/mlr-org/mlr3hyperband)). Moreover, it
can
[automatically](https://mlr3book.mlr-org.com/chapters/chapter4/hyperparameter_optimization.html#sec-autotuner)
optimize learners and estimate the performance of optimized models with
[nested
resampling](https://mlr3book.mlr-org.com/chapters/chapter4/hyperparameter_optimization.html#sec-nested-resampling).
The package is built on the optimization framework
[bbotk](https://github.com/mlr-org/bbotk).

## Extension packages

mlr3tuning is extended by the following packages.

- [mlr3tuningspaces](https://github.com/mlr-org/mlr3tuningspaces) is a
  collection of search spaces from scientific articles for commonly used
  learners.
- [mlr3hyperband](https://github.com/mlr-org/mlr3hyperband) adds the
  Hyperband and Successive Halving algorithm.
- [mlr3mbo](https://github.com/mlr-org/mlr3mbo) adds Bayesian
  Optimization methods.

## Resources

There are several sections about hyperparameter optimization in the
[mlr3book](https://mlr3book.mlr-org.com).

- Getting started with [hyperparameter
  optimization](https://mlr3book.mlr-org.com/chapters/chapter4/hyperparameter_optimization.html).
- An overview of all tuners can be found on our
  [website](https://mlr-org.com/tuners.html).
- [Tune](https://mlr3book.mlr-org.com/chapters/chapter4/hyperparameter_optimization.html#sec-model-tuning)
  a support vector machine on the Sonar data set.
- Learn about [tuning
  spaces](https://mlr3book.mlr-org.com/chapters/chapter4/hyperparameter_optimization.html#sec-defining-search-spaces).
- Estimate the model performance with [nested
  resampling](https://mlr3book.mlr-org.com/chapters/chapter4/hyperparameter_optimization.html#sec-nested-resampling).
- Learn about [multi-objective
  optimization](https://mlr3book.mlr-org.com/chapters/chapter5/advanced_tuning_methods_and_black_box_optimization.html#sec-multi-metrics-tuning).
- Simultaneously optimize hyperparameters and use [early
  stopping](https://mlr3book.mlr-org.com/chapters/chapter15/predsets_valid_inttune.html)
  with XGBoost.
- [Automate](https://mlr3book.mlr-org.com/chapters/chapter4/hyperparameter_optimization.html#sec-autotuner)
  the tuning.

The [gallery](https://mlr-org.com/gallery-all-optimization.html)
features a collection of case studies and demos about optimization.

- Learn more advanced methods with the [Practical Tuning
  Series](https://mlr-org.com/gallery/series/2021-03-09-practical-tuning-series-tune-a-support-vector-machine/).
- Learn about
  [hotstarting](https://mlr-org.com/gallery/optimization/2023-01-16-hotstart/)
  models.
- Run the [default hyperparameter
  configuration](https://mlr-org.com/gallery/optimization/2023-01-31-default-configuration/)
  of learners as a baseline.
- Use the
  [Hyperband](https://mlr-org.com/gallery/series/2023-01-15-hyperband-xgboost/)
  optimizer with different budget parameters.

The [cheatsheet](https://cheatsheets.mlr-org.com/mlr3tuning.pdf)
summarizes the most important functions of mlr3tuning.

## Installation

Install the last release from CRAN:

``` r
install.packages("mlr3tuning")
```

Install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("mlr-org/mlr3tuning")
```

## Examples

We optimize the `cost` and `gamma` hyperparameters of a support vector
machine on the
[Sonar](https://mlr3.mlr-org.com/reference/mlr_tasks_sonar.html) data
set.

``` r
library("mlr3learners")
library("mlr3tuning")

learner = lrn("classif.svm",
  cost  = to_tune(1e-5, 1e5, logscale = TRUE),
  gamma = to_tune(1e-5, 1e5, logscale = TRUE),
  kernel = "radial",
  type = "C-classification"
)
```

We construct a tuning instance with the `ti()` function. The tuning
instance describes the tuning problem.

``` r
instance = ti(
  task = tsk("sonar"),
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measures = msr("classif.ce"),
  terminator = trm("none")
)
instance
```

    ## 
    ## ── <TuningInstanceBatchSingleCrit> ─────────────────────────────────────────────────────────────────
    ## • State: Not optimized
    ## • Objective: <ObjectiveTuningBatch>
    ## • Search Space:
    ##       id    class     lower    upper nlevels
    ## 1:  cost ParamDbl -11.51293 11.51293     Inf
    ## 2: gamma ParamDbl -11.51293 11.51293     Inf
    ## • Terminator: <TerminatorNone>

We select a simple grid search as the optimization algorithm.

``` r
tuner = tnr("grid_search", resolution = 5)
tuner
```

    ## 
    ## ── <TunerBatchGridSearch>: Grid Search ─────────────────────────────────────────────────────────────
    ## • Parameters: batch_size=1, resolution=5
    ## • Parameter classes: <ParamLgl>, <ParamInt>, <ParamDbl>, and <ParamFct>
    ## • Properties: dependencies, single-crit, and multi-crit
    ## • Packages: mlr3tuning and bbotk

To start the tuning, we simply pass the tuning instance to the tuner.

``` r
tuner$optimize(instance)
```

    ##        cost     gamma learner_param_vals  x_domain classif.ce
    ## 1: 5.756463 -5.756463          <list[4]> <list[2]>  0.1828847

The tuner returns the best hyperparameter configuration and the
corresponding measured performance.

The archive contains all evaluated hyperparameter configurations.

``` r
as.data.table(instance$archive)[, .(cost, gamma, classif.ce, batch_nr, resample_result)]
```

    ##           cost      gamma classif.ce batch_nr  resample_result
    ##  1:  -5.756463   5.756463  0.4663216        1 <ResampleResult>
    ##  2:   5.756463  -5.756463  0.1828847        2 <ResampleResult>
    ##  3:  11.512925   5.756463  0.4663216        3 <ResampleResult>
    ##  4:   5.756463  11.512925  0.4663216        4 <ResampleResult>
    ##  5: -11.512925 -11.512925  0.4663216        5 <ResampleResult>
    ## ---                                                           
    ## 21:  -5.756463  -5.756463  0.4663216       21 <ResampleResult>
    ## 22:  11.512925  11.512925  0.4663216       22 <ResampleResult>
    ## 23: -11.512925  11.512925  0.4663216       23 <ResampleResult>
    ## 24:  11.512925  -5.756463  0.1828847       24 <ResampleResult>
    ## 25:   0.000000  -5.756463  0.2402346       25 <ResampleResult>

The [mlr3viz](https://mlr3viz.mlr-org.com/) package visualizes tuning
results.

``` r
library(mlr3viz)

autoplot(instance, type = "surface")
```

<img src="man/figures/plot.png"/>

We fit a final model with optimized hyperparameters to make predictions
on new data.

``` r
learner$param_set$values = instance$result_learner_param_vals
learner$train(tsk("sonar"))
```
