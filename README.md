
# mlr3tuning

Package website: [release](https://mlr3tuning.mlr-org.com/) |
[dev](https://mlr3tuning.mlr-org.com/dev/)

<!-- badges: start -->

[![tic](https://github.com/mlr-org/mlr3tuning/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mlr3tuning/actions)
[![CRAN
Status](https://www.r-pkg.org/badges/version-ago/mlr3tuning)](https://cran.r-project.org/package=mlr3tuning)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
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
[automatically](https://mlr3book.mlr-org.com/optimization.html#sec-autotuner)
optimize learners and estimate the performance of optimized models with
[nested
resampling](https://mlr3book.mlr-org.com/optimization.html#sec-nested-resampling).
The package is built on the optimization framework
[bbotk](https://github.com/mlr-org/bbotk).

## Extension packages

mlr3tuning is extended by the following packages.

  - [mlr3tuningspaces](https://github.com/mlr-org/mlr3tuningspaces) is a
    collection of search spaces from scientific articles for commonly
    used learners.
  - [mlr3hyperband](https://github.com/mlr-org/mlr3hyperband) adds the
    Hyperband and Successive Halving algorithm.
  - [mlr3mbo](https://github.com/mlr-org/mlr3mbo) adds Bayesian
    Optimization methods.

## Resources

There are several sections about hyperparameter optimization in the
[mlr3book](https://mlr3book.mlr-org.com).

  - Getting started with [Hyperparameter
    Optimization](https://mlr3book.mlr-org.com/optimization.html)
  - [Tune](https://mlr3book.mlr-org.com/optimization.html#sec-tuning-instance)
    a simple classification tree on the Palmer Penguins data set.
  - Learn about [Tuning
    Spaces](https://mlr3book.mlr-org.com/technical.html#sec-tuning-space).
  - Estimate Model Performance with [Nested
    Resampling](https://mlr3book.mlr-org.com/optimization.html#sec-model-performance).

The [gallery](https://mlr-org.com/gallery.html#category:tuning) features
a collection of case studies and demos about optimization.

  - [Practical Tuning
    Series](https://mlr-org.com/gallery.html#category:practical_tuning_series)
  - [Tuning Search
    Spaces](https://mlr3book.mlr-org.com/optimization.html#searchspace)
  - [Nested
    Resampling](https://mlr3book.mlr-org.com/optimization.html#nested-resampling)

The [cheatsheet](https://cheatsheets.mlr-org.com/mlr3tuning.pdf)
summarizes the most important functions of mlr3tuning.

## Installation

Install the last release from CRAN:

``` r
install.packages("mlr3tuning")
```

Install the development version from GitHub:

``` r
remotes::install_github("mlr-org/mlr3tuning")
```

## Examples

### Basic Hyperparameter Optimization

``` r
library("mlr3tuning")

# load learner and set search space
learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))

# hyperparameter tuning on the Pima Indians Diabetes data set
instance = tune(
  method = tnr("random_search"),
  task = tsk("pima"),
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.ce"),
  term_evals = 10,
  batch_size = 5
)

# best performing hyperparameter configuration
instance$result
```

    ##           cp learner_param_vals  x_domain classif.ce
    ## 1: -3.879799          <list[2]> <list[1]>       0.25

``` r
# all evaluated hyperparameter configuration
as.data.table(instance$archive)[, list(batch_nr, cp, classif.ce, resample_result)]
```

    ##     batch_nr        cp classif.ce      resample_result
    ##  1:        1 -6.355096  0.2786458 <ResampleResult[21]>
    ##  2:        2 -7.539553  0.2786458 <ResampleResult[21]>
    ##  3:        3 -3.879799  0.2500000 <ResampleResult[21]>
    ##  4:        4 -4.703298  0.2773438 <ResampleResult[21]>
    ##  5:        5 -7.457379  0.2786458 <ResampleResult[21]>
    ##  6:        6 -8.298748  0.2786458 <ResampleResult[21]>
    ##  7:        7 -6.290041  0.2786458 <ResampleResult[21]>
    ##  8:        8 -7.631330  0.2786458 <ResampleResult[21]>
    ##  9:        9 -4.470716  0.2734375 <ResampleResult[21]>
    ## 10:       10 -7.082225  0.2786458 <ResampleResult[21]>

``` r
# fit the final model on the complete data set
learner$param_set$values = instance$result_learner_param_vals
learner$train(tsk("pima"))
```

### Automatic Tuning

``` r
# construct auto tuner
at = auto_tuner(
  method = tnr("random_search"),
  learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.ce"),
  term_evals = 10,
  batch_size = 5
)

# train/test split
task = tsk("pima")
train_set = sample(task$nrow, 0.8 * task$nrow)
test_set = setdiff(seq_len(task$nrow), train_set)

# tune hyperparameters and fit the final model on the complete data set in one go
at$train(task, row_ids = train_set)

# best performing hyperparameter configuration
at$tuning_result
```

    ##           cp learner_param_vals  x_domain classif.ce
    ## 1: -2.923195          <list[2]> <list[1]>  0.2426909

``` r
# all evaluated hyperparameter configuration
as.data.table(at$archive)[, list(batch_nr, cp, classif.ce, resample_result)]
```

    ##     batch_nr        cp classif.ce      resample_result
    ##  1:        1 -8.546511  0.2948350 <ResampleResult[21]>
    ##  2:        2 -3.721661  0.2557389 <ResampleResult[21]>
    ##  3:        3 -9.192468  0.2948350 <ResampleResult[21]>
    ##  4:        4 -2.923195  0.2426909 <ResampleResult[21]>
    ##  5:        5 -7.448214  0.2948350 <ResampleResult[21]>
    ##  6:        6 -6.073759  0.2915830 <ResampleResult[21]>
    ##  7:        7 -5.082615  0.2915830 <ResampleResult[21]>
    ##  8:        8 -4.925975  0.2932090 <ResampleResult[21]>
    ##  9:        9 -5.051991  0.2915830 <ResampleResult[21]>
    ## 10:       10 -2.365374  0.2541288 <ResampleResult[21]>

``` r
# predict on new data
at$predict(task, row_ids = test_set)
```

    ## <PredictionClassif> for 154 observations:
    ##     row_ids truth response
    ##           3   pos      neg
    ##           6   neg      neg
    ##          11   neg      neg
    ## ---                       
    ##         761   neg      neg
    ##         766   neg      neg
    ##         767   pos      neg

### Nested resampling

``` r
# construct auto tuner with inner resampling
at = auto_tuner(
  method = tnr("random_search"),
  learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.ce"),
  term_evals = 10,
  batch_size = 5
)

# specify outer resampling
resampling_outer = rsmp("cv", folds = 3)

# run nested resampling
rr = resample(tsk("pima"), at, resampling_outer, store_models = TRUE)

# aggregated performance of all outer resampling iterations
rr$aggregate()
```

    ## classif.ce 
    ##  0.2721354

``` r
# performance scores of the outer resampling
rr$score()[, list(iteration, classif.ce)]
```

    ##    iteration classif.ce
    ## 1:         1  0.2968750
    ## 2:         2  0.2695312
    ## 3:         3  0.2500000

``` r
# inner resampling results
extract_inner_tuning_results(rr)[, list(iteration, cp, classif.ce)]
```

    ##    iteration        cp classif.ce
    ## 1:         1 -3.953194  0.2324848
    ## 2:         2 -2.575506  0.2519207
    ## 3:         3 -3.357545  0.2733861

``` r
# inner resampling archives
extract_inner_tuning_archives(rr)[, list(iteration, cp, classif.ce)]
```

    ##     iteration        cp classif.ce
    ##  1:         1 -7.772494  0.2480908
    ##  2:         1 -3.383309  0.2383328
    ##  3:         1 -5.479227  0.2480908
    ##  4:         1 -5.032661  0.2480908
    ##  5:         1 -3.953194  0.2324848
    ## ---                               
    ## 26:         3 -6.172105  0.2948515
    ## 27:         3 -3.819965  0.2772732
    ## 28:         3 -8.426387  0.2948515
    ## 29:         3 -3.357545  0.2733861
    ## 30:         3 -9.201862  0.2948515

### Hotstart

``` r
library("mlr3learners")

# load learner and set search space
learner = lrn("classif.xgboost",
  eta = to_tune(),
  nrounds = to_tune(500, 2500)
)

# hyperparameter tuning on the Pima Indians Diabetes data set
instance = tune(
  method = tnr("grid_search"),
  task = tsk("pima"),
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.ce"),
  allow_hotstart = TRUE,
  resolution = 5,
  batch_size = 5
)

# best performing hyperparameter configuration
instance$result
```

    ##    nrounds       eta learner_param_vals  x_domain classif.ce
    ## 1:     500 0.5555556          <list[4]> <list[2]>  0.2591146

``` r
# all evaluated hyperparameter configuration
as.data.table(instance$archive)[, list(batch_nr, eta, nrounds, classif.ce, resample_result)]
```

    ##      batch_nr       eta nrounds classif.ce      resample_result
    ##   1:        1 0.0000000     500  0.4908854 <ResampleResult[21]>
    ##   2:        2 0.1111111     500  0.2721354 <ResampleResult[21]>
    ##   3:        3 0.2222222     500  0.2734375 <ResampleResult[21]>
    ##   4:        4 0.3333333     500  0.2734375 <ResampleResult[21]>
    ##   5:        5 0.4444444     500  0.2734375 <ResampleResult[21]>
    ##  ---                                                           
    ##  96:       96 0.5555556    2500  0.2656250 <ResampleResult[21]>
    ##  97:       97 0.6666667    2500  0.2747396 <ResampleResult[21]>
    ##  98:       98 0.7777778    2500  0.2838542 <ResampleResult[21]>
    ##  99:       99 0.8888889    2500  0.2851562 <ResampleResult[21]>
    ## 100:      100 1.0000000    2500  0.2721354 <ResampleResult[21]>

``` r
# fit the final model on the complete data set
learner$param_set$values = instance$result_learner_param_vals
learner$train(tsk("pima"))
```
