
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

### Basic hyperparameter tuning

``` r
library("mlr3tuning")

# task
task = tsk("pima")

# load learner and set search space
learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))

# hyperparameter tuning on the pima indians diabetes data set
instance = tune(
  method = "random_search",
  task = task,
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
    ## 1: -2.774656          <list[2]> <list[1]>  0.2617188

``` r
# all evaluated hyperparameter configuration
as.data.table(instance$archive)
```

    ##            cp classif.ce  x_domain_cp runtime_learners           timestamp batch_nr      resample_result
    ##  1: -5.401998  0.2695312 0.0045075636            0.063 2021-06-08 15:20:00        2 <ResampleResult[20]>
    ##  2: -7.140496  0.2786458 0.0007923588            0.078 2021-06-08 15:19:59        1 <ResampleResult[20]>
    ##  3: -6.355096  0.2786458 0.0017378683            0.057 2021-06-08 15:19:59        1 <ResampleResult[20]>
    ##  4: -2.774656  0.2617188 0.0623709163            0.046 2021-06-08 15:20:00        2 <ResampleResult[20]>
    ##  5: -4.703298  0.2773438 0.0090653290            0.053 2021-06-08 15:20:00        2 <ResampleResult[20]>
    ##  6: -4.441283  0.2734375 0.0117808090            0.050 2021-06-08 15:20:00        2 <ResampleResult[20]>
    ##  7: -4.280177  0.2734375 0.0138402055            0.070 2021-06-08 15:19:59        1 <ResampleResult[20]>
    ##  8: -9.114735  0.2786458 0.0001100325            0.062 2021-06-08 15:20:00        2 <ResampleResult[20]>
    ##  9: -7.539553  0.2786458 0.0005316351            0.053 2021-06-08 15:19:59        1 <ResampleResult[20]>
    ## 10: -5.937751  0.2799479 0.0026379549            0.073 2021-06-08 15:19:59        1 <ResampleResult[20]>

``` r
# fit final model on complete data set
learner$param_set$values = instance$result_learner_param_vals
learner$train(task)
```

### Automatic tuning

``` r
# task
task = tsk("pima")

# construct auto tuner
at = auto_tuner(
  method = "random_search",
  learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.ce"),
  term_evals = 10,
  batch_size = 5
)

# train/test split
train_set = sample(task$nrow, 0.8 * task$nrow)
test_set = setdiff(seq_len(task$nrow), train_set)

# tune hyperparameters and fit final model on the complete data set in one go
at$train(task, row_ids = train_set)

# best performing hyperparameter configuration
at$tuning_result
```

    ##           cp learner_param_vals  x_domain classif.ce
    ## 1: -4.159136          <list[2]> <list[1]>  0.2590228

``` r
# all evaluated hyperparameter configuration
as.data.table(at$archive)
```

    ##            cp classif.ce  x_domain_cp runtime_learners           timestamp batch_nr      resample_result
    ##  1: -5.975552  0.2671051 0.0025400997            0.033 2021-06-08 15:20:01        1 <ResampleResult[20]>
    ##  2: -6.469604  0.2671051 0.0015498397            0.041 2021-06-08 15:20:01        1 <ResampleResult[20]>
    ##  3: -6.304555  0.2671051 0.0018279594            0.037 2021-06-08 15:20:01        1 <ResampleResult[20]>
    ##  4: -4.159136  0.2590228 0.0156210471            0.033 2021-06-08 15:20:02        2 <ResampleResult[20]>
    ##  5: -9.206399  0.2671051 0.0001003949            0.039 2021-06-08 15:20:02        2 <ResampleResult[20]>
    ##  6: -6.649454  0.2671051 0.0012947286            0.033 2021-06-08 15:20:02        2 <ResampleResult[20]>
    ##  7: -5.316384  0.2671051 0.0049104786            0.037 2021-06-08 15:20:02        2 <ResampleResult[20]>
    ##  8: -2.885359  0.2703730 0.0558347447            0.031 2021-06-08 15:20:01        1 <ResampleResult[20]>
    ##  9: -5.878631  0.2671051 0.0027986148            0.064 2021-06-08 15:20:01        1 <ResampleResult[20]>
    ## 10: -4.869814  0.2785350 0.0076747945            0.034 2021-06-08 15:20:02        2 <ResampleResult[20]>

``` r
# predict new data with model trained on the complete data set and optimized hyperparameters
at$predict(task, row_ids = test_set)
```

    ## <PredictionClassif> for 154 observations:
    ##     row_ids truth response
    ##           3   pos      pos
    ##           6   neg      neg
    ##          11   neg      neg
    ## ---                       
    ##         756   pos      pos
    ##         758   pos      pos
    ##         768   neg      neg

### Nested resampling

``` r
# task
task = tsk("pima")

# load learner and set search space
learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))

# nested resampling
rr = tune_nested(
  method = "random_search",
  task =  task,
  learner = learner,
  inner_resampling = rsmp("holdout"),
  outer_resampling = rsmp("cv", folds = 3),
  measure = msr("classif.ce"),
  term_evals = 10,
  batch_size = 5
)

# aggregated performance of all outer resampling iterations
rr$aggregate()
```

    ## classif.ce 
    ##  0.2473958

``` r
# performance scores of the outer resampling
rr$score()
```

    ##                 task task_id         learner          learner_id         resampling resampling_id iteration              prediction classif.ce
    ## 1: <TaskClassif[47]>    pima <AutoTuner[39]> classif.rpart.tuned <ResamplingCV[19]>            cv         1 <PredictionClassif[19]>  0.2187500
    ## 2: <TaskClassif[47]>    pima <AutoTuner[39]> classif.rpart.tuned <ResamplingCV[19]>            cv         2 <PredictionClassif[19]>  0.2500000
    ## 3: <TaskClassif[47]>    pima <AutoTuner[39]> classif.rpart.tuned <ResamplingCV[19]>            cv         3 <PredictionClassif[19]>  0.2734375

``` r
# inner resampling results
extract_inner_tuning_results(rr)
```

    ##    iteration        cp classif.ce learner_param_vals  x_domain task_id          learner_id resampling_id
    ## 1:         1 -3.383571  0.2456140          <list[2]> <list[1]>    pima classif.rpart.tuned            cv
    ## 2:         2 -3.880799  0.2046784          <list[2]> <list[1]>    pima classif.rpart.tuned            cv
    ## 3:         3 -8.862942  0.2748538          <list[2]> <list[1]>    pima classif.rpart.tuned            cv

``` r
# inner resampling archives
extract_inner_tuning_archives(rr)
```

    ##      iteration        cp classif.ce  x_domain_cp runtime_learners           timestamp batch_nr      resample_result task_id          learner_id resampling_id
    ##   1:         1 -3.759055  0.2631579 0.0233057429            0.010 2021-06-08 15:20:05        6 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ##   2:         1 -8.752513  0.2631579 0.0001580636            0.010 2021-06-08 15:20:03        2 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ##   3:         1 -2.743448  0.2573099 0.0643480889            0.010 2021-06-08 15:20:04        5 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ##   4:         1 -5.786068  0.2631579 0.0030700313            0.014 2021-06-08 15:20:09       15 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ##   5:         1 -2.700986  0.2573099 0.0671392930            0.013 2021-06-08 15:20:07       12 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ##  ---                                                                                                                                                         
    ## 316:         3 -4.181416  0.2982456 0.0152768599            0.011 2021-06-08 15:20:31       16 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 317:         3 -8.752838  0.2748538 0.0001580123            0.010 2021-06-08 15:20:28       10 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 318:         3 -7.161180  0.2748538 0.0007761382            0.010 2021-06-08 15:20:27        8 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 319:         3 -4.434322  0.2982456 0.0118631027            0.010 2021-06-08 15:20:24        3 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 320:         3 -5.041857  0.2748538 0.0064617407            0.013 2021-06-08 15:20:30       14 <ResampleResult[20]>    pima classif.rpart.tuned            cv
