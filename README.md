
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
iterated racing and different termination criteria can be set and
combined. `AutoTuner` provides a convenient way to perform nested
resampling in combination with `mlr3`. The package is build on
[bbotk](https://github.com/mlr-org/bbotk) which provides a common
framework for optimization.

## Resources

  - mlr3book chapters on [tuning search
    spaces](https://mlr3book.mlr-org.com/optimization.html#searchspace),
    [hyperparameter
    tuning](https://mlr3book.mlr-org.com/optimization.html#tuning) and
    [nested
    resampling](https://mlr3book.mlr-org.com/optimization.html#nested-resampling).
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

# retrieve task
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
    ##  1: -6.355096  0.2786458 0.0017378683            0.024 2021-09-02 13:45:24        1 <ResampleResult[20]>
    ##  2: -5.937751  0.2799479 0.0026379549            0.025 2021-09-02 13:45:24        1 <ResampleResult[20]>
    ##  3: -4.280177  0.2734375 0.0138402055            0.023 2021-09-02 13:45:24        1 <ResampleResult[20]>
    ##  4: -7.539553  0.2786458 0.0005316351            0.053 2021-09-02 13:45:24        1 <ResampleResult[20]>
    ##  5: -7.140496  0.2786458 0.0007923588            0.027 2021-09-02 13:45:24        1 <ResampleResult[20]>
    ##  6: -9.114735  0.2786458 0.0001100325            0.021 2021-09-02 13:45:25        2 <ResampleResult[20]>
    ##  7: -5.401998  0.2695312 0.0045075636            0.022 2021-09-02 13:45:25        2 <ResampleResult[20]>
    ##  8: -4.703298  0.2773438 0.0090653290            0.022 2021-09-02 13:45:25        2 <ResampleResult[20]>
    ##  9: -2.774656  0.2617188 0.0623709163            0.022 2021-09-02 13:45:25        2 <ResampleResult[20]>
    ## 10: -4.441283  0.2734375 0.0117808090            0.024 2021-09-02 13:45:25        2 <ResampleResult[20]>

``` r
# fit final model on complete data set
learner$param_set$values = instance$result_learner_param_vals
learner$train(task)
```

### Automatic tuning

``` r
# retrieve task
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
    ##  1: -6.469604  0.2671051 0.0015498397            0.022 2021-09-02 13:45:26        1 <ResampleResult[20]>
    ##  2: -5.975552  0.2671051 0.0025400997            0.022 2021-09-02 13:45:26        1 <ResampleResult[20]>
    ##  3: -6.304555  0.2671051 0.0018279594            0.022 2021-09-02 13:45:26        1 <ResampleResult[20]>
    ##  4: -2.885359  0.2703730 0.0558347447            0.022 2021-09-02 13:45:26        1 <ResampleResult[20]>
    ##  5: -5.878631  0.2671051 0.0027986148            0.038 2021-09-02 13:45:26        1 <ResampleResult[20]>
    ##  6: -5.316384  0.2671051 0.0049104786            0.022 2021-09-02 13:45:26        2 <ResampleResult[20]>
    ##  7: -4.159136  0.2590228 0.0156210471            0.021 2021-09-02 13:45:26        2 <ResampleResult[20]>
    ##  8: -9.206399  0.2671051 0.0001003949            0.024 2021-09-02 13:45:26        2 <ResampleResult[20]>
    ##  9: -4.869814  0.2785350 0.0076747945            0.023 2021-09-02 13:45:26        2 <ResampleResult[20]>
    ## 10: -6.649454  0.2671051 0.0012947286            0.022 2021-09-02 13:45:26        2 <ResampleResult[20]>

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
# retrieve task
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
    ## 1: <TaskClassif[47]>    pima <AutoTuner[40]> classif.rpart.tuned <ResamplingCV[19]>            cv         1 <PredictionClassif[19]>  0.2187500
    ## 2: <TaskClassif[47]>    pima <AutoTuner[40]> classif.rpart.tuned <ResamplingCV[19]>            cv         2 <PredictionClassif[19]>  0.2500000
    ## 3: <TaskClassif[47]>    pima <AutoTuner[40]> classif.rpart.tuned <ResamplingCV[19]>            cv         3 <PredictionClassif[19]>  0.2734375

``` r
# inner resampling results
extract_inner_tuning_results(rr)
```

    ##    iteration        cp classif.ce learner_param_vals  x_domain task_id          learner_id resampling_id
    ## 1:         1 -2.768620  0.2573099          <list[2]> <list[1]>    pima classif.rpart.tuned            cv
    ## 2:         2 -3.880799  0.2046784          <list[2]> <list[1]>    pima classif.rpart.tuned            cv
    ## 3:         3 -8.862942  0.2748538          <list[2]> <list[1]>    pima classif.rpart.tuned            cv

``` r
# inner resampling archives
extract_inner_tuning_archives(rr)
```

    ##     iteration        cp classif.ce  x_domain_cp runtime_learners           timestamp batch_nr      resample_result task_id          learner_id resampling_id
    ##  1:         1 -4.539772  0.2748538 0.0106758449            0.008 2021-09-02 13:45:27        1 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ##  2:         1 -7.559936  0.2631579 0.0005209086            0.007 2021-09-02 13:45:27        1 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ##  3:         1 -8.648543  0.2631579 0.0001753822            0.007 2021-09-02 13:45:27        1 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ##  4:         1 -6.297959  0.2631579 0.0018400560            0.008 2021-09-02 13:45:27        1 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ##  5:         1 -8.947182  0.2631579 0.0001301033            0.007 2021-09-02 13:45:27        1 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ##  6:         1 -8.067483  0.2631579 0.0003135715            0.008 2021-09-02 13:45:27        2 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ##  7:         1 -8.350241  0.2631579 0.0002363396            0.007 2021-09-02 13:45:27        2 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ##  8:         1 -5.913481  0.2631579 0.0027027622            0.007 2021-09-02 13:45:27        2 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ##  9:         1 -8.752513  0.2631579 0.0001580636            0.008 2021-09-02 13:45:27        2 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 10:         1 -2.768620  0.2573099 0.0627485302            0.008 2021-09-02 13:45:27        2 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 11:         2 -3.286175  0.2105263 0.0373966171            0.007 2021-09-02 13:45:27        1 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 12:         2 -4.124071  0.2456140 0.0161785202            0.007 2021-09-02 13:45:27        1 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 13:         2 -2.385855  0.2105263 0.0920102553            0.007 2021-09-02 13:45:27        1 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 14:         2 -3.880799  0.2046784 0.0206343371            0.007 2021-09-02 13:45:27        1 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 15:         2 -4.328644  0.2456140 0.0131854101            0.007 2021-09-02 13:45:27        1 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 16:         2 -4.394274  0.2456140 0.0123478454            0.007 2021-09-02 13:45:28        2 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 17:         2 -5.922306  0.2690058 0.0026790151            0.007 2021-09-02 13:45:28        2 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 18:         2 -7.331236  0.2690058 0.0006547636            0.007 2021-09-02 13:45:28        2 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 19:         2 -4.992721  0.2690058 0.0067871745            0.007 2021-09-02 13:45:28        2 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 20:         2 -3.362562  0.2105263 0.0346463879            0.008 2021-09-02 13:45:28        2 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 21:         3 -8.862942  0.2748538 0.0001415380            0.007 2021-09-02 13:45:28        1 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 22:         3 -7.347079  0.2748538 0.0006444723            0.007 2021-09-02 13:45:28        1 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 23:         3 -7.067612  0.2748538 0.0008522663            0.007 2021-09-02 13:45:28        1 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 24:         3 -8.044845  0.2748538 0.0003207512            0.006 2021-09-02 13:45:28        1 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 25:         3 -6.697964  0.2748538 0.0012334207            0.007 2021-09-02 13:45:28        1 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 26:         3 -6.530743  0.2748538 0.0014579225            0.007 2021-09-02 13:45:28        2 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 27:         3 -4.267553  0.2982456 0.0140160370            0.007 2021-09-02 13:45:28        2 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 28:         3 -6.352161  0.2748538 0.0017429764            0.007 2021-09-02 13:45:28        2 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 29:         3 -6.222573  0.2748538 0.0019841342            0.007 2021-09-02 13:45:28        2 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ## 30:         3 -3.570023  0.2865497 0.0281551963            0.007 2021-09-02 13:45:28        2 <ResampleResult[20]>    pima classif.rpart.tuned            cv
    ##     iteration        cp classif.ce  x_domain_cp runtime_learners           timestamp batch_nr      resample_result task_id          learner_id resampling_id
