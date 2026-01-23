# Function for Automatic Tuning

The
[AutoTuner](https://mlr3tuning.mlr-org.com/dev/reference/AutoTuner.md)
wraps a [mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html)
and augments it with an automatic tuning process for a given set of
hyperparameters. The `auto_tuner()` function creates an
[AutoTuner](https://mlr3tuning.mlr-org.com/dev/reference/AutoTuner.md)
object.

## Usage

``` r
auto_tuner(
  tuner,
  learner,
  resampling,
  measure = NULL,
  term_evals = NULL,
  term_time = NULL,
  terminator = NULL,
  search_space = NULL,
  store_tuning_instance = TRUE,
  store_benchmark_result = TRUE,
  store_models = FALSE,
  check_values = FALSE,
  callbacks = NULL,
  rush = NULL,
  id = NULL
)
```

## Arguments

- tuner:

  ([Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md))  
  Optimization algorithm.

- learner:

  ([mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html))  
  Learner to tune.

- resampling:

  ([mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html))  
  Resampling that is used to evaluate the performance of the
  hyperparameter configurations. Uninstantiated resamplings are
  instantiated during construction so that all configurations are
  evaluated on the same data splits. Already instantiated resamplings
  are kept unchanged. Specialized
  [Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md) change
  the resampling e.g. to evaluate a hyperparameter configuration on
  different data splits. This field, however, always returns the
  resampling passed in construction.

- measure:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
  Measure to optimize. If `NULL`, default measure is used.

- term_evals:

  (`integer(1)`)  
  Number of allowed evaluations. Ignored if `terminator` is passed.

- term_time:

  (`integer(1)`)  
  Maximum allowed time in seconds. Ignored if `terminator` is passed.

- terminator:

  ([bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html))  
  Stop criterion of the tuning process.

- search_space:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Hyperparameter search space. If `NULL` (default), the search space is
  constructed from the
  [paradox::TuneToken](https://paradox.mlr-org.com/reference/to_tune.html)
  of the learner's parameter set (learner\$param_set). When using
  [`to_tune()`](https://paradox.mlr-org.com/reference/to_tune.html)
  tokens, dependencies for hierarchical search spaces are automatically
  handled.

- store_tuning_instance:

  (`logical(1)`)  
  If `TRUE` (default), stores the internally created
  [TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/dev/reference/TuningInstanceBatchSingleCrit.md)
  with all intermediate results in slot `$tuning_instance`.

- store_benchmark_result:

  (`logical(1)`)  
  If `TRUE` (default), store resample result of evaluated hyperparameter
  configurations in archive as
  [mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html).

- store_models:

  (`logical(1)`)  
  If `TRUE`, fitted models are stored in the benchmark result
  (`archive$benchmark_result`). If `store_benchmark_result = FALSE`,
  models are only stored temporarily and not accessible after the
  tuning. This combination is needed for measures that require a model.

- check_values:

  (`logical(1)`)  
  If `TRUE`, hyperparameter values are checked before evaluation and
  performance scores after. If `FALSE` (default), values are unchecked
  but computational overhead is reduced.

- callbacks:

  (list of
  [mlr3misc::Callback](https://mlr3misc.mlr-org.com/reference/Callback.html))  
  List of callbacks.

- rush:

  (`Rush`)  
  If a rush instance is supplied, the tuning runs without batches.

- id:

  (`character(1)`)  
  Identifier for the new instance.

## Value

[AutoTuner](https://mlr3tuning.mlr-org.com/dev/reference/AutoTuner.md).

## Details

The
[AutoTuner](https://mlr3tuning.mlr-org.com/dev/reference/AutoTuner.md)
is a [mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html)
which wraps another
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html) and
performs the following steps during `$train()`:

1.  The hyperparameters of the wrapped (inner) learner are trained on
    the training data via resampling. The tuning can be specified by
    providing a
    [Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md), a
    [bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html),
    a search space as
    [paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html),
    a
    [mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html)
    and a
    [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html).

2.  The best found hyperparameter configuration is set as
    hyperparameters for the wrapped (inner) learner stored in
    `at$learner`. Access the tuned hyperparameters via
    `at$tuning_result`.

3.  A final model is fit on the complete training data using the now
    parametrized wrapped learner. The respective model is available via
    field `at$learner$model`.

During `$predict()` the `AutoTuner` just calls the predict method of the
wrapped (inner) learner. A set timeout is disabled while fitting the
final model.

## Default Measures

If no measure is passed, the default measure is used. The default
measure depends on the task type.

|                |                  |                                                               |
|----------------|------------------|---------------------------------------------------------------|
| Task           | Default Measure  | Package                                                       |
| `"classif"`    | `"classif.ce"`   | [mlr3](https://CRAN.R-project.org/package=mlr3)               |
| `"regr"`       | `"regr.mse"`     | [mlr3](https://CRAN.R-project.org/package=mlr3)               |
| `"surv"`       | `"surv.cindex"`  | [mlr3proba](https://CRAN.R-project.org/package=mlr3proba)     |
| `"dens"`       | `"dens.logloss"` | [mlr3proba](https://CRAN.R-project.org/package=mlr3proba)     |
| `"classif_st"` | `"classif.ce"`   | [mlr3spatial](https://CRAN.R-project.org/package=mlr3spatial) |
| `"regr_st"`    | `"regr.mse"`     | [mlr3spatial](https://CRAN.R-project.org/package=mlr3spatial) |
| `"clust"`      | `"clust.dunn"`   | [mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster) |

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

## Nested Resampling

Nested resampling is performed by passing an
[AutoTuner](https://mlr3tuning.mlr-org.com/dev/reference/AutoTuner.md)
to
[`mlr3::resample()`](https://mlr3.mlr-org.com/reference/resample.html)
or
[`mlr3::benchmark()`](https://mlr3.mlr-org.com/reference/benchmark.html).
To access the inner resampling results, set
`store_tuning_instance = TRUE` and execute
[`mlr3::resample()`](https://mlr3.mlr-org.com/reference/resample.html)
or
[`mlr3::benchmark()`](https://mlr3.mlr-org.com/reference/benchmark.html)
with `store_models = TRUE` (see examples). The
[mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html)
passed to the
[AutoTuner](https://mlr3tuning.mlr-org.com/dev/reference/AutoTuner.md)
is meant to be the inner resampling, operating on the training set of an
arbitrary outer resampling. For this reason, the inner resampling should
be not instantiated. If an instantiated resampling is passed, the
[AutoTuner](https://mlr3tuning.mlr-org.com/dev/reference/AutoTuner.md)
fails when a row id of the inner resampling is not present in the
training set of the outer resampling.

## Examples

``` r
at = auto_tuner(
  tuner = tnr("random_search"),
  learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
  resampling = rsmp ("holdout"),
  measure = msr("classif.ce"),
  term_evals = 4)

at$train(tsk("pima"))
```
