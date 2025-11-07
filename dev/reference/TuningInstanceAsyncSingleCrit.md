# Single Criterion Tuning with Rush

The `TuningInstanceAsyncSingleCrit` specifies a tuning problem for a
[TunerAsync](https://mlr3tuning.mlr-org.com/dev/reference/TunerAsync.md).
The function
[`ti_async()`](https://mlr3tuning.mlr-org.com/dev/reference/ti_async.md)
creates a TuningInstanceAsyncSingleCrit and the function
[`tune()`](https://mlr3tuning.mlr-org.com/dev/reference/tune.md) creates
an instance internally.

## Details

The instance contains an
[ObjectiveTuningAsync](https://mlr3tuning.mlr-org.com/dev/reference/ObjectiveTuningAsync.md)
object that encodes the black box objective function a
[Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md) has to
optimize. The instance allows the basic operations of querying the
objective at design points (`$eval_async()`). This operation is usually
done by the
[Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md).
Hyperparameter configurations are asynchronously sent to workers and
evaluated by calling
[`mlr3::resample()`](https://mlr3.mlr-org.com/reference/resample.html).
The evaluated hyperparameter configurations are stored in the
[ArchiveAsyncTuning](https://mlr3tuning.mlr-org.com/dev/reference/ArchiveAsyncTuning.md)
(`$archive`). Before a batch is evaluated, the
[bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html)
is queried for the remaining budget. If the available budget is
exhausted, an exception is raised, and no further evaluations can be
performed from this point on. The tuner is also supposed to store its
final result, consisting of a selected hyperparameter configuration and
associated estimated performance values, by calling the method
`instance$.assign_result`.

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

## Analysis

For analyzing the tuning results, it is recommended to pass the
[ArchiveAsyncTuning](https://mlr3tuning.mlr-org.com/dev/reference/ArchiveAsyncTuning.md)
to
[`as.data.table()`](https://rdatatable.gitlab.io/data.table/reference/as.data.table.html).
The returned data table contains the
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
for each hyperparameter evaluation.

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

## Extension Packages

mlr3tuning is extended by the following packages.

- [mlr3tuningspaces](https://github.com/mlr-org/mlr3tuningspaces) is a
  collection of search spaces from scientific articles for commonly used
  learners.

- [mlr3hyperband](https://github.com/mlr-org/mlr3hyperband) adds the
  Hyperband and Successive Halving algorithm.

- [mlr3mbo](https://github.com/mlr-org/mlr3mbo) adds Bayesian
  optimization methods.

## Super classes

[`bbotk::OptimInstance`](https://bbotk.mlr-org.com/reference/OptimInstance.html)
-\>
[`bbotk::OptimInstanceAsync`](https://bbotk.mlr-org.com/reference/OptimInstanceAsync.html)
-\>
[`bbotk::OptimInstanceAsyncSingleCrit`](https://bbotk.mlr-org.com/reference/OptimInstanceAsyncSingleCrit.html)
-\> `TuningInstanceAsyncSingleCrit`

## Public fields

- `internal_search_space`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  The search space containing those parameters that are internally
  optimized by the
  [mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html).

## Active bindings

- `result_learner_param_vals`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Param values for the optimal learner call.

## Methods

### Public methods

- [`TuningInstanceAsyncSingleCrit$new()`](#method-TuningInstanceAsyncSingleCrit-new)

- [`TuningInstanceAsyncSingleCrit$assign_result()`](#method-TuningInstanceAsyncSingleCrit-assign_result)

- [`TuningInstanceAsyncSingleCrit$clone()`](#method-TuningInstanceAsyncSingleCrit-clone)

Inherited methods

- [`bbotk::OptimInstance$format()`](https://bbotk.mlr-org.com/reference/OptimInstance.html#method-format)
- [`bbotk::OptimInstanceAsync$clear()`](https://bbotk.mlr-org.com/reference/OptimInstanceAsync.html#method-clear)
- [`bbotk::OptimInstanceAsync$print()`](https://bbotk.mlr-org.com/reference/OptimInstanceAsync.html#method-print)
- [`bbotk::OptimInstanceAsync$reconnect()`](https://bbotk.mlr-org.com/reference/OptimInstanceAsync.html#method-reconnect)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TuningInstanceAsyncSingleCrit$new(
      task,
      learner,
      resampling,
      measure = NULL,
      terminator,
      search_space = NULL,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      check_values = FALSE,
      callbacks = NULL,
      rush = NULL
    )

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html))  
  Task to operate on.

- `learner`:

  ([mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html))  
  Learner to tune.

- `resampling`:

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

- `measure`:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
  Measure to optimize. If `NULL`, default measure is used.

- `terminator`:

  ([bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html))  
  Stop criterion of the tuning process.

- `search_space`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Hyperparameter search space. If `NULL` (default), the search space is
  constructed from the
  [paradox::TuneToken](https://paradox.mlr-org.com/reference/to_tune.html)
  of the learner's parameter set (learner\$param_set).

- `store_benchmark_result`:

  (`logical(1)`)  
  If `TRUE` (default), store resample result of evaluated hyperparameter
  configurations in archive as
  [mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html).

- `store_models`:

  (`logical(1)`)  
  If `TRUE`, fitted models are stored in the benchmark result
  (`archive$benchmark_result`). If `store_benchmark_result = FALSE`,
  models are only stored temporarily and not accessible after the
  tuning. This combination is needed for measures that require a model.

- `check_values`:

  (`logical(1)`)  
  If `TRUE`, hyperparameter values are checked before evaluation and
  performance scores after. If `FALSE` (default), values are unchecked
  but computational overhead is reduced.

- `callbacks`:

  (list of
  [mlr3misc::Callback](https://mlr3misc.mlr-org.com/reference/Callback.html))  
  List of callbacks.

- `rush`:

  (`Rush`)  
  If a rush instance is supplied, the tuning runs without batches.

------------------------------------------------------------------------

### Method `assign_result()`

The
[TunerAsync](https://mlr3tuning.mlr-org.com/dev/reference/TunerAsync.md)
object writes the best found point and estimated performance value here.
For internal use.

#### Usage

    TuningInstanceAsyncSingleCrit$assign_result(
      xdt,
      y,
      learner_param_vals = NULL,
      extra = NULL,
      ...
    )

#### Arguments

- `xdt`:

  ([`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  Hyperparameter values as
  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html).
  Each row is one configuration. Contains values in the search space.
  Can contain additional columns for extra information.

- `y`:

  (`numeric(1)`)  
  Optimal outcome.

- `learner_param_vals`:

  (List of named `list()s`)  
  Fixed parameter values of the learner that are neither part of the

- `extra`:

  ([`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  Additional information.

- `...`:

  (`any`)  
  ignored.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TuningInstanceAsyncSingleCrit$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
