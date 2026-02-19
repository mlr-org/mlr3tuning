# Class for Single Criterion Tuning

The TuningInstanceBatchSingleCrit specifies a tuning problem for a
[Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md). The
function [`ti()`](https://mlr3tuning.mlr-org.com/dev/reference/ti.md)
creates a TuningInstanceBatchSingleCrit and the function
[`tune()`](https://mlr3tuning.mlr-org.com/dev/reference/tune.md) creates
an instance internally.

## Details

The instance contains an
[ObjectiveTuningBatch](https://mlr3tuning.mlr-org.com/dev/reference/ObjectiveTuningBatch.md)
object that encodes the black box objective function a
[Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md) has to
optimize. The instance allows the basic operations of querying the
objective at design points (`$eval_batch()`). This operation is usually
done by the
[Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md).
Evaluations of hyperparameter configurations are performed in batches by
calling
[`mlr3::benchmark()`](https://mlr3.mlr-org.com/reference/benchmark.html)
internally. The evaluated hyperparameter configurations are stored in
the
[ArchiveBatchTuning](https://mlr3tuning.mlr-org.com/dev/reference/ArchiveBatchTuning.md)
(`$archive`). Before a batch is evaluated, the
[bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html)
is queried for the remaining budget. If the available budget is
exhausted, an exception is raised, and no further evaluations can be
performed from this point on. The tuner is also supposed to store its
final result, consisting of a selected hyperparameter configuration and
associated estimated performance values, by calling the method
`instance$assign_result`.

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

## Search Space

The search space defines the hyperparameters to be tuned and their
possible values. It can be specified in two ways:

1.  Tune tokens: Set
    [`to_tune()`](https://paradox.mlr-org.com/reference/to_tune.html)
    tokens in the learner's parameter set and leave
    `search_space = NULL` (default). The search space is automatically
    constructed from the tune tokens. Dependencies are automatically
    handled.

2.  Explicit search space: Pass a
    [paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
    to the `search_space` argument. For search spaces with dependencies,
    use the `depends` argument in `p_*()`.

## Extension Packages

mlr3tuning is extended by the following packages.

- [mlr3tuningspaces](https://github.com/mlr-org/mlr3tuningspaces) is a
  collection of search spaces from scientific articles for commonly used
  learners.

- [mlr3hyperband](https://github.com/mlr-org/mlr3hyperband) adds the
  Hyperband and Successive Halving algorithm.

- [mlr3mbo](https://github.com/mlr-org/mlr3mbo) adds Bayesian
  optimization methods.

## Analysis

For analyzing the tuning results, it is recommended to pass the
[ArchiveBatchTuning](https://mlr3tuning.mlr-org.com/dev/reference/ArchiveBatchTuning.md)
to
[`as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html).
The returned data table is joined with the benchmark result which adds
the
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
for each hyperparameter evaluation.

The archive provides various getters (e.g. `$learners()`) to ease the
access. All getters extract by position (`i`) or unique hash (`uhash`).
For a complete list of all getters see the methods section.

The benchmark result (`$benchmark_result`) allows to score the
hyperparameter configurations again on a different measure.
Alternatively, measures can be supplied to
[`as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html).

The [mlr3viz](https://CRAN.R-project.org/package=mlr3viz) package
provides visualizations for tuning results.

## Super classes

[`bbotk::OptimInstance`](https://bbotk.mlr-org.com/reference/OptimInstance.html)
-\>
[`bbotk::OptimInstanceBatch`](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html)
-\>
[`bbotk::OptimInstanceBatchSingleCrit`](https://bbotk.mlr-org.com/reference/OptimInstanceBatchSingleCrit.html)
-\> `TuningInstanceBatchSingleCrit`

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

- [`TuningInstanceBatchSingleCrit$new()`](#method-TuningInstanceBatchSingleCrit-new)

- [`TuningInstanceBatchSingleCrit$assign_result()`](#method-TuningInstanceBatchSingleCrit-assign_result)

- [`TuningInstanceBatchSingleCrit$clone()`](#method-TuningInstanceBatchSingleCrit-clone)

Inherited methods

- [`bbotk::OptimInstance$clear()`](https://bbotk.mlr-org.com/reference/OptimInstance.html#method-clear)
- [`bbotk::OptimInstance$format()`](https://bbotk.mlr-org.com/reference/OptimInstance.html#method-format)
- [`bbotk::OptimInstance$print()`](https://bbotk.mlr-org.com/reference/OptimInstance.html#method-print)
- [`bbotk::OptimInstanceBatch$eval_batch()`](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html#method-eval_batch)
- [`bbotk::OptimInstanceBatch$objective_function()`](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html#method-objective_function)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TuningInstanceBatchSingleCrit$new(
      task,
      learner,
      resampling,
      measure = NULL,
      terminator,
      search_space = NULL,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      check_values = FALSE,
      callbacks = NULL
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
  of the learner's parameter set (learner\$param_set). When using
  [`to_tune()`](https://paradox.mlr-org.com/reference/to_tune.html)
  tokens, dependencies for hierarchical search spaces are automatically
  handled.

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

------------------------------------------------------------------------

### Method `assign_result()`

The [Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md)
object writes the best found point and estimated performance value here.
For internal use.

#### Usage

    TuningInstanceBatchSingleCrit$assign_result(
      xdt,
      y,
      learner_param_vals = NULL,
      extra = NULL,
      ...
    )

#### Arguments

- `xdt`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Hyperparameter values as
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).
  Each row is one configuration. Contains values in the search space.
  Can contain additional columns for extra information.

- `y`:

  (`numeric(1)`)  
  Optimal outcome.

- `learner_param_vals`:

  (List of named `list()s`)  
  Fixed parameter values of the learner that are neither part of the
  search space nor the domain.

- `extra`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Additional information.

- `...`:

  (`any`)  
  ignored.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TuningInstanceBatchSingleCrit$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Hyperparameter optimization on the Palmer Penguins data set
task = tsk("penguins")

# Load learner and set search space
learner = lrn("classif.rpart",
  cp = to_tune(1e-04, 1e-1, logscale = TRUE)
)

# Construct tuning instance
instance = ti(
  task = task,
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measures = msr("classif.ce"),
  terminator = trm("evals", n_evals = 4)
)

# Choose optimization algorithm
tuner = tnr("random_search", batch_size = 2)

# Run tuning
tuner$optimize(instance)
#>           cp learner_param_vals  x_domain classif.ce
#>        <num>             <list>    <list>      <num>
#> 1: -7.255326          <list[2]> <list[1]> 0.05517417

# Set optimal hyperparameter configuration to learner
learner$param_set$values = instance$result_learner_param_vals

# Train the learner on the full data set
learner$train(task)

# Inspect all evaluated configurations
as.data.table(instance$archive)
#>           cp classif.ce runtime_learners           timestamp warnings errors
#>        <num>      <num>            <num>              <POSc>    <int>  <int>
#> 1: -7.255326 0.05517417            0.017 2026-02-19 14:51:19        0      0
#> 2: -6.314690 0.05517417            0.018 2026-02-19 14:51:19        0      0
#> 3: -8.991870 0.05517417            0.019 2026-02-19 14:51:19        0      0
#> 4: -4.755440 0.05517417            0.016 2026-02-19 14:51:19        0      0
#>     x_domain batch_nr  resample_result
#>       <list>    <int>           <list>
#> 1: <list[1]>        1 <ResampleResult>
#> 2: <list[1]>        1 <ResampleResult>
#> 3: <list[1]>        2 <ResampleResult>
#> 4: <list[1]>        2 <ResampleResult>
```
