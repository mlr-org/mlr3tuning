# Single Criterion Tuning Instance for Batch Tuning

`TuningInstanceSingleCrit` is a deprecated class that is now a wrapper
around
[TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md).

## Super classes

[`bbotk::OptimInstance`](https://bbotk.mlr-org.com/reference/OptimInstance.html)
-\>
[`bbotk::OptimInstanceBatch`](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html)
-\>
[`bbotk::OptimInstanceBatchSingleCrit`](https://bbotk.mlr-org.com/reference/OptimInstanceBatchSingleCrit.html)
-\>
[`mlr3tuning::TuningInstanceBatchSingleCrit`](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md)
-\> `TuningInstanceSingleCrit`

## Methods

### Public methods

- [`TuningInstanceSingleCrit$new()`](#method-TuningInstanceSingleCrit-new)

- [`TuningInstanceSingleCrit$clone()`](#method-TuningInstanceSingleCrit-clone)

Inherited methods

- [`bbotk::OptimInstance$clear()`](https://bbotk.mlr-org.com/reference/OptimInstance.html#method-clear)
- [`bbotk::OptimInstance$format()`](https://bbotk.mlr-org.com/reference/OptimInstance.html#method-format)
- [`bbotk::OptimInstance$print()`](https://bbotk.mlr-org.com/reference/OptimInstance.html#method-print)
- [`bbotk::OptimInstanceBatch$eval_batch()`](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html#method-eval_batch)
- [`bbotk::OptimInstanceBatch$objective_function()`](https://bbotk.mlr-org.com/reference/OptimInstanceBatch.html#method-objective_function)
- [`mlr3tuning::TuningInstanceBatchSingleCrit$assign_result()`](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.html#method-assign_result)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TuningInstanceSingleCrit$new(
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
  [Tuner](https://mlr3tuning.mlr-org.com/reference/Tuner.md) change the
  resampling e.g. to evaluate a hyperparameter configuration on
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

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TuningInstanceSingleCrit$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
