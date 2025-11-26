# Class for Tuning Objective

Stores the objective function that estimates the performance of
hyperparameter configurations. This class is usually constructed
internally by the
[TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md)
or
[TuningInstanceBatchMultiCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchMultiCrit.md).

## Super class

[`bbotk::Objective`](https://bbotk.mlr-org.com/reference/Objective.html)
-\> `ObjectiveTuning`

## Public fields

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)).

- `learner`:

  ([mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html)).

- `resampling`:

  ([mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html)).

- `measures`:

  (list of
  [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html)).

- `store_models`:

  (`logical(1)`).

- `store_benchmark_result`:

  (`logical(1)`).

- `callbacks`:

  (List of
  [mlr3misc::Callback](https://mlr3misc.mlr-org.com/reference/Callback.html)).

- `default_values`:

  (named [`list()`](https://rdrr.io/r/base/list.html)).

- `internal_search_space`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)).
  Internal search space for internal tuning.

## Methods

### Public methods

- [`ObjectiveTuning$new()`](#method-ObjectiveTuning-new)

- [`ObjectiveTuning$clone()`](#method-ObjectiveTuning-clone)

Inherited methods

- [`bbotk::Objective$eval()`](https://bbotk.mlr-org.com/reference/Objective.html#method-eval)
- [`bbotk::Objective$eval_dt()`](https://bbotk.mlr-org.com/reference/Objective.html#method-eval_dt)
- [`bbotk::Objective$eval_many()`](https://bbotk.mlr-org.com/reference/Objective.html#method-eval_many)
- [`bbotk::Objective$format()`](https://bbotk.mlr-org.com/reference/Objective.html#method-format)
- [`bbotk::Objective$help()`](https://bbotk.mlr-org.com/reference/Objective.html#method-help)
- [`bbotk::Objective$print()`](https://bbotk.mlr-org.com/reference/Objective.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    ObjectiveTuning$new(
      task,
      learner,
      resampling,
      measures,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      check_values = FALSE,
      callbacks = NULL,
      internal_search_space = NULL
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

- `measures`:

  (list of
  [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
  Measures to optimize.

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

- `internal_search_space`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
  or `NULL`)  
  The internal search space.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ObjectiveTuning$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
