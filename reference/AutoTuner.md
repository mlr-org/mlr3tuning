# Class for Automatic Tuning

The AutoTuner wraps a
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html) and
augments it with an automatic tuning process for a given set of
hyperparameters. The
[`auto_tuner()`](https://mlr3tuning.mlr-org.com/reference/auto_tuner.md)
function creates an AutoTuner object.

## Details

The AutoTuner is a
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html) which
wraps another
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html) and
performs the following steps during `$train()`:

1.  The hyperparameters of the wrapped (inner) learner are trained on
    the training data via resampling. The tuning can be specified by
    providing a
    [Tuner](https://mlr3tuning.mlr-org.com/reference/Tuner.md), a
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

## Validation

The `AutoTuner` itself does **not** have the `"validation"` property. To
enable validation during the tuning, set the `$validate` field of the
tuned learner. This is also possible via
[`set_validate()`](https://mlr3.mlr-org.com/reference/mlr_sugar.html).

## Nested Resampling

Nested resampling is performed by passing an AutoTuner to
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
passed to the AutoTuner is meant to be the inner resampling, operating
on the training set of an arbitrary outer resampling. For this reason,
the inner resampling should be not instantiated. If an instantiated
resampling is passed, the AutoTuner fails when a row id of the inner
resampling is not present in the training set of the outer resampling.

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

## Super class

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
`AutoTuner`

## Public fields

- `instance_args`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  All arguments from construction to create the
  [TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md).

- `tuner`:

  ([Tuner](https://mlr3tuning.mlr-org.com/reference/Tuner.md))  
  Optimization algorithm.

## Active bindings

- `archive`:

  [ArchiveBatchTuning](https://mlr3tuning.mlr-org.com/reference/ArchiveBatchTuning.md)  
  Archive of the
  [TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md).

- `learner`:

  ([mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html))  
  Trained learner

- `tuning_instance`:

  ([TuningInstanceAsyncSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceAsyncSingleCrit.md)
  \|
  [TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md))  
  Internally created tuning instance with all intermediate results.

- `tuning_result`:

  ([data.table::data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  Short-cut to `result` from tuning instance.

- `predict_type`:

  (`character(1)`)  
  Stores the currently active predict type, e.g. `"response"`. Must be
  an element of `$predict_types`. A few learners already use the predict
  type during training. So there is no guarantee that changing the
  predict type after tuning and training will have any effect or does
  not lead to errors.

- `hash`:

  (`character(1)`)  
  Hash (unique identifier) for this object.

- `phash`:

  (`character(1)`)  
  Hash (unique identifier) for this partial object, excluding some
  components which are varied systematically during tuning (parameter
  values) or feature selection (feature names).

## Methods

### Public methods

- [`AutoTuner$new()`](#method-AutoTuner-new)

- [`AutoTuner$base_learner()`](#method-AutoTuner-base_learner)

- [`AutoTuner$importance()`](#method-AutoTuner-importance)

- [`AutoTuner$selected_features()`](#method-AutoTuner-selected_features)

- [`AutoTuner$oob_error()`](#method-AutoTuner-oob_error)

- [`AutoTuner$loglik()`](#method-AutoTuner-loglik)

- [`AutoTuner$print()`](#method-AutoTuner-print)

- [`AutoTuner$marshal()`](#method-AutoTuner-marshal)

- [`AutoTuner$unmarshal()`](#method-AutoTuner-unmarshal)

- [`AutoTuner$marshaled()`](#method-AutoTuner-marshaled)

- [`AutoTuner$clone()`](#method-AutoTuner-clone)

Inherited methods

- [`mlr3::Learner$configure()`](https://mlr3.mlr-org.com/reference/Learner.html#method-configure)
- [`mlr3::Learner$encapsulate()`](https://mlr3.mlr-org.com/reference/Learner.html#method-encapsulate)
- [`mlr3::Learner$format()`](https://mlr3.mlr-org.com/reference/Learner.html#method-format)
- [`mlr3::Learner$help()`](https://mlr3.mlr-org.com/reference/Learner.html#method-help)
- [`mlr3::Learner$predict()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict)
- [`mlr3::Learner$predict_newdata()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict_newdata)
- [`mlr3::Learner$reset()`](https://mlr3.mlr-org.com/reference/Learner.html#method-reset)
- [`mlr3::Learner$train()`](https://mlr3.mlr-org.com/reference/Learner.html#method-train)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    AutoTuner$new(
      tuner,
      learner,
      resampling,
      measure = NULL,
      terminator,
      search_space = NULL,
      store_tuning_instance = TRUE,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      check_values = FALSE,
      callbacks = NULL,
      rush = NULL,
      id = NULL
    )

#### Arguments

- `tuner`:

  ([Tuner](https://mlr3tuning.mlr-org.com/reference/Tuner.md))  
  Optimization algorithm.

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

- `store_tuning_instance`:

  (`logical(1)`)  
  If `TRUE` (default), stores the internally created
  [TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md)
  with all intermediate results in slot `$tuning_instance`.

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

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

------------------------------------------------------------------------

### Method `base_learner()`

Extracts the base learner from nested learner objects like
`GraphLearner` in
[mlr3pipelines](https://CRAN.R-project.org/package=mlr3pipelines). If
`recursive = 0`, the (tuned) learner is returned.

#### Usage

    AutoTuner$base_learner(recursive = Inf)

#### Arguments

- `recursive`:

  (`integer(1)`)  
  Depth of recursion for multiple nested objects.

#### Returns

[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html).

------------------------------------------------------------------------

### Method `importance()`

The importance scores of the final model.

#### Usage

    AutoTuner$importance()

#### Returns

Named [`numeric()`](https://rdrr.io/r/base/numeric.html).

------------------------------------------------------------------------

### Method `selected_features()`

The selected features of the final model.

#### Usage

    AutoTuner$selected_features()

#### Returns

[`character()`](https://rdrr.io/r/base/character.html).

------------------------------------------------------------------------

### Method `oob_error()`

The out-of-bag error of the final model.

#### Usage

    AutoTuner$oob_error()

#### Returns

`numeric(1)`.

------------------------------------------------------------------------

### Method `loglik()`

The log-likelihood of the final model.

#### Usage

    AutoTuner$loglik()

#### Returns

`logLik`. Printer.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

#### Usage

    AutoTuner$print()

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method `marshal()`

Marshal the learner.

#### Usage

    AutoTuner$marshal(...)

#### Arguments

- `...`:

  (any)  
  Additional parameters.

#### Returns

self

------------------------------------------------------------------------

### Method `unmarshal()`

Unmarshal the learner.

#### Usage

    AutoTuner$unmarshal(...)

#### Arguments

- `...`:

  (any)  
  Additional parameters.

#### Returns

self

------------------------------------------------------------------------

### Method `marshaled()`

Whether the learner is marshaled.

#### Usage

    AutoTuner$marshaled()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AutoTuner$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Automatic Tuning

# split to train and external set
task = tsk("penguins")
split = partition(task, ratio = 0.8)

# load learner and set search space
learner = lrn("classif.rpart",
  cp = to_tune(1e-04, 1e-1, logscale = TRUE)
)

# create auto tuner
at = auto_tuner(
  tuner = tnr("random_search"),
  learner = learner,
  resampling = rsmp ("holdout"),
  measure = msr("classif.ce"),
  term_evals = 4)

# tune hyperparameters and fit final model
at$train(task, row_ids = split$train)

# predict with final model
at$predict(task, row_ids = split$test)
#> 
#> ── <PredictionClassif> for 69 observations: ────────────────────────────────────
#>  row_ids     truth  response
#>        1    Adelie    Adelie
#>        2    Adelie    Adelie
#>        9    Adelie    Adelie
#>      ---       ---       ---
#>      319 Chinstrap Chinstrap
#>      337 Chinstrap    Gentoo
#>      339 Chinstrap Chinstrap

# show tuning result
at$tuning_result
#>          cp learner_param_vals  x_domain classif.ce
#>       <num>             <list>    <list>      <num>
#> 1: -2.52958          <list[2]> <list[1]> 0.05434783

# model slot contains trained learner and tuning instance
at$model
#> $learner
#> 
#> ── <LearnerClassifRpart> (classif.rpart): Classification Tree ──────────────────
#> • Model: rpart
#> • Parameters: cp=0.07969, xval=0
#> • Packages: mlr3 and rpart
#> • Predict Types: [response] and prob
#> • Feature Types: logical, integer, numeric, factor, and ordered
#> • Encapsulation: none (fallback: -)
#> • Properties: importance, missings, multiclass, selected_features, twoclass,
#> and weights
#> • Other settings: use_weights = 'use'
#> 
#> $tuning_instance
#> 
#> ── <TuningInstanceBatchSingleCrit> ─────────────────────────────────────────────
#> • State: Optimized
#> • Objective: <ObjectiveTuningBatch>
#> • Search Space:
#>        id    class    lower     upper nlevels
#>    <char>   <char>    <num>     <num>   <num>
#> 1:     cp ParamDbl -9.21034 -2.302585     Inf
#> • Terminator: <TerminatorEvals> (n_evals=4, k=0)
#> • Result:
#>          cp classif.ce
#>       <num>      <num>
#> 1: -2.52958 0.05434783
#> • Archive:
#>    classif.ce    cp
#>         <num> <num>
#> 1:       0.05    -3
#> 2:       0.05    -5
#> 3:       0.05    -2
#> 4:       0.05    -5
#> 
#> attr(,"class")
#> [1] "auto_tuner_model" "list"            

# shortcut trained learner
at$learner
#> 
#> ── <LearnerClassifRpart> (classif.rpart): Classification Tree ──────────────────
#> • Model: rpart
#> • Parameters: cp=0.07969, xval=0
#> • Packages: mlr3 and rpart
#> • Predict Types: [response] and prob
#> • Feature Types: logical, integer, numeric, factor, and ordered
#> • Encapsulation: none (fallback: -)
#> • Properties: importance, missings, multiclass, selected_features, twoclass,
#> and weights
#> • Other settings: use_weights = 'use'

# shortcut tuning instance
at$tuning_instance
#> 
#> ── <TuningInstanceBatchSingleCrit> ─────────────────────────────────────────────
#> • State: Optimized
#> • Objective: <ObjectiveTuningBatch>
#> • Search Space:
#>        id    class    lower     upper nlevels
#>    <char>   <char>    <num>     <num>   <num>
#> 1:     cp ParamDbl -9.21034 -2.302585     Inf
#> • Terminator: <TerminatorEvals> (n_evals=4, k=0)
#> • Result:
#>          cp classif.ce
#>       <num>      <num>
#> 1: -2.52958 0.05434783
#> • Archive:
#>    classif.ce    cp
#>         <num> <num>
#> 1:       0.05    -3
#> 2:       0.05    -5
#> 3:       0.05    -2
#> 4:       0.05    -5


# Nested Resampling

at = auto_tuner(
  tuner = tnr("random_search"),
  learner = learner,
  resampling = rsmp ("holdout"),
  measure = msr("classif.ce"),
  term_evals = 4)

resampling_outer = rsmp("cv", folds = 3)
rr = resample(task, at, resampling_outer, store_models = TRUE)

# retrieve inner tuning results.
extract_inner_tuning_results(rr)
#>    iteration        cp classif.ce learner_param_vals  x_domain  task_id
#>        <int>     <num>      <num>             <list>    <list>   <char>
#> 1:         1 -2.421343 0.06578947          <list[2]> <list[1]> penguins
#> 2:         2 -7.917442 0.06493506          <list[2]> <list[1]> penguins
#> 3:         3 -8.698664 0.03947368          <list[2]> <list[1]> penguins
#>             learner_id resampling_id
#>                 <char>        <char>
#> 1: classif.rpart.tuned            cv
#> 2: classif.rpart.tuned            cv
#> 3: classif.rpart.tuned            cv

# performance scores estimated on the outer resampling
rr$score()
#>     task_id          learner_id resampling_id iteration classif.ce
#>      <char>              <char>        <char>     <int>      <num>
#> 1: penguins classif.rpart.tuned            cv         1 0.06956522
#> 2: penguins classif.rpart.tuned            cv         2 0.06956522
#> 3: penguins classif.rpart.tuned            cv         3 0.01754386
#> Hidden columns: task, learner, resampling, prediction_test

# unbiased performance of the final model trained on the full data set
rr$aggregate()
#> classif.ce 
#> 0.05222476 
```
