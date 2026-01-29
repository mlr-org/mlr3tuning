# Function for Tuning a Learner

Function to tune a
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html). The
function internally creates a
[TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/dev/reference/TuningInstanceBatchSingleCrit.md)
or
[TuningInstanceBatchMultiCrit](https://mlr3tuning.mlr-org.com/dev/reference/TuningInstanceBatchMultiCrit.md)
which describes the tuning problem. It executes the tuning with the
[Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md) (`tuner`)
and returns the result with the tuning instance (`$result`). The
[ArchiveBatchTuning](https://mlr3tuning.mlr-org.com/dev/reference/ArchiveBatchTuning.md)
and
[ArchiveAsyncTuning](https://mlr3tuning.mlr-org.com/dev/reference/ArchiveAsyncTuning.md)
(`$archive`) stores all evaluated hyperparameter configurations and
performance scores.

You can find an overview of all tuners on our
[website](https://mlr-org.com/tuners.html).

## Usage

``` r
tune(
  tuner,
  task,
  learner,
  resampling,
  measures = NULL,
  term_evals = NULL,
  term_time = NULL,
  terminator = NULL,
  search_space = NULL,
  store_benchmark_result = TRUE,
  store_models = FALSE,
  check_values = FALSE,
  callbacks = NULL,
  rush = NULL
)
```

## Arguments

- tuner:

  ([Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md))  
  Optimization algorithm.

- task:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html))  
  Task to operate on.

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

- measures:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html) or
  list of
  [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
  A single measure creates a
  [TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/dev/reference/TuningInstanceBatchSingleCrit.md)
  and multiple measures a
  [TuningInstanceBatchMultiCrit](https://mlr3tuning.mlr-org.com/dev/reference/TuningInstanceBatchMultiCrit.md).
  If `NULL`, default measure is used.

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

## Value

[TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/dev/reference/TuningInstanceBatchSingleCrit.md)
\|
[TuningInstanceBatchMultiCrit](https://mlr3tuning.mlr-org.com/dev/reference/TuningInstanceBatchMultiCrit.md)

## Details

The [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html),
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html),
[mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html),
[mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html) and
[bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html)
are used to construct a
[TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/dev/reference/TuningInstanceBatchSingleCrit.md).
If multiple performance
[mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html)s are
supplied, a
[TuningInstanceBatchMultiCrit](https://mlr3tuning.mlr-org.com/dev/reference/TuningInstanceBatchMultiCrit.md)
is created. The parameter `term_evals` and `term_time` are shortcuts to
create a
[bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html).
If both parameters are passed, a
[bbotk::TerminatorCombo](https://bbotk.mlr-org.com/reference/mlr_terminators_combo.html)
is constructed. For other
[Terminators](https://bbotk.mlr-org.com/reference/Terminator.html), pass
one with `terminator`. If no termination criterion is needed, set
`term_evals`, `term_time` and `terminator` to `NULL`. The search space
is created from
[paradox::TuneToken](https://paradox.mlr-org.com/reference/to_tune.html)
or is supplied by `search_space`.

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

## Examples

``` r
# Hyperparameter optimization on the Palmer Penguins data set
task = tsk("pima")

# Load learner and set search space
learner = lrn("classif.rpart",
  cp = to_tune(1e-04, 1e-1, logscale = TRUE)
)

# Run tuning
instance = tune(
  tuner = tnr("random_search", batch_size = 2),
  task = tsk("pima"),
  learner = learner,
  resampling = rsmp ("holdout"),
  measures = msr("classif.ce"),
  terminator = trm("evals", n_evals = 4)
)

# Set optimal hyperparameter configuration to learner
learner$param_set$values = instance$result_learner_param_vals

# Train the learner on the full data set
learner$train(task)

# Inspect all evaluated configurations
as.data.table(instance$archive)
#>           cp classif.ce runtime_learners           timestamp warnings errors
#>        <num>      <num>            <num>              <POSc>    <int>  <int>
#> 1: -8.458173  0.2890625            0.026 2026-01-29 12:41:43        0      0
#> 2: -8.809285  0.2890625            0.008 2026-01-29 12:41:43        0      0
#> 3: -6.984893  0.2890625            0.008 2026-01-29 12:41:43        0      0
#> 4: -2.654367  0.2304688            0.007 2026-01-29 12:41:43        0      0
#>     x_domain batch_nr  resample_result
#>       <list>    <int>           <list>
#> 1: <list[1]>        1 <ResampleResult>
#> 2: <list[1]>        1 <ResampleResult>
#> 3: <list[1]>        2 <ResampleResult>
#> 4: <list[1]>        2 <ResampleResult>
```
