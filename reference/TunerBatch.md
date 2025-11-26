# Class for Batch Tuning Algorithms

The TunerBatch implements the optimization algorithm.

## Details

TunerBatch is an abstract base class that implements the base
functionality each tuner must provide. A subclass is implemented in the
following way:

- Inherit from Tuner.

- Specify the private abstract method `$.optimize()` and use it to call
  into your optimizer.

- You need to call `instance$eval_batch()` to evaluate design points.

- The batch evaluation is requested at the
  [TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md)/[TuningInstanceBatchMultiCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchMultiCrit.md)
  object `instance`, so each batch is possibly executed in parallel via
  [`mlr3::benchmark()`](https://mlr3.mlr-org.com/reference/benchmark.html),
  and all evaluations are stored inside of `instance$archive`.

- Before the batch evaluation, the
  [bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html)
  is checked, and if it is positive, an exception of class
  `"terminated_error"` is generated. In the later case the current batch
  of evaluations is still stored in `instance`, but the numeric scores
  are not sent back to the handling optimizer as it has lost execution
  control.

- After such an exception was caught we select the best configuration
  from `instance$archive` and return it.

- Note that therefore more points than specified by the
  [bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html)
  may be evaluated, as the Terminator is only checked before a batch
  evaluation, and not in-between evaluation in a batch. How many more
  depends on the setting of the batch size.

- Overwrite the private super-method `.assign_result()` if you want to
  decide yourself how to estimate the final configuration in the
  instance and its estimated performance. The default behavior is: We
  pick the best resample-experiment, regarding the given measure, then
  assign its configuration and aggregated performance to the instance.

## Private Methods

- `.optimize(instance)` -\> `NULL`  
  Abstract base method. Implement to specify tuning of your subclass.
  See details sections.

- `.assign_result(instance)` -\> `NULL`  
  Abstract base method. Implement to specify how the final configuration
  is selected. See details sections.

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

[`mlr3tuning::Tuner`](https://mlr3tuning.mlr-org.com/reference/Tuner.md)
-\> `TunerBatch`

## Methods

### Public methods

- [`TunerBatch$new()`](#method-TunerBatch-new)

- [`TunerBatch$optimize()`](#method-TunerBatch-optimize)

- [`TunerBatch$clone()`](#method-TunerBatch-clone)

Inherited methods

- [`mlr3tuning::Tuner$format()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-format)
- [`mlr3tuning::Tuner$help()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-help)
- [`mlr3tuning::Tuner$print()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TunerBatch$new(
      id = "tuner_batch",
      param_set,
      param_classes,
      properties,
      packages = character(),
      label = NA_character_,
      man = NA_character_
    )

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of control parameters.

- `param_classes`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Supported parameter classes for learner hyperparameters that the tuner
  can optimize, as given in the
  [paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
  `$class` field.

- `properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of properties of the tuner. Must be a subset of
  [`mlr_reflections$tuner_properties`](https://mlr3.mlr-org.com/reference/mlr_reflections.html).

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of required packages. Note that these packages will be loaded via
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html), and are
  not attached.

- `label`:

  (`character(1)`)  
  Label for this object. Can be used in tables, plot and text output
  instead of the ID.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. The referenced help package can be opened via method
  `$help()`.

------------------------------------------------------------------------

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

Performs the tuning on a
[TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md)
or
[TuningInstanceBatchMultiCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchMultiCrit.md)
until termination. The single evaluations will be written into the
[ArchiveBatchTuning](https://mlr3tuning.mlr-org.com/reference/ArchiveBatchTuning.md)
that resides in the
[TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md)/[TuningInstanceBatchMultiCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchMultiCrit.md).
The result will be written into the instance object.

#### Usage

    TunerBatch$optimize(inst)

#### Arguments

- `inst`:

  ([TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md)
  \|
  [TuningInstanceBatchMultiCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchMultiCrit.md)).

#### Returns

[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TunerBatch$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
