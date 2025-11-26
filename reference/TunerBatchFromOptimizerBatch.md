# TunerBatchFromOptimizerBatch

Internally used to transform
[bbotk::Optimizer](https://bbotk.mlr-org.com/reference/Optimizer.html)
to [Tuner](https://mlr3tuning.mlr-org.com/reference/Tuner.md).

## Super classes

[`mlr3tuning::Tuner`](https://mlr3tuning.mlr-org.com/reference/Tuner.md)
-\>
[`mlr3tuning::TunerBatch`](https://mlr3tuning.mlr-org.com/reference/TunerBatch.md)
-\> `TunerBatchFromOptimizerBatch`

## Active bindings

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of control parameters.

## Methods

### Public methods

- [`TunerBatchFromOptimizerBatch$new()`](#method-TunerBatchFromOptimizerBatch-new)

- [`TunerBatchFromOptimizerBatch$optimize()`](#method-TunerBatchFromOptimizerBatch-optimize)

- [`TunerBatchFromOptimizerBatch$clone()`](#method-TunerBatchFromOptimizerBatch-clone)

Inherited methods

- [`mlr3tuning::Tuner$format()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-format)
- [`mlr3tuning::Tuner$help()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-help)
- [`mlr3tuning::Tuner$print()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TunerBatchFromOptimizerBatch$new(optimizer, man = NA_character_)

#### Arguments

- `optimizer`:

  [bbotk::Optimizer](https://bbotk.mlr-org.com/reference/Optimizer.html)  
  Optimizer that is called.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. The referenced help package can be opened via method
  `$help()`.

------------------------------------------------------------------------

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

Performs the tuning on a
[TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md)
/
[TuningInstanceBatchMultiCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchMultiCrit.md)
until termination. The single evaluations and the final results will be
written into the
[ArchiveBatchTuning](https://mlr3tuning.mlr-org.com/reference/ArchiveBatchTuning.md)
that resides in the
[TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md)/[TuningInstanceBatchMultiCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchMultiCrit.md).
The final result is returned.

#### Usage

    TunerBatchFromOptimizerBatch$optimize(inst)

#### Arguments

- `inst`:

  ([TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md)
  \|
  [TuningInstanceBatchMultiCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchMultiCrit.md)).

#### Returns

[data.table::data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TunerBatchFromOptimizerBatch$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
