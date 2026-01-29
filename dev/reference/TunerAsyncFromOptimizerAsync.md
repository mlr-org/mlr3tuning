# TunerAsyncFromOptimizerAsync

Internally used to transform
[bbotk::Optimizer](https://bbotk.mlr-org.com/reference/Optimizer.html)
to [Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md).

## Super classes

[`mlr3tuning::Tuner`](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md)
-\>
[`mlr3tuning::TunerAsync`](https://mlr3tuning.mlr-org.com/dev/reference/TunerAsync.md)
-\> `TunerAsyncFromOptimizerAsync`

## Active bindings

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of control parameters.

## Methods

### Public methods

- [`TunerAsyncFromOptimizerAsync$new()`](#method-TunerAsyncFromOptimizerAsync-new)

- [`TunerAsyncFromOptimizerAsync$optimize()`](#method-TunerAsyncFromOptimizerAsync-optimize)

- [`TunerAsyncFromOptimizerAsync$clone()`](#method-TunerAsyncFromOptimizerAsync-clone)

Inherited methods

- [`mlr3tuning::Tuner$format()`](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.html#method-format)
- [`mlr3tuning::Tuner$help()`](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.html#method-help)
- [`mlr3tuning::Tuner$print()`](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TunerAsyncFromOptimizerAsync$new(optimizer, man = NA_character_)

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
[TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/dev/reference/TuningInstanceBatchSingleCrit.md)
/
[TuningInstanceBatchMultiCrit](https://mlr3tuning.mlr-org.com/dev/reference/TuningInstanceBatchMultiCrit.md)
until termination. The single evaluations and the final results will be
written into the
[ArchiveAsyncTuning](https://mlr3tuning.mlr-org.com/dev/reference/ArchiveAsyncTuning.md)
that resides in the
[TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/dev/reference/TuningInstanceBatchSingleCrit.md)/[TuningInstanceBatchMultiCrit](https://mlr3tuning.mlr-org.com/dev/reference/TuningInstanceBatchMultiCrit.md).
The final result is returned.

#### Usage

    TunerAsyncFromOptimizerAsync$optimize(inst)

#### Arguments

- `inst`:

  ([TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/dev/reference/TuningInstanceBatchSingleCrit.md)
  \|
  [TuningInstanceBatchMultiCrit](https://mlr3tuning.mlr-org.com/dev/reference/TuningInstanceBatchMultiCrit.md)).

#### Returns

[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TunerAsyncFromOptimizerAsync$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
