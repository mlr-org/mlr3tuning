# Class for Asynchronous Tuning Algorithms

The TunerAsync implements the asynchronous optimization algorithm.

## Details

TunerAsync is an abstract base class that implements the base
functionality each asynchronous tuner must provide.

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
-\> `TunerAsync`

## Methods

### Public methods

- [`TunerAsync$optimize()`](#method-TunerAsync-optimize)

- [`TunerAsync$clone()`](#method-TunerAsync-clone)

Inherited methods

- [`mlr3tuning::Tuner$format()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-format)
- [`mlr3tuning::Tuner$help()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-help)
- [`mlr3tuning::Tuner$initialize()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-initialize)
- [`mlr3tuning::Tuner$print()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-print)

------------------------------------------------------------------------

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

Performs the tuning on a
[TuningInstanceAsyncSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceAsyncSingleCrit.md)
or
[TuningInstanceAsyncMultiCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceAsyncMultiCrit.md)
until termination. The single evaluations will be written into the
[ArchiveAsyncTuning](https://mlr3tuning.mlr-org.com/reference/ArchiveAsyncTuning.md)
that resides in the
[TuningInstanceAsyncSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceAsyncSingleCrit.md)/[TuningInstanceAsyncMultiCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceAsyncMultiCrit.md).
The result will be written into the instance object.

#### Usage

    TunerAsync$optimize(inst)

#### Arguments

- `inst`:

  ([TuningInstanceAsyncSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceAsyncSingleCrit.md)
  \|
  [TuningInstanceAsyncMultiCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceAsyncMultiCrit.md)).

#### Returns

[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TunerAsync$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
