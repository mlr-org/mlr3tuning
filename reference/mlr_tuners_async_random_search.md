# Hyperparameter Tuning with Asynchronous Random Search

Subclass for asynchronous random search tuning.

## Source

Bergstra J, Bengio Y (2012). “Random Search for Hyper-Parameter
Optimization.” *Journal of Machine Learning Research*, **13**(10),
281–305. <https://jmlr.csail.mit.edu/papers/v13/bergstra12a.html>.

## Details

The random points are sampled by
[`paradox::generate_design_random()`](https://paradox.mlr-org.com/reference/generate_design_random.html).

## Dictionary

This [Tuner](https://mlr3tuning.mlr-org.com/reference/Tuner.md) can be
instantiated with the associated sugar function
[`tnr()`](https://mlr3tuning.mlr-org.com/reference/tnr.md):

    tnr("async_random_search")

## See also

Other TunerAsync:
[`mlr_tuners_async_design_points`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_async_design_points.md),
[`mlr_tuners_async_grid_search`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_async_grid_search.md)

## Super classes

[`mlr3tuning::Tuner`](https://mlr3tuning.mlr-org.com/reference/Tuner.md)
-\>
[`mlr3tuning::TunerAsync`](https://mlr3tuning.mlr-org.com/reference/TunerAsync.md)
-\>
[`mlr3tuning::TunerAsyncFromOptimizerAsync`](https://mlr3tuning.mlr-org.com/reference/TunerAsyncFromOptimizerAsync.md)
-\> `TunerAsyncRandomSearch`

## Methods

### Public methods

- [`TunerAsyncRandomSearch$new()`](#method-TunerAsyncRandomSearch-new)

- [`TunerAsyncRandomSearch$clone()`](#method-TunerAsyncRandomSearch-clone)

Inherited methods

- [`mlr3tuning::Tuner$format()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-format)
- [`mlr3tuning::Tuner$help()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-help)
- [`mlr3tuning::Tuner$print()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-print)
- [`mlr3tuning::TunerAsyncFromOptimizerAsync$optimize()`](https://mlr3tuning.mlr-org.com/reference/TunerAsyncFromOptimizerAsync.html#method-optimize)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TunerAsyncRandomSearch$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TunerAsyncRandomSearch$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
