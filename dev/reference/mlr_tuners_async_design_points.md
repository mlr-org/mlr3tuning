# Hyperparameter Tuning with Asynchronous Design Points

Subclass for asynchronous design points tuning.

## Dictionary

This [Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md) can
be instantiated with the associated sugar function
[`tnr()`](https://mlr3tuning.mlr-org.com/dev/reference/tnr.md):

    tnr("async_design_points")

## Parameters

- `design`:

  [data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)  
  Design points to try in search, one per row.

## See also

Other TunerAsync:
[`mlr_tuners_async_grid_search`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_async_grid_search.md),
[`mlr_tuners_async_random_search`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_async_random_search.md)

## Super classes

[`Tuner`](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md) -\>
[`TunerAsync`](https://mlr3tuning.mlr-org.com/dev/reference/TunerAsync.md)
-\>
[`TunerAsyncFromOptimizerAsync`](https://mlr3tuning.mlr-org.com/dev/reference/TunerAsyncFromOptimizerAsync.md)
-\> `TunerAsyncDesignPoints`

## Methods

### Public methods

- [`TunerAsyncDesignPoints$new()`](#method-TunerAsyncDesignPoints-initialize)

- [`TunerAsyncDesignPoints$clone()`](#method-TunerAsyncDesignPoints-clone)

Inherited methods

- [`Tuner$format()`](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.html#method-format)
- [`Tuner$help()`](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.html#method-help)
- [`Tuner$print()`](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.html#method-print)
- [`TunerAsyncFromOptimizerAsync$optimize()`](https://mlr3tuning.mlr-org.com/dev/reference/TunerAsyncFromOptimizerAsync.html#method-optimize)

------------------------------------------------------------------------

### `TunerAsyncDesignPoints$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TunerAsyncDesignPoints$new()

------------------------------------------------------------------------

### `TunerAsyncDesignPoints$clone()`

The objects of this class are cloneable with this method.

#### Usage

    TunerAsyncDesignPoints$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
