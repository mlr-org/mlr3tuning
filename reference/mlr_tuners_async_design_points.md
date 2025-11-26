# Hyperparameter Tuning with Asynchronous Design Points

Subclass for asynchronous design points tuning.

## Dictionary

This [Tuner](https://mlr3tuning.mlr-org.com/reference/Tuner.md) can be
instantiated with the associated sugar function
[`tnr()`](https://mlr3tuning.mlr-org.com/reference/tnr.md):

    tnr("async_design_points")

## Parameters

- `design`:

  [data.table::data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html)  
  Design points to try in search, one per row.

## See also

Other TunerAsync:
[`mlr_tuners_async_grid_search`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_async_grid_search.md),
[`mlr_tuners_async_random_search`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_async_random_search.md)

## Super classes

[`mlr3tuning::Tuner`](https://mlr3tuning.mlr-org.com/reference/Tuner.md)
-\>
[`mlr3tuning::TunerAsync`](https://mlr3tuning.mlr-org.com/reference/TunerAsync.md)
-\>
[`mlr3tuning::TunerAsyncFromOptimizerAsync`](https://mlr3tuning.mlr-org.com/reference/TunerAsyncFromOptimizerAsync.md)
-\> `TunerAsyncDesignPoints`

## Methods

### Public methods

- [`TunerAsyncDesignPoints$new()`](#method-TunerAsyncDesignPoints-new)

- [`TunerAsyncDesignPoints$clone()`](#method-TunerAsyncDesignPoints-clone)

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

    TunerAsyncDesignPoints$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TunerAsyncDesignPoints$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
