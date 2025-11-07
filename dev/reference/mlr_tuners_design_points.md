# Hyperparameter Tuning with Design Points

Subclass for tuning w.r.t. fixed design points.

We simply search over a set of points fully specified by the user. The
points in the design are evaluated in order as given.

## Dictionary

This [Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md) can
be instantiated with the associated sugar function
[`tnr()`](https://mlr3tuning.mlr-org.com/dev/reference/tnr.md):

    tnr("design_points")

## Parallelization

In order to support general termination criteria and parallelization, we
evaluate points in a batch-fashion of size `batch_size`. Larger batches
mean we can parallelize more, smaller batches imply a more fine-grained
checking of termination criteria. A batch contains of `batch_size` times
`resampling$iters` jobs. E.g., if you set a batch size of 10 points and
do a 5-fold cross validation, you can utilize up to 50 cores.

Parallelization is supported via package
[future](https://CRAN.R-project.org/package=future) (see
[`mlr3::benchmark()`](https://mlr3.mlr-org.com/reference/benchmark.html)'s
section on parallelization for more details).

## Logging

All [Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md)s use
a logger (as implemented in
[lgr](https://CRAN.R-project.org/package=lgr)) from package
[bbotk](https://CRAN.R-project.org/package=bbotk). Use
`lgr::get_logger("bbotk")` to access and control the logger.

## Optimizer

This [Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md) is
based on
[bbotk::OptimizerBatchDesignPoints](https://bbotk.mlr-org.com/reference/mlr_optimizers_design_points.html)
which can be applied on any black box optimization problem. See also the
documentation of [bbotk](https://bbotk.mlr-org.com/).

## Parameters

- `batch_size`:

  `integer(1)`  
  Maximum number of configurations to try in a batch.

- `design`:

  [data.table::data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html)  
  Design points to try in search, one per row.

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

## Progress Bars

`$optimize()` supports progress bars via the package
[progressr](https://CRAN.R-project.org/package=progressr) combined with
a [Terminator](https://bbotk.mlr-org.com/reference/Terminator.html).
Simply wrap the function in `progressr::with_progress()` to enable them.
We recommend to use package
[progress](https://CRAN.R-project.org/package=progress) as backend;
enable with `progressr::handlers("progress")`.

## See also

Package
[mlr3hyperband](https://CRAN.R-project.org/package=mlr3hyperband) for
hyperband tuning.

Other Tuner:
[`Tuner`](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md),
[`mlr_tuners`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners.md),
[`mlr_tuners_cmaes`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_cmaes.md),
[`mlr_tuners_gensa`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_gensa.md),
[`mlr_tuners_grid_search`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_grid_search.md),
[`mlr_tuners_internal`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_internal.md),
[`mlr_tuners_irace`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_irace.md),
[`mlr_tuners_nloptr`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_nloptr.md),
[`mlr_tuners_random_search`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_random_search.md)

## Super classes

[`mlr3tuning::Tuner`](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md)
-\>
[`mlr3tuning::TunerBatch`](https://mlr3tuning.mlr-org.com/dev/reference/TunerBatch.md)
-\>
[`mlr3tuning::TunerBatchFromOptimizerBatch`](https://mlr3tuning.mlr-org.com/dev/reference/TunerBatchFromOptimizerBatch.md)
-\> `TunerBatchDesignPoints`

## Methods

### Public methods

- [`TunerBatchDesignPoints$new()`](#method-TunerBatchDesignPoints-new)

- [`TunerBatchDesignPoints$clone()`](#method-TunerBatchDesignPoints-clone)

Inherited methods

- [`mlr3tuning::Tuner$format()`](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.html#method-format)
- [`mlr3tuning::Tuner$help()`](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.html#method-help)
- [`mlr3tuning::Tuner$print()`](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.html#method-print)
- [`mlr3tuning::TunerBatchFromOptimizerBatch$optimize()`](https://mlr3tuning.mlr-org.com/dev/reference/TunerBatchFromOptimizerBatch.html#method-optimize)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TunerBatchDesignPoints$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TunerBatchDesignPoints$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Hyperparameter Optimization

# load learner and set search space
learner = lrn("classif.rpart",
  cp = to_tune(1e-04, 1e-1),
  minsplit = to_tune(2, 128),
  minbucket = to_tune(1, 64)
)

# create design
design = mlr3misc::rowwise_table(
  ~cp,   ~minsplit,  ~minbucket,
  0.1,   2,          64,
  0.01,  64,         32,
  0.001, 128,        1
)

# run hyperparameter tuning on the Palmer Penguins data set
instance = tune(
  tuner = tnr("design_points", design = design),
  task = tsk("penguins"),
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce")
)

# best performing hyperparameter configuration
instance$result
#>       cp minbucket minsplit learner_param_vals  x_domain classif.ce
#>    <num>     <num>    <num>             <list>    <list>      <num>
#> 1:  0.01        32       64          <list[4]> <list[3]> 0.06086957

# all evaluated hyperparameter configuration
as.data.table(instance$archive)
#>       cp minbucket minsplit classif.ce runtime_learners           timestamp
#>    <num>     <num>    <num>      <num>            <num>              <POSc>
#> 1: 0.100        64        2 0.09565217            0.005 2025-11-07 11:34:04
#> 2: 0.010        32       64 0.06086957            0.008 2025-11-07 11:34:04
#> 3: 0.001         1      128 0.06086957            0.006 2025-11-07 11:34:04
#>    warnings errors  x_domain batch_nr  resample_result
#>       <int>  <int>    <list>    <int>           <list>
#> 1:        0      0 <list[3]>        1 <ResampleResult>
#> 2:        0      0 <list[3]>        2 <ResampleResult>
#> 3:        0      0 <list[3]>        3 <ResampleResult>

# fit final model on complete data set
learner$param_set$values = instance$result_learner_param_vals
learner$train(tsk("penguins"))
```
