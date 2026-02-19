# Hyperparameter Tuning with Grid Search

Subclass for grid search tuning.

## Details

The grid is constructed as a Cartesian product over discretized values
per parameter, see
[`paradox::generate_design_grid()`](https://paradox.mlr-org.com/reference/generate_design_grid.html).
If the learner supports hotstarting, the grid is sorted by the hotstart
parameter (see also
[mlr3::HotstartStack](https://mlr3.mlr-org.com/reference/HotstartStack.html)).
If not, the points of the grid are evaluated in a random order.

## Dictionary

This [Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md) can
be instantiated with the associated sugar function
[`tnr()`](https://mlr3tuning.mlr-org.com/dev/reference/tnr.md):

    tnr("grid_search")

## Control Parameters

- `resolution`:

  `integer(1)`  
  Resolution of the grid, see
  [`paradox::generate_design_grid()`](https://paradox.mlr-org.com/reference/generate_design_grid.html).

- `param_resolutions`:

  named [`integer()`](https://rdrr.io/r/base/integer.html)  
  Resolution per parameter, named by parameter ID, see
  [`paradox::generate_design_grid()`](https://paradox.mlr-org.com/reference/generate_design_grid.html).

- `batch_size`:

  `integer(1)`  
  Maximum number of points to try in a batch.

## Progress Bars

`$optimize()` supports progress bars via the package
[progressr](https://CRAN.R-project.org/package=progressr) combined with
a
[bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html).
Simply wrap the function in `progressr::with_progress()` to enable them.
We recommend to use package
[progress](https://CRAN.R-project.org/package=progress) as backend;
enable with `progressr::handlers("progress")`.

## Parallelization

In order to support general termination criteria and parallelization, we
evaluate points in a batch-fashion of size `batch_size`. Larger batches
mean we can parallelize more, smaller batches imply a more fine-grained
checking of termination criteria. A batch consists of `batch_size` times
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
[bbotk::OptimizerBatchGridSearch](https://bbotk.mlr-org.com/reference/mlr_optimizers_grid_search.html)
which can be applied on any black box optimization problem. See also the
documentation of [bbotk](https://bbotk.mlr-org.com/).

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

## See also

Other Tuner:
[`Tuner`](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md),
[`mlr_tuners`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners.md),
[`mlr_tuners_cmaes`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_cmaes.md),
[`mlr_tuners_design_points`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_design_points.md),
[`mlr_tuners_gensa`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_gensa.md),
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
-\> `TunerBatchGridSearch`

## Methods

### Public methods

- [`TunerBatchGridSearch$new()`](#method-TunerBatchGridSearch-new)

- [`TunerBatchGridSearch$clone()`](#method-TunerBatchGridSearch-clone)

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

    TunerBatchGridSearch$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TunerBatchGridSearch$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Hyperparameter Optimization

# load learner and set search space
learner = lrn("classif.rpart",
  cp = to_tune(1e-04, 1e-1, logscale = TRUE)
)

# run hyperparameter tuning on the Palmer Penguins data set
instance = tune(
  tuner = tnr("grid_search"),
  task = tsk("penguins"),
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  term_evals = 10
)

# best performing hyperparameter configuration
instance$result
#>          cp learner_param_vals  x_domain classif.ce
#>       <num>             <list>    <list>      <num>
#> 1: -4.60517          <list[2]> <list[1]> 0.08695652

# all evaluated hyperparameter configuration
as.data.table(instance$archive)
#>            cp classif.ce runtime_learners           timestamp warnings errors
#>         <num>      <num>            <num>              <POSc>    <int>  <int>
#>  1: -4.605170 0.08695652            0.005 2026-02-19 14:35:16        0      0
#>  2: -7.675284 0.08695652            0.006 2026-02-19 14:35:17        0      0
#>  3: -9.210340 0.08695652            0.005 2026-02-19 14:35:17        0      0
#>  4: -6.907755 0.08695652            0.008 2026-02-19 14:35:17        0      0
#>  5: -3.070113 0.08695652            0.006 2026-02-19 14:35:17        0      0
#>  6: -2.302585 0.08695652            0.006 2026-02-19 14:35:17        0      0
#>  7: -5.372699 0.08695652            0.006 2026-02-19 14:35:17        0      0
#>  8: -6.140227 0.08695652            0.006 2026-02-19 14:35:17        0      0
#>  9: -3.837642 0.08695652            0.005 2026-02-19 14:35:17        0      0
#> 10: -8.442812 0.08695652            0.007 2026-02-19 14:35:17        0      0
#>      x_domain batch_nr  resample_result
#>        <list>    <int>           <list>
#>  1: <list[1]>        1 <ResampleResult>
#>  2: <list[1]>        2 <ResampleResult>
#>  3: <list[1]>        3 <ResampleResult>
#>  4: <list[1]>        4 <ResampleResult>
#>  5: <list[1]>        5 <ResampleResult>
#>  6: <list[1]>        6 <ResampleResult>
#>  7: <list[1]>        7 <ResampleResult>
#>  8: <list[1]>        8 <ResampleResult>
#>  9: <list[1]>        9 <ResampleResult>
#> 10: <list[1]>       10 <ResampleResult>

# fit final model on complete data set
learner$param_set$values = instance$result_learner_param_vals
learner$train(tsk("penguins"))
```
