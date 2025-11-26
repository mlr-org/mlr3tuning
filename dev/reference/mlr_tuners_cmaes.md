# Hyperparameter Tuning with Covariance Matrix Adaptation Evolution Strategy

Subclass for Covariance Matrix Adaptation Evolution Strategy (CMA-ES).
Calls [`adagio::pureCMAES()`](https://rdrr.io/pkg/adagio/man/cmaes.html)
from package [adagio](https://CRAN.R-project.org/package=adagio).

## Source

Hansen N (2016). “The CMA Evolution Strategy: A Tutorial.” 1604.00772.

## Dictionary

This [Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md) can
be instantiated with the associated sugar function
[`tnr()`](https://mlr3tuning.mlr-org.com/dev/reference/tnr.md):

    tnr("cmaes")

## Control Parameters

- `start_values`:

  `character(1)`  
  Create `random` start values or based on `center` of search space? In
  the latter case, it is the center of the parameters before a trafo is
  applied.

For the meaning of the control parameters, see
[`adagio::pureCMAES()`](https://rdrr.io/pkg/adagio/man/cmaes.html). Note
that we have removed all control parameters which refer to the
termination of the algorithm and where our terminators allow to obtain
the same behavior.

## Progress Bars

`$optimize()` supports progress bars via the package
[progressr](https://CRAN.R-project.org/package=progressr) combined with
a
[bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html).
Simply wrap the function in `progressr::with_progress()` to enable them.
We recommend to use package
[progress](https://CRAN.R-project.org/package=progress) as backend;
enable with `progressr::handlers("progress")`.

## Logging

All [Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md)s use
a logger (as implemented in
[lgr](https://CRAN.R-project.org/package=lgr)) from package
[bbotk](https://CRAN.R-project.org/package=bbotk). Use
`lgr::get_logger("bbotk")` to access and control the logger.

## Optimizer

This [Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md) is
based on
[bbotk::OptimizerBatchCmaes](https://bbotk.mlr-org.com/reference/mlr_optimizers_cmaes.html)
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
[`mlr_tuners_design_points`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_design_points.md),
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
-\> `TunerBatchCmaes`

## Methods

### Public methods

- [`TunerBatchCmaes$new()`](#method-TunerBatchCmaes-new)

- [`TunerBatchCmaes$clone()`](#method-TunerBatchCmaes-clone)

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

    TunerBatchCmaes$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TunerBatchCmaes$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Hyperparameter Optimization

# load learner and set search space
learner = lrn("classif.rpart",
  cp = to_tune(1e-04, 1e-1, logscale = TRUE),
  minsplit = to_tune(p_dbl(2, 128, trafo = as.integer)),
  minbucket = to_tune(p_dbl(1, 64, trafo = as.integer))
)

# run hyperparameter tuning on the Palmer Penguins data set
instance = tune(
  tuner = tnr("cmaes"),
  task = tsk("penguins"),
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  term_evals = 10)

# best performing hyperparameter configuration
instance$result
#>          cp minbucket minsplit learner_param_vals  x_domain classif.ce
#>       <num>     <num>    <num>             <list>    <list>      <num>
#> 1: -9.21034         1 44.08528          <list[4]> <list[3]> 0.04347826

# all evaluated hyperparameter configuration
as.data.table(instance$archive)
#>            cp minbucket  minsplit classif.ce runtime_learners
#>         <num>     <num>     <num>      <num>            <num>
#>  1: -8.093058  1.000000   2.00000 0.05217391            0.005
#>  2: -9.210340 10.648145  46.54107 0.06086957            0.007
#>  3: -2.302585  1.000000 128.00000 0.06086957            0.006
#>  4: -8.146263 28.083484  90.18488 0.06086957            0.005
#>  5: -5.501698 11.868902  13.80173 0.06086957            0.025
#>  6: -7.999490  6.263489 122.64745 0.06086957            0.005
#>  7: -9.210340 64.000000  92.97889 0.11304348            0.006
#>  8: -9.210340  1.000000  44.08528 0.04347826            0.006
#>  9: -4.730547  1.000000  86.76723 0.04347826            0.006
#> 10: -9.210340  1.000000   2.00000 0.05217391            0.006
#>               timestamp warnings errors  x_domain batch_nr  resample_result
#>                  <POSc>    <int>  <int>    <list>    <int>           <list>
#>  1: 2025-11-26 09:16:31        0      0 <list[3]>        1 <ResampleResult>
#>  2: 2025-11-26 09:16:31        0      0 <list[3]>        2 <ResampleResult>
#>  3: 2025-11-26 09:16:31        0      0 <list[3]>        3 <ResampleResult>
#>  4: 2025-11-26 09:16:31        0      0 <list[3]>        4 <ResampleResult>
#>  5: 2025-11-26 09:16:31        0      0 <list[3]>        5 <ResampleResult>
#>  6: 2025-11-26 09:16:31        0      0 <list[3]>        6 <ResampleResult>
#>  7: 2025-11-26 09:16:31        0      0 <list[3]>        7 <ResampleResult>
#>  8: 2025-11-26 09:16:31        0      0 <list[3]>        8 <ResampleResult>
#>  9: 2025-11-26 09:16:31        0      0 <list[3]>        9 <ResampleResult>
#> 10: 2025-11-26 09:16:31        0      0 <list[3]>       10 <ResampleResult>

# fit final model on complete data set
learner$param_set$values = instance$result_learner_param_vals
learner$train(tsk("penguins"))
```
