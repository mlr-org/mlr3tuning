# Hyperparameter Tuning with Generalized Simulated Annealing

Subclass for generalized simulated annealing tuning. Calls
[`GenSA::GenSA()`](https://rdrr.io/pkg/GenSA/man/GenSA.html) from
package [GenSA](https://CRAN.R-project.org/package=GenSA).

## Source

Tsallis C, Stariolo DA (1996). “Generalized simulated annealing.”
*Physica A: Statistical Mechanics and its Applications*, **233**(1-2),
395–406.
[doi:10.1016/s0378-4371(96)00271-3](https://doi.org/10.1016/s0378-4371%2896%2900271-3)
.

Xiang Y, Gubian S, Suomela B, Hoeng J (2013). “Generalized Simulated
Annealing for Global Optimization: The GenSA Package.” *The R Journal*,
**5**(1), 13.
[doi:10.32614/rj-2013-002](https://doi.org/10.32614/rj-2013-002) .

## Details

In contrast to the
[`GenSA::GenSA()`](https://rdrr.io/pkg/GenSA/man/GenSA.html) defaults,
we set `smooth = FALSE` as a default.

## Dictionary

This [Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md) can
be instantiated with the associated sugar function
[`tnr()`](https://mlr3tuning.mlr-org.com/dev/reference/tnr.md):

    tnr("gensa")

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
[bbotk::OptimizerBatchGenSA](https://bbotk.mlr-org.com/reference/mlr_optimizers_gensa.html)
which can be applied on any black box optimization problem. See also the
documentation of [bbotk](https://bbotk.mlr-org.com/).

## Parameters

- `par`:

  [`numeric()`](https://rdrr.io/r/base/numeric.html)  
  Initial parameter values. Default is `NULL`, in which case, default
  values will be generated automatically.

- `start_values`:

  `character(1)`  
  Create `"random"` start values or based on `"center"` of search space?
  In the latter case, it is the center of the parameters before a trafo
  is applied. By default, `nloptr` will generate start values
  automatically. Custom start values can be passed via the `par`
  parameter.

For the meaning of the control parameters, see
[`GenSA::GenSA()`](https://rdrr.io/pkg/GenSA/man/GenSA.html). Note that
[`GenSA::GenSA()`](https://rdrr.io/pkg/GenSA/man/GenSA.html) uses
`smooth = TRUE` as a default. In the case of using this optimizer for
Hyperparameter Optimization you may want to set `smooth = FALSE`.

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

Other Tuner:
[`Tuner`](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md),
[`mlr_tuners`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners.md),
[`mlr_tuners_cmaes`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_cmaes.md),
[`mlr_tuners_design_points`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_design_points.md),
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
-\> `TunerBatchGenSA`

## Methods

### Public methods

- [`TunerBatchGenSA$new()`](#method-TunerBatchGenSA-new)

- [`TunerBatchGenSA$clone()`](#method-TunerBatchGenSA-clone)

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

    TunerBatchGenSA$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TunerBatchGenSA$clone(deep = FALSE)

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
  tuner = tnr("gensa"),
  task = tsk("penguins"),
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  term_evals = 10
)
#> Warning: one-dimensional optimization by Nelder-Mead is unreliable:
#> use "Brent" or optimize() directly

# best performing hyperparameter configuration
instance$result
#>          cp learner_param_vals  x_domain classif.ce
#>       <num>             <list>    <list>      <num>
#> 1: -4.96455          <list[2]> <list[1]> 0.06086957

# all evaluated hyperparameter configuration
as.data.table(instance$archive)
#>            cp classif.ce runtime_learners           timestamp warnings errors
#>         <num>      <num>            <num>              <POSc>    <int>  <int>
#>  1: -4.964550 0.06086957            0.005 2025-11-07 11:34:05        0      0
#>  2: -9.001977 0.06086957            0.005 2025-11-07 11:34:05        0      0
#>  3: -6.812503 0.06086957            0.006 2025-11-07 11:34:05        0      0
#>  4: -4.964550 0.06086957            0.005 2025-11-07 11:34:05        0      0
#>  5: -4.964550 0.06086957            0.005 2025-11-07 11:34:05        0      0
#>  6: -4.964550 0.06086957            0.006 2025-11-07 11:34:05        0      0
#>  7: -4.468095 0.06086957            0.009 2025-11-07 11:34:05        0      0
#>  8: -5.461005 0.06086957            0.005 2025-11-07 11:34:05        0      0
#>  9: -5.212778 0.06086957            0.005 2025-11-07 11:34:05        0      0
#> 10: -4.716323 0.06086957            0.022 2025-11-07 11:34:05        0      0
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
