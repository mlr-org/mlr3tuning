# Hyperparameter Tuning with Non-linear Optimization

Subclass for non-linear optimization (NLopt). Calls
[nloptr::nloptr](https://astamm.github.io/nloptr/reference/nloptr.html)
from package [nloptr](https://CRAN.R-project.org/package=nloptr).

## Source

Johnson, G S (2020). “The NLopt nonlinear-optimization package.”
<https://github.com/stevengj/nlopt>.

## Details

The termination conditions `stopval`, `maxtime` and `maxeval` of
[`nloptr::nloptr()`](https://astamm.github.io/nloptr/reference/nloptr.html)
are deactivated and replaced by the
[bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html)
subclasses. The x and function value tolerance termination conditions
(`xtol_rel = 10^-4`, `xtol_abs = rep(0.0, length(x0))`, `ftol_rel = 0.0`
and `ftol_abs = 0.0`) are still available and implemented with their
package defaults. To deactivate these conditions, set them to `-1`.

## Dictionary

This [Tuner](https://mlr3tuning.mlr-org.com/reference/Tuner.md) can be
instantiated with the associated sugar function
[`tnr()`](https://mlr3tuning.mlr-org.com/reference/tnr.md):

    tnr("nloptr")

## Logging

All [Tuner](https://mlr3tuning.mlr-org.com/reference/Tuner.md)s use a
logger (as implemented in [lgr](https://CRAN.R-project.org/package=lgr))
from package [bbotk](https://CRAN.R-project.org/package=bbotk). Use
`lgr::get_logger("bbotk")` to access and control the logger.

## Optimizer

This [Tuner](https://mlr3tuning.mlr-org.com/reference/Tuner.md) is based
on
[bbotk::OptimizerBatchNLoptr](https://bbotk.mlr-org.com/reference/mlr_optimizers_nloptr.html)
which can be applied on any black box optimization problem. See also the
documentation of [bbotk](https://bbotk.mlr-org.com/).

## Parameters

- `algorithm`:

  `character(1)`  
  Algorithm to use. See
  [`nloptr::nloptr.print.options()`](https://astamm.github.io/nloptr/reference/nloptr.print.options.html)
  for available algorithms.

- `x0`:

  [`numeric()`](https://rdrr.io/r/base/numeric.html)  
  Initial parameter values. Use `start_values` parameter to create
  `"random"` or `"center"` start values.

- `start_values`:

  `character(1)`  
  Create `"random"` start values or based on `"center"` of search space?
  In the latter case, it is the center of the parameters before a trafo
  is applied. Custom start values can be passed via the `x0` parameter.

- `approximate_eval_grad_f`:

  `logical(1)`  
  Should gradients be numerically approximated via finite differences
  ([nloptr::nl.grad](https://astamm.github.io/nloptr/reference/nl.grad.html)).
  Only required for certain algorithms. Note that function evaluations
  required for the numerical gradient approximation will be logged as
  usual and are not treated differently than regular function
  evaluations by, e.g.,
  [Terminator](https://bbotk.mlr-org.com/reference/Terminator.html)s.

For the meaning of other control parameters, see
[`nloptr::nloptr()`](https://astamm.github.io/nloptr/reference/nloptr.html)
and
[`nloptr::nloptr.print.options()`](https://astamm.github.io/nloptr/reference/nloptr.print.options.html).

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
[`Tuner`](https://mlr3tuning.mlr-org.com/reference/Tuner.md),
[`mlr_tuners`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners.md),
[`mlr_tuners_cmaes`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_cmaes.md),
[`mlr_tuners_design_points`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_design_points.md),
[`mlr_tuners_gensa`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_gensa.md),
[`mlr_tuners_grid_search`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_grid_search.md),
[`mlr_tuners_internal`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_internal.md),
[`mlr_tuners_irace`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_irace.md),
[`mlr_tuners_random_search`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_random_search.md)

## Super classes

[`mlr3tuning::Tuner`](https://mlr3tuning.mlr-org.com/reference/Tuner.md)
-\>
[`mlr3tuning::TunerBatch`](https://mlr3tuning.mlr-org.com/reference/TunerBatch.md)
-\>
[`mlr3tuning::TunerBatchFromOptimizerBatch`](https://mlr3tuning.mlr-org.com/reference/TunerBatchFromOptimizerBatch.md)
-\> `TunerBatchNLoptr`

## Methods

### Public methods

- [`TunerBatchNLoptr$new()`](#method-TunerBatchNLoptr-new)

- [`TunerBatchNLoptr$clone()`](#method-TunerBatchNLoptr-clone)

Inherited methods

- [`mlr3tuning::Tuner$format()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-format)
- [`mlr3tuning::Tuner$help()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-help)
- [`mlr3tuning::Tuner$print()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-print)
- [`mlr3tuning::TunerBatchFromOptimizerBatch$optimize()`](https://mlr3tuning.mlr-org.com/reference/TunerBatchFromOptimizerBatch.html#method-optimize)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TunerBatchNLoptr$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TunerBatchNLoptr$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples
