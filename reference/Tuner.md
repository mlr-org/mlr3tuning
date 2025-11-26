# Tuner

The `Tuner` implements the optimization algorithm.

## Details

`Tuner` is an abstract base class that implements the base functionality
each tuner must provide.

## Extension Packages

Additional tuners are provided by the following packages.

- [mlr3hyperband](https://github.com/mlr-org/mlr3hyperband) adds the
  Hyperband and Successive Halving algorithm.

- [mlr3mbo](https://github.com/mlr-org/mlr3mbo) adds Bayesian
  optimization methods.

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
[`mlr_tuners`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners.md),
[`mlr_tuners_cmaes`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_cmaes.md),
[`mlr_tuners_design_points`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_design_points.md),
[`mlr_tuners_gensa`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_gensa.md),
[`mlr_tuners_grid_search`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_grid_search.md),
[`mlr_tuners_internal`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_internal.md),
[`mlr_tuners_irace`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_irace.md),
[`mlr_tuners_nloptr`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_nloptr.md),
[`mlr_tuners_random_search`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_random_search.md)

## Public fields

- `id`:

  (`character(1)`)  
  Identifier of the object. Used in tables, plot and text output.

## Active bindings

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of control parameters.

- `param_classes`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Supported parameter classes for learner hyperparameters that the tuner
  can optimize, as given in the
  [paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
  `$class` field.

- `properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of properties of the tuner. Must be a subset of
  [`mlr_reflections$tuner_properties`](https://mlr3.mlr-org.com/reference/mlr_reflections.html).

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of required packages. Note that these packages will be loaded via
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html), and are
  not attached.

- `label`:

  (`character(1)`)  
  Label for this object. Can be used in tables, plot and text output
  instead of the ID.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. The referenced help package can be opened via method
  `$help()`.

## Methods

### Public methods

- [`Tuner$new()`](#method-Tuner-new)

- [`Tuner$format()`](#method-Tuner-format)

- [`Tuner$print()`](#method-Tuner-print)

- [`Tuner$help()`](#method-Tuner-help)

- [`Tuner$clone()`](#method-Tuner-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    Tuner$new(
      id = "tuner",
      param_set,
      param_classes,
      properties,
      packages = character(),
      label = NA_character_,
      man = NA_character_
    )

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of control parameters.

- `param_classes`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Supported parameter classes for learner hyperparameters that the tuner
  can optimize, as given in the
  [paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
  `$class` field.

- `properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of properties of the tuner. Must be a subset of
  [`mlr_reflections$tuner_properties`](https://mlr3.mlr-org.com/reference/mlr_reflections.html).

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of required packages. Note that these packages will be loaded via
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html), and are
  not attached.

- `label`:

  (`character(1)`)  
  Label for this object. Can be used in tables, plot and text output
  instead of the ID.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. The referenced help package can be opened via method
  `$help()`.

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    Tuner$format(...)

#### Arguments

- `...`:

  (ignored).

#### Returns

([`character()`](https://rdrr.io/r/base/character.html)).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method.

#### Usage

    Tuner$print()

#### Returns

([`character()`](https://rdrr.io/r/base/character.html)).

------------------------------------------------------------------------

### Method [`help()`](https://rdrr.io/r/utils/help.html)

Opens the corresponding help page referenced by field `$man`.

#### Usage

    Tuner$help()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Tuner$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
