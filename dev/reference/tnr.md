# Syntactic Sugar for Tuning Objects Construction

Functions to retrieve objects, set parameters and assign to fields in
one go. Relies on
[`mlr3misc::dictionary_sugar_get()`](https://mlr3misc.mlr-org.com/reference/dictionary_sugar_get.html)
to extract objects from the respective
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html):

- `tnr()` for a
  [Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md) from
  [mlr_tuners](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners.md).

- `tnrs()` for a list of
  [Tuners](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md) from
  [mlr_tuners](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners.md).

- [`trm()`](https://bbotk.mlr-org.com/reference/trm.html) for a
  [bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html)
  from
  [mlr_terminators](https://bbotk.mlr-org.com/reference/mlr_terminators.html).

- [`trms()`](https://bbotk.mlr-org.com/reference/trm.html) for a list of
  [Terminators](https://bbotk.mlr-org.com/reference/Terminator.html)
  from
  [mlr_terminators](https://bbotk.mlr-org.com/reference/mlr_terminators.html).

## Usage

``` r
tnr(.key, ...)

tnrs(.keys, ...)
```

## Arguments

- .key:

  (`character(1)`)  
  Key passed to the respective
  [dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  to retrieve the object.

- ...:

  (any)  
  Additional arguments.

- .keys:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Keys passed to the respective
  [dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  to retrieve multiple objects.

## Value

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object of the
respective type, or a list of
[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) objects for
the plural versions.

## Examples

``` r
# random search tuner with batch size of 5
tnr("random_search", batch_size = 5)
#> 
#> ── <TunerBatchRandomSearch>: Random Search ─────────────────────────────────────
#> • Parameters: batch_size=5
#> • Parameter classes: <ParamLgl>, <ParamInt>, <ParamDbl>, and <ParamFct>
#> • Properties: dependencies, single-crit, and multi-crit
#> • Packages: mlr3tuning and bbotk

# run time terminator with 20 seconds
trm("run_time", secs = 20)
#> 
#> ── <TerminatorRunTime> - Run Time ──────────────────────────────────────────────
#> • Parameters: secs=20
#> • Terminators:
```
