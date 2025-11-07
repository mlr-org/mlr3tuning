# Dictionary of Tuners

A simple
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
storing objects of class
[Tuner](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md). Each
tuner has an associated help page, see `mlr_tuners_[id]`.

This dictionary can get populated with additional tuners by add-on
packages.

For a more convenient way to retrieve and construct tuner, see
[`tnr()`](https://mlr3tuning.mlr-org.com/dev/reference/tnr.md)/[`tnrs()`](https://mlr3tuning.mlr-org.com/dev/reference/tnr.md).

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## Methods

See
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## S3 methods

- `as.data.table(dict, ..., objects = FALSE)`  
  [mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  -\>
  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)  
  Returns a
  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  with fields "key", "label", "param_classes", "properties" and
  "packages" as columns. If `objects` is set to `TRUE`, the constructed
  objects are returned in the list column named `object`.

## See also

Sugar functions:
[`tnr()`](https://mlr3tuning.mlr-org.com/dev/reference/tnr.md),
[`tnrs()`](https://mlr3tuning.mlr-org.com/dev/reference/tnr.md)

Other Tuner:
[`Tuner`](https://mlr3tuning.mlr-org.com/dev/reference/Tuner.md),
[`mlr_tuners_cmaes`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_cmaes.md),
[`mlr_tuners_design_points`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_design_points.md),
[`mlr_tuners_gensa`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_gensa.md),
[`mlr_tuners_grid_search`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_grid_search.md),
[`mlr_tuners_internal`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_internal.md),
[`mlr_tuners_irace`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_irace.md),
[`mlr_tuners_nloptr`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_nloptr.md),
[`mlr_tuners_random_search`](https://mlr3tuning.mlr-org.com/dev/reference/mlr_tuners_random_search.md)

## Examples

``` r
as.data.table(mlr_tuners)
#> Key: <key>
#>                     key                                           label
#>                  <char>                                          <char>
#>  1: async_design_points                      Asynchronous Design Points
#>  2:   async_grid_search                        Asynchronous Grid Search
#>  3: async_random_search                      Asynchronous Random Search
#>  4:               cmaes Covariance Matrix Adaptation Evolution Strategy
#>  5:       design_points                                   Design Points
#>  6:               gensa                 Generalized Simulated Annealing
#>  7:         grid_search                                     Grid Search
#>  8:            internal                              Internal Optimizer
#>  9:               irace                                 Iterated Racing
#> 10:              nloptr                         Non-linear Optimization
#> 11:       random_search                                   Random Search
#>                                    param_classes
#>                                           <list>
#>  1: ParamLgl,ParamInt,ParamDbl,ParamFct,ParamUty
#>  2:          ParamLgl,ParamInt,ParamDbl,ParamFct
#>  3:          ParamLgl,ParamInt,ParamDbl,ParamFct
#>  4:                                     ParamDbl
#>  5: ParamLgl,ParamInt,ParamDbl,ParamFct,ParamUty
#>  6:                                     ParamDbl
#>  7:          ParamLgl,ParamInt,ParamDbl,ParamFct
#>  8:          ParamLgl,ParamInt,ParamDbl,ParamFct
#>  9:          ParamDbl,ParamInt,ParamFct,ParamLgl
#> 10:                                     ParamDbl
#> 11:          ParamLgl,ParamInt,ParamDbl,ParamFct
#>                                    properties                packages
#>                                        <list>                  <list>
#>  1: dependencies,single-crit,multi-crit,async   mlr3tuning,bbotk,rush
#>  2: dependencies,single-crit,multi-crit,async   mlr3tuning,bbotk,rush
#>  3: dependencies,single-crit,multi-crit,async   mlr3tuning,bbotk,rush
#>  4:                               single-crit mlr3tuning,bbotk,adagio
#>  5:       dependencies,single-crit,multi-crit        mlr3tuning,bbotk
#>  6:                               single-crit  mlr3tuning,bbotk,GenSA
#>  7:       dependencies,single-crit,multi-crit        mlr3tuning,bbotk
#>  8:                  dependencies,single-crit              mlr3tuning
#>  9:                  dependencies,single-crit  mlr3tuning,bbotk,irace
#> 10:                               single-crit mlr3tuning,bbotk,nloptr
#> 11:       dependencies,single-crit,multi-crit        mlr3tuning,bbotk
mlr_tuners$get("random_search")
#> 
#> ── <TunerBatchRandomSearch>: Random Search ─────────────────────────────────────
#> • Parameters: batch_size=1
#> • Parameter classes: <ParamLgl>, <ParamInt>, <ParamDbl>, and <ParamFct>
#> • Properties: dependencies, single-crit, and multi-crit
#> • Packages: mlr3tuning and bbotk
tnr("random_search")
#> 
#> ── <TunerBatchRandomSearch>: Random Search ─────────────────────────────────────
#> • Parameters: batch_size=1
#> • Parameter classes: <ParamLgl>, <ParamInt>, <ParamDbl>, and <ParamFct>
#> • Properties: dependencies, single-crit, and multi-crit
#> • Packages: mlr3tuning and bbotk
```
