# Hyperparameter Tuning with Iterated Racing.

Subclass for iterated racing. Calls
[`irace::irace()`](https://mlopez-ibanez.github.io/irace/reference/irace.html)
from package [irace](https://CRAN.R-project.org/package=irace).

## Source

Lopez-Ibanez M, Dubois-Lacoste J, Caceres LP, Birattari M, Stuetzle T
(2016). “The irace package: Iterated racing for automatic algorithm
configuration.” *Operations Research Perspectives*, **3**, 43–58.
[doi:10.1016/j.orp.2016.09.002](https://doi.org/10.1016/j.orp.2016.09.002)
.

## Dictionary

This [Tuner](https://mlr3tuning.mlr-org.com/reference/Tuner.md) can be
instantiated with the associated sugar function
[`tnr()`](https://mlr3tuning.mlr-org.com/reference/tnr.md):

    tnr("irace")

## Control Parameters

- `n_instances`:

  `integer(1)`  
  Number of resampling instances.

For the meaning of all other parameters, see
[`irace::defaultScenario()`](https://mlopez-ibanez.github.io/irace/reference/defaultScenario.html).
Note that we have removed all control parameters which refer to the
termination of the algorithm. Use
[bbotk::TerminatorEvals](https://bbotk.mlr-org.com/reference/mlr_terminators_evals.html)
instead. Other terminators do not work with `TunerIrace`.

## Archive

The
[ArchiveBatchTuning](https://mlr3tuning.mlr-org.com/reference/ArchiveBatchTuning.md)
holds the following additional columns:

- `"race"` (`integer(1)`)  
  Race iteration.

- `"step"` (`integer(1)`)  
  Step number of race.

- `"instance"` (`integer(1)`)  
  Identifies resampling instances across races and steps.

- `"configuration"` (`integer(1)`)  
  Identifies configurations across races and steps.

## Result

The tuning result (`instance$result`) is the best-performing elite of
the final race. The reported performance is the average performance
estimated on all used instances.

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

All [Tuner](https://mlr3tuning.mlr-org.com/reference/Tuner.md)s use a
logger (as implemented in [lgr](https://CRAN.R-project.org/package=lgr))
from package [bbotk](https://CRAN.R-project.org/package=bbotk). Use
`lgr::get_logger("bbotk")` to access and control the logger.

## Optimizer

This [Tuner](https://mlr3tuning.mlr-org.com/reference/Tuner.md) is based
on
[bbotk::OptimizerBatchIrace](https://bbotk.mlr-org.com/reference/mlr_optimizers_irace.html)
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
[`Tuner`](https://mlr3tuning.mlr-org.com/reference/Tuner.md),
[`mlr_tuners`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners.md),
[`mlr_tuners_cmaes`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_cmaes.md),
[`mlr_tuners_design_points`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_design_points.md),
[`mlr_tuners_gensa`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_gensa.md),
[`mlr_tuners_grid_search`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_grid_search.md),
[`mlr_tuners_internal`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_internal.md),
[`mlr_tuners_nloptr`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_nloptr.md),
[`mlr_tuners_random_search`](https://mlr3tuning.mlr-org.com/reference/mlr_tuners_random_search.md)

## Super classes

[`mlr3tuning::Tuner`](https://mlr3tuning.mlr-org.com/reference/Tuner.md)
-\>
[`mlr3tuning::TunerBatch`](https://mlr3tuning.mlr-org.com/reference/TunerBatch.md)
-\>
[`mlr3tuning::TunerBatchFromOptimizerBatch`](https://mlr3tuning.mlr-org.com/reference/TunerBatchFromOptimizerBatch.md)
-\> `TunerBatchIrace`

## Methods

### Public methods

- [`TunerBatchIrace$new()`](#method-TunerBatchIrace-new)

- [`TunerBatchIrace$optimize()`](#method-TunerBatchIrace-optimize)

- [`TunerBatchIrace$clone()`](#method-TunerBatchIrace-clone)

Inherited methods

- [`mlr3tuning::Tuner$format()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-format)
- [`mlr3tuning::Tuner$help()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-help)
- [`mlr3tuning::Tuner$print()`](https://mlr3tuning.mlr-org.com/reference/Tuner.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TunerBatchIrace$new()

------------------------------------------------------------------------

### Method [`optimize()`](https://rdrr.io/r/stats/optimize.html)

Performs the tuning on a
[TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md)
until termination. The single evaluations and the final results will be
written into the
[ArchiveBatchTuning](https://mlr3tuning.mlr-org.com/reference/ArchiveBatchTuning.md)
that resides in the
[TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md).
The final result is returned.

#### Usage

    TunerBatchIrace$optimize(inst)

#### Arguments

- `inst`:

  ([TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md)).

#### Returns

[data.table::data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TunerBatchIrace$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# retrieve task
task = tsk("pima")

# load learner and set search space
learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))

# runtime of the example is too long
# \donttest{
# hyperparameter tuning on the pima indians diabetes data set
instance = tune(
  tuner = tnr("irace"),
  task = task,
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  term_evals = 200
)
#> # 2025-11-26 09:13:33 UTC: Initialization
#> # Elitist race
#> # Elitist new instances: 1
#> # Elitist limit: 2
#> # nbIterations: 2
#> # minNbSurvival: 2
#> # nbParameters: 1
#> # seed: 1855097766
#> # confidence level: 0.95
#> # budget: 200
#> # mu: 5
#> # deterministic: FALSE
#> 
#> # 2025-11-26 09:13:33 UTC: Iteration 1 of 2
#> # experimentsUsed: 0
#> # remainingBudget: 200
#> # currentBudget: 100
#> # nbConfigurations: 16
#> # Markers:
#>      x No test is performed.
#>      c Configurations are discarded only due to capping.
#>      - The test is performed and some configurations are discarded.
#>      = The test is performed but no configuration is discarded.
#>      ! The test is performed and configurations could be discarded but elite configurations are preserved.
#>      . All alive configurations are elite and nothing is discarded.
#> 
#> +-+-----------+-----------+-----------+----------------+-----------+--------+-----+----+------+
#> | |   Instance|      Alive|       Best|       Mean best| Exp so far|  W time|  rho|KenW|  Qvar|
#> +-+-----------+-----------+-----------+----------------+-----------+--------+-----+----+------+
#> |x|          1|         16|          2|    0.2460937500|         16|00:00:00|   NA|  NA|    NA|
#> |x|          2|         16|          5|    0.2539062500|         32|00:00:00|+0.85|0.93|0.2432|
#> |x|          3|         16|          5|    0.2513020833|         48|00:00:00|+0.72|0.81|0.5409|
#> |x|          4|         16|          5|    0.2421875000|         64|00:00:00|+0.63|0.73|0.4926|
#> |-|          5|          6|          5|    0.2398437500|         80|00:00:00|+0.01|0.21|0.8365|
#> |=|          6|          6|          2|    0.2513020833|         86|00:00:00|-0.12|0.07|0.9438|
#> |=|          7|          6|          5|    0.2483258929|         92|00:00:00|-0.01|0.13|0.8680|
#> +-+-----------+-----------+-----------+----------------+-----------+--------+-----+----+------+
#> Best-so-far configuration:           5    mean value:     0.2483258929
#> Description of the best-so-far configuration:
#>   .ID.                cp .PARENT.
#> 5    5 -2.86582979189387       NA
#> 
#> # 2025-11-26 09:13:36 UTC: Elite configurations (first number is the configuration ID; listed from best to worst according to the sum of ranks):
#>                  cp
#> 5 -2.86582979189387
#> 2 -3.72929920176664
#> # 2025-11-26 09:13:36 UTC: Iteration 2 of 2
#> # experimentsUsed: 92
#> # remainingBudget: 108
#> # currentBudget: 108
#> # nbConfigurations: 15
#> # Markers:
#>      x No test is performed.
#>      c Configurations are discarded only due to capping.
#>      - The test is performed and some configurations are discarded.
#>      = The test is performed but no configuration is discarded.
#>      ! The test is performed and configurations could be discarded but elite configurations are preserved.
#>      . All alive configurations are elite and nothing is discarded.
#> 
#> +-+-----------+-----------+-----------+----------------+-----------+--------+-----+----+------+
#> | |   Instance|      Alive|       Best|       Mean best| Exp so far|  W time|  rho|KenW|  Qvar|
#> +-+-----------+-----------+-----------+----------------+-----------+--------+-----+----+------+
#> |x|          8|         15|         18|    0.2539062500|         15|00:00:00|   NA|  NA|    NA|
#> |x|          5|         15|         18|    0.2402343750|         28|00:00:00|+0.62|0.81|0.3638|
#> |x|          2|         15|         18|    0.2473958333|         41|00:00:00|+0.28|0.52|0.6042|
#> |x|          3|         15|         18|    0.2500000000|         54|00:00:00|-0.01|0.24|1.0053|
#> |=|          6|         15|         18|    0.2546875000|         67|00:00:00|-0.14|0.09|1.0726|
#> |=|          4|         15|         29|    0.2486979167|         80|00:00:00|-0.14|0.05|1.0898|
#> |=|          7|         15|         29|    0.2488839286|         93|00:00:00|-0.09|0.06|1.0318|
#> |=|          1|         15|         29|    0.2495117188|        106|00:00:00|-0.11|0.03|1.0296|
#> +-+-----------+-----------+-----------+----------------+-----------+--------+-----+----+------+
#> Best-so-far configuration:          29    mean value:     0.2495117188
#> Description of the best-so-far configuration:
#>    .ID.                cp .PARENT.
#> 29   29 -2.96257259670328        5
#> 
#> # 2025-11-26 09:13:39 UTC: Elite configurations (first number is the configuration ID; listed from best to worst according to the sum of ranks):
#>                   cp
#> 29 -2.96257259670328
#> 5  -2.86582979189387
#> # 2025-11-26 09:13:39 UTC: Stopped because there is not enough budget left to race more than the minimum (2).
#> # You may either increase the budget or set 'minNbSurvival' to a lower value.
#> # Iteration: 3
#> # nbIterations: 3
#> # experimentsUsed: 198
#> # timeUsed: 0
#> # remainingBudget: 2
#> # currentBudget: 2
#> # number of elites: 2
#> # nbConfigurations: 2
#> # Total CPU user time: 6.498, CPU sys time: 0.082, Wall-clock time: 6.58
#> # 2025-11-26 09:13:39 UTC: Starting post-selection:
#> # Configurations selected: 29, 5.
#> # Pending instances: 0, 0.
#> # 2025-11-26 09:13:40 UTC: seed: 1855097766
#> # Configurations: 2
#> # Available experiments: 2
#> # minSurvival: 1
#> # Markers:
#>      x No test is performed.
#>      c Configurations are discarded only due to capping.
#>      - The test is performed and some configurations are discarded.
#>      = The test is performed but no configuration is discarded.
#>      ! The test is performed and configurations could be discarded but elite configurations are preserved.
#>      . All alive configurations are elite and nothing is discarded.
#> 
#> +-+-----------+-----------+-----------+----------------+-----------+--------+-----+----+------+
#> | |   Instance|      Alive|       Best|       Mean best| Exp so far|  W time|  rho|KenW|  Qvar|
#> +-+-----------+-----------+-----------+----------------+-----------+--------+-----+----+------+
#> |.|          4|          2|          5|    0.2148437500|          0|00:00:00|   NA|  NA|    NA|
#> |.|          7|          2|          5|    0.2324218750|          0|00:00:00|+1.00|1.00|0.0000|
#> |.|          2|          2|          5|    0.2395833333|          0|00:00:00|+1.00|1.00|0.0000|
#> |.|          1|          2|          5|    0.2431640625|          0|00:00:00|+1.00|1.00|0.0000|
#> |.|          8|          2|          5|    0.2492187500|          0|00:00:00|+1.00|1.00|0.0000|
#> |.|          5|          2|          5|    0.2460937500|          0|00:00:00|+1.00|1.00|0.0000|
#> |.|          3|          2|          5|    0.2460937500|          0|00:00:00|+1.00|1.00|0.0000|
#> |.|          6|          2|          5|    0.2514648438|          0|00:00:00|+0.00|0.12|0.0625|
#> |-|          9|          1|         29|    0.2513020833|          2|00:00:00|   NA|  NA|    NA|
#> +-+-----------+-----------+-----------+----------------+-----------+--------+-----+----+------+
#> Best-so-far configuration:          29    mean value:     0.2513020833
#> Description of the best-so-far configuration:
#>    .ID.                cp .PARENT.
#> 29   29 -2.96257259670328        5
#> 
#> # 2025-11-26 09:13:40 UTC: Elite configurations (first number is the configuration ID; listed from best to worst according to the sum of ranks):
#>                   cp
#> 29 -2.96257259670328
#> # Total CPU user time: 6.646, CPU sys time: 0.082, Wall-clock time: 6.728

# best performing hyperparameter configuration
instance$result
#>           cp configuration learner_param_vals  x_domain classif.ce
#>        <num>         <int>             <list>    <list>      <num>
#> 1: -2.962573            29          <list[2]> <list[1]>  0.2513021

# all evaluated hyperparameter configuration
as.data.table(instance$archive)
#>             cp classif.ce runtime_learners           timestamp  race  step
#>          <num>      <num>            <num>              <POSc> <int> <int>
#>   1: -7.183177  0.3085938            0.007 2025-11-26 09:13:33     1     1
#>   2: -3.729299  0.2460938            0.007 2025-11-26 09:13:33     1     1
#>   3: -8.910116  0.3085938            0.026 2025-11-26 09:13:33     1     1
#>   4: -5.456238  0.3085938            0.007 2025-11-26 09:13:33     1     1
#>   5: -2.865830  0.2539062            0.008 2025-11-26 09:13:33     1     1
#>  ---                                                                      
#> 196: -2.847277  0.2539062            0.007 2025-11-26 09:13:39     2     1
#> 197: -2.716179  0.2812500            0.008 2025-11-26 09:13:39     2     1
#> 198: -2.962573  0.2539062            0.007 2025-11-26 09:13:39     2     1
#> 199: -2.865830  0.2656250            0.008 2025-11-26 09:13:40     3     1
#> 200: -2.962573  0.2656250            0.007 2025-11-26 09:13:40     3     1
#>      instance configuration warnings errors  x_domain batch_nr  resample_result
#>         <int>         <int>    <int>  <int>    <list>    <int>           <list>
#>   1:       10             1        0      0 <list[1]>        1 <ResampleResult>
#>   2:       10             2        0      0 <list[1]>        1 <ResampleResult>
#>   3:       10             3        0      0 <list[1]>        1 <ResampleResult>
#>   4:       10             4        0      0 <list[1]>        1 <ResampleResult>
#>   5:       10             5        0      0 <list[1]>        1 <ResampleResult>
#>  ---                                                                           
#> 196:       10            27        0      0 <list[1]>       15 <ResampleResult>
#> 197:       10            28        0      0 <list[1]>       15 <ResampleResult>
#> 198:       10            29        0      0 <list[1]>       15 <ResampleResult>
#> 199:        2             5        0      0 <list[1]>       16 <ResampleResult>
#> 200:        2            29        0      0 <list[1]>       16 <ResampleResult>

# fit final model on complete data set
learner$param_set$values = instance$result_learner_param_vals
learner$train(task)
# }
```
