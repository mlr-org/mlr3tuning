# Freeze Archive Callback

This
[CallbackAsyncTuning](https://mlr3tuning.mlr-org.com/reference/CallbackAsyncTuning.md)
freezes the
[ArchiveAsyncTuning](https://mlr3tuning.mlr-org.com/reference/ArchiveAsyncTuning.md)
to
[ArchiveAsyncTuningFrozen](https://mlr3tuning.mlr-org.com/reference/ArchiveAsyncTuningFrozen.md)
after the optimization has finished.

## Examples

``` r
clbk("mlr3tuning.async_freeze_archive")
#> <CallbackAsyncTuning:mlr3tuning.async_freeze_archive>: Archive Freeze Callback
#> * Active Stages: on_optimization_end
```
