#' @param store_models (`logical(1)`)\cr
#' If `FALSE` (default), the fitted models are not stored in the
#' [mlr3::BenchmarkResult]. If `store_benchmark_result = FALSE`, the models are
#' only stored temporarily and not accessible after the tuning. This combination
#' might be useful for measures that require a model.
