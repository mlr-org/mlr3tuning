#' @section Parallelization:
#' In order to support general termination criteria and parallelization, we
#' evaluate points in a batch-fashion of size `batch_size`. Larger batches mean
#' we can parallelize more, smaller batches imply a more fine-grained checking
#' of termination criteria. A batch consists of `batch_size` times `resampling$iters` jobs.
#' E.g., if you set a batch size of 10 points and do a 5-fold cross validation, you can
#' utilize up to 50 cores.
#'
#' Parallelization is supported via package \CRANpkg{future} (see [mlr3::benchmark()]'s
#' section on parallelization for more details).
