#' @param resampling ([mlr3::Resampling])\cr
#' Resampling that is used to evaluated the performance of the hyperparameter
#' configurations. Uninstantiated resamplings are instantiated during
#' construction so that all configurations are evaluated on the same data
#' splits. Already instantiated resamplings are kept unchanged. Specialized
#' [Tuner] change the resampling e.g. to evaluate a hyperparameter configuration
#' on different data splits. This field, however, always returns the resampling
#' passed in construction.
