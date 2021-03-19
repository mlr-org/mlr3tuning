#' @param resampling ([mlr3::Resampling])\cr
#' Uninstantiated resamplings are instantiated during construction
#' so that all configurations are evaluated on the same data splits. If a new
#' resampling is passed, it is instantiated with new data splits. Already
#' instantiated resamplings are kept unchanged.
