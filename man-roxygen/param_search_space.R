#' @param search_space ([paradox::ParamSet])\cr
#'   Hyperparameter search space.
#'   If `NULL` (default), the search space is constructed from the [paradox::TuneToken] of the learner's parameter set (learner$param_set).
#'   When using `to_tune()` tokens, dependencies for hierarchical search spaces are automatically handled.
