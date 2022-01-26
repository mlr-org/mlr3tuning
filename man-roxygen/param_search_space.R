#' @param search_space ([paradox::ParamSet] | [mlr3tuningspaces::TuningSpace])\cr
#'   Hyperparameter search space. If `NULL` (default), the search space is
#'   constructed from the [TuneToken] of the learner's parameter set
#'   (learner$param_set).
