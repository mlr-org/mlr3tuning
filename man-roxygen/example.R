#' @examples
#' library(mlr3)
#' library(paradox)
#' search_space = ParamSet$new(list(
#'   ParamDbl$new("cp", lower = 0.001, upper = 0.1)
#' ))
#' terminator = term("evals", n_evals = 3)
#' instance = TuningInstance$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   search_space = search_space,
#'   terminator = terminator
#' )
#' tt = tnr("<%= id %>")
#' # modifies the instance by reference
#' tt$optimize(instance)
#' # returns best configuration and best performance
#' instance$result
#' # allows access of data.table of full path of all evaluations
#' instance$archive
