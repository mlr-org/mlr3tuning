#' @examples
#' instance = TuningInstanceSingleCrit$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   terminator = trm("evals", n_evals = 3)
#' )
#' tuner = tnr("<%= id %>")
#'
#' # optimize hyperparameter
#' # modifies the instance by reference
#' tuner$optimize(instance)
#'
#' # returns best configuration and best performance
#' instance$result
#'
#' # allows access of data.table of full path of all evaluations
#' instance$archive
