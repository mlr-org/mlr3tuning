#' @examples
#' # load learner and set search space
#' learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))
#' 
#' # hyperparameter tuning on the pima indians diabetes data set
#' instance = tune(
#'   method = "<%= id %>",
#'   task = tsk("pima"),
#'   learner = learner,
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 10
#' )
#'
#' # best performing hyperparameter configuration
#' instance$result
#' 
#' # all evaluated hyperparameter configuration
#' as.data.table(instance$archive)
#' 
#' # fit final model on complete data set
#' learner$param_set$values = instance$result_learner_param_vals
#' learner$train(tsk("pima"))
