library(mlr3)
old_opts = options(
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE
)

# https://github.com/HenrikBengtsson/Wishlist-for-R/issues/88
old_opts = lapply(old_opts, function(x) if (is.null(x)) FALSE else x)

lg = lgr::get_logger("mlr3")
old_threshold = lg$threshold
lg$set_threshold("warn")
set.seed(123)


# create a simple PE object for rpart with cp param and 2CV resampling
TEST_MAKE_PS1 = function() {
  ps = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1)
  ))
}
TEST_MAKE_PE1 = function(values = NULL, folds = 2L, measures = "classif.ce") {
  ps = TEST_MAKE_PS1()
  lrn = mlr_learners$get("classif.rpart")
  if (!is.null(values))
    lrn$param_set$values = values
  rs = mlr_resamplings$get("cv", param_vals = list(folds = folds))
  pe = PerfEval$new("iris", lrn, rs, measures, ps)
  return(pe)
}

