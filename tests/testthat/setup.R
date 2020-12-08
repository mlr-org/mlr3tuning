library(mlr3)
attachNamespace("checkmate")
old_opts = options(
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE
)

# https://github.com/HenrikBengtsson/Wishlist-for-R/issues/88
old_opts = lapply(old_opts, function(x) if (is.null(x)) FALSE else x)

lg_mlr3 = lgr::get_logger("mlr3")
old_threshold_mlr3 = lg_mlr3$threshold
lg_mlr3$set_threshold("warn")
lg_bbotk = lgr::get_logger("bbotk")
old_threshold_bbotk = lg_bbotk$threshold
lg_bbotk$set_threshold("warn")
set.seed(123)
