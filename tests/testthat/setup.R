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
