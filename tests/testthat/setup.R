old_opts = options(
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE
)

# https://github.com/HenrikBengtsson/Wishlist-for-R/issues/88
old_opts = lapply(old_opts, function(x) if (is.null(x)) FALSE else x)

lg_mlr3 = lgr::get_logger("mlr3")
lg_bbotk = lgr::get_logger("bbotk")
lg_rush = lgr::get_logger("rush")

old_threshold_mlr3 = lg_mlr3$threshold
old_threshold_bbotk = lg_bbotk$threshold
old_threshold_rush = lg_rush$threshold

lg_mlr3$set_threshold(0)
lg_bbotk$set_threshold(0)
lg_rush$set_threshold(0)

