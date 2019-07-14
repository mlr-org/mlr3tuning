old_opts = options(
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE
)
lg = lgr::get_logger("mlr3")
old_threshold = lg$threshold
lg$set_threshold("warn")
