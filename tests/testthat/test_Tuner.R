context("Tuner")

test_that("API", {
  for (n_evals in c(1, 5)) {
    rs = TunerRandomSearch$new()
    inst = TEST_MAKE_INST1(measure = msr("classif.ce"), term_evals = n_evals)
    rs$optimize(inst)
    a = inst$archive$data()
    expect_data_table(a, nrows = n_evals)
    expect_true("cp" %in% names(a))
  }
})

test_that("proper error if tuner cannot handle deps", {
  skip_if_not_installed("GenSA")
  ps = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamDbl$new("minsplit", lower = 1, upper = 10)
  ))
  ps$add_dep("minsplit", on = "cp", cond = CondEqual$new(0.1))
  te = term("evals", n_evals = 2)
  inst = TuningInstance$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), ps, te)
  tt = TunerGenSA$new()
  expect_error(tt$optimize(inst), "dependencies")
})

test_that("we get a result when some subordinate params are not fulfilled", {
  TunerManual = R6Class("TunerManual",
    inherit = Tuner,
    public = list(
      initialize = function() {
        super$initialize(
          param_set = ParamSet$new(),
          param_classes = c("ParamFct", "ParamDbl"),
          properties = c("dependencies", "single-crit")
        )
      }
    ),
    private = list(
      .optimize = function(inst) {}
    )
  )
  tuner_manual = TunerManual$new()
  inst = TEST_MAKE_INST2(measure = msr("dummy.cp.regr"))
  d = data.table(xx = c("a", "b"), yy = c(1, NA), cp = c(0.2, 0.1))
  inst$eval_batch(d)
  tuner_manual$optimize(inst)
  expect_equal(inst$result_y, c(dummy.cp.regr = 0.1))
  expect_equal(inst$result_x_domain, list(xx = "b", cp = 0.1))
  expect_equal(inst$result_x_domain, inst$result_learner_param_vals)
})

