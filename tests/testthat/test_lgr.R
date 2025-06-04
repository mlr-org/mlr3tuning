# test_that("logger works", {
#   on.exit({
#     lgr::get_logger("mlr3")$set_threshold(0)
#     lgr::get_logger("mlr3/core")$set_threshold(0)
#     lgr::get_logger("mlr3/bbotk")$set_threshold(0)
#   })

#   # reset loggers
#   lgr::get_logger("mlr3")$set_threshold(NULL)
#   lgr::get_logger("mlr3/core")$set_threshold(NULL)
#   lgr::get_logger("mlr3/bbotk")$set_threshold(NULL)

#   # log level is inherited from root logger
#   res = capture_output(tune(
#     tuner = tnr("random_search"),
#     task = tsk("pima"),
#     learner = lrn("classif.featureless"),
#     resampling = rsmp("cv", folds = 3L),
#     measures = msr("classif.acc"),
#     term_evals = 2L
#   ))
#   expect_match(res, "\\[mlr3\\]")
#   expect_match(res, "\\[bbotk\\]")

#   # turn off mlr3 core and bbotk logging via parent logger
#   lgr::get_logger("mlr3")$set_threshold("error")
#   res = capture_output(tune(
#     tuner = tnr("random_search"),
#     task = tsk("pima"),
#     learner = lrn("classif.featureless"),
#     resampling = rsmp("cv", folds = 3L),
#     measures = msr("classif.acc"),
#     term_evals = 2L
#   ))
#   expect_equal(res, "")

#   # turn on mlr3 core and bbotk logging via parent logger
#   lgr::get_logger("mlr3")$set_threshold("info")
#   res = capture_output(tune(
#     tuner = tnr("random_search"),
#     task = tsk("pima"),
#     learner = lrn("classif.featureless"),
#     resampling = rsmp("cv", folds = 3L),
#     measures = msr("classif.acc"),
#     term_evals = 2L
#   ))
#   expect_match(res, "\\[mlr3\\]")
#   expect_match(res, "\\[bbotk\\]")

#   # turn off bbotk logging
#   lgr::get_logger("mlr3/bbotk")$set_threshold("error")
#   res = capture_output(tune(
#     tuner = tnr("random_search"),
#     task = tsk("pima"),
#     learner = lrn("classif.featureless"),
#     resampling = rsmp("cv", folds = 3L),
#     measures = msr("classif.acc"),
#     term_evals = 2L
#   ))
#   expect_match(res, "\\[mlr3\\]")
#   expect_no_match(res, "\\[bbotk\\]")

#   # turn on bbotk logging and turn off mlr3 core logging
#   lgr::get_logger("mlr3/bbotk")$set_threshold("info")
#   lgr::get_logger("mlr3/core")$set_threshold("error")
#   res = capture_output(tune(
#     tuner = tnr("random_search"),
#     task = tsk("pima"),
#     learner = lrn("classif.featureless"),
#     resampling = rsmp("cv", folds = 3L),
#     measures = msr("classif.acc"),
#     term_evals = 2L
#   ))

#   expect_no_match(res, "\\[mlr3\\]")
#   expect_match(res, "\\[bbotk\\]")

#   # if the child logger is configured, it overrides the parent logger
#   lgr::get_logger("mlr3")$set_threshold("info")
#   lgr::get_logger("mlr3/core")$set_threshold("error")
#   lgr::get_logger("mlr3/bbotk")$set_threshold("error")
#   res = capture_output(tune(
#     tuner = tnr("random_search"),
#     task = tsk("pima"),
#     learner = lrn("classif.featureless"),
#     resampling = rsmp("cv", folds = 3L),
#     measures = msr("classif.acc"),
#     term_evals = 2L
#   ))
#   expect_equal(res, "")
# })

# test_that("thresholds are restored on workers", {

#    on.exit({
#     lgr::get_logger("mlr3")$set_threshold(0)
#     lgr::get_logger("mlr3/core")$set_threshold(0)
#     lgr::get_logger("mlr3/bbotk")$set_threshold(0)
#   })

#   # reset loggers
#   lgr::get_logger("mlr3")$set_threshold(NULL)
#   lgr::get_logger("mlr3/core")$set_threshold(NULL)
#   lgr::get_logger("mlr3/bbotk")$set_threshold(NULL)

#   # log level is inherited from root logger
#   res = capture_output(with_future(future::multisession, {
#     tune(
#       tuner = tnr("random_search"),
#       task = tsk("pima"),
#       learner = lrn("classif.featureless"),
#       resampling = rsmp("cv", folds = 3L),
#       measures = msr("classif.acc"),
#       term_evals = 2L
#     )
#   }))
#   expect_match(res, "\\[mlr3\\]")
#   expect_match(res, "\\[bbotk\\]")

#   # turn off mlr3 core and bbotk logging via parent logger
#   lgr::get_logger("mlr3")$set_threshold("error")
#   res = capture_output(with_future(future::multisession, {
#     tune(
#       tuner = tnr("random_search"),
#       task = tsk("pima"),
#       learner = lrn("classif.featureless"),
#       resampling = rsmp("cv", folds = 3L),
#       measures = msr("classif.acc"),
#       term_evals = 2L
#     )
#   }))
#   expect_equal(res, "")

#   # turn on mlr3 core and bbotk logging via parent logger
#   lgr::get_logger("mlr3")$set_threshold("info")
#   res = capture_output(with_future(future::multisession, {
#     tune(
#       tuner = tnr("random_search"),
#       task = tsk("pima"),
#       learner = lrn("classif.featureless"),
#       resampling = rsmp("cv", folds = 3L),
#       measures = msr("classif.acc"),
#       term_evals = 2L
#     )
#   }))
#   expect_match(res, "\\[mlr3\\]")
#   expect_match(res, "\\[bbotk\\]")

#   # turn off bbotk logging
#   lgr::get_logger("mlr3/bbotk")$set_threshold("error")
#   res = capture_output(with_future(future::multisession, {
#     tune(
#       tuner = tnr("random_search"),
#       task = tsk("pima"),
#       learner = lrn("classif.featureless"),
#       resampling = rsmp("cv", folds = 3L),
#       measures = msr("classif.acc"),
#       term_evals = 2L
#     )
#   }))
#   expect_match(res, "\\[mlr3\\]")
#   expect_no_match(res, "\\[bbotk\\]")

#   # turn on bbotk logging and turn off mlr3 core logging
#   lgr::get_logger("mlr3/bbotk")$set_threshold("info")
#   lgr::get_logger("mlr3/core")$set_threshold("error")
#   res = capture_output(with_future(future::multisession, {
#     tune(
#       tuner = tnr("random_search"),
#       task = tsk("pima"),
#       learner = lrn("classif.featureless"),
#       resampling = rsmp("cv", folds = 3L),
#       measures = msr("classif.acc"),
#       term_evals = 2L
#     )
#   }))

#   expect_no_match(res, "\\[mlr3\\]")
#   expect_match(res, "\\[bbotk\\]")

#   # if the child logger is configured, it overrides the parent logger
#   lgr::get_logger("mlr3")$set_threshold("info")
#   lgr::get_logger("mlr3/core")$set_threshold("error")
#   lgr::get_logger("mlr3/bbotk")$set_threshold("error")
#   res = capture_output(with_future(future::multisession, {
#     tune(
#       tuner = tnr("random_search"),
#       task = tsk("pima"),
#       learner = lrn("classif.featureless"),
#       resampling = rsmp("cv", folds = 3L),
#       measures = msr("classif.acc"),
#       term_evals = 2L
#     )
#   }))
#   expect_equal(res, "")
# })

