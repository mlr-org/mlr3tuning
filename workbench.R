devtools::load_all()
library(data.table)

set.seed(123)
lg$set_threshold("error")

# define hyperparameter and budget parameter for tuning with hyperband
ps = ParamSet$new(list(
  ParamDbl$new("cp",       lower = 0.001, upper = 0.1),
  ParamInt$new("minsplit", lower = 1,     upper = 10)
))

inst = TuningInstance$new(
  tsk("pima"),
  lrn("classif.rpart"),
  rsmp("holdout"),
  list(msr("classif.tpr"), msr("classif.fpr")),
  ps,
  term("evals", 10)
)

tuner = TunerRandomSearch$new()
tuner$tune(inst)

print(inst$archive())



    pareto_front = function(measures = NULL) {

      measure_ids = map_chr(measures, "id")

      if (is.null(measures)) {
        measures = self$measures
      } else {
        measures = lapply(measures, as_measure, task_type = self$task$task_type)
        # check that we are only using contained measures
        assert_choice(measure_ids, map_chr(self$measures, "id"))
     }
      # check that at leaste two measures are given
      assert_true(length(measure_ids) > 1)
      #assert_measure(measure, task = self$task, learner = self$learner)
      #if (is.na(measure$minimize))
      #  stopf("Measure '%s' has minimize = NA and hence cannot be tuned", measure$id)

      tab = self$bmr$aggregate(measures, ids = FALSE)
      y = tab[measure_ids]
      #if (allMissing(y))
      #  stopf("No non-missing performance value stored")

      minimize = map_chr(measures, "minimize")
      is_pareto = calculate_pareto_front(y, !minimize, return_data = FALSE)

      result = tab$resample_result[is_pareto]
    }
