library(mlr3learners)

task = tsk("iris")
learner = lrn("classif.svm", type = "C-classification")
resampling = rsmp("holdout")
measure = msr("classif.ce")
terminator = trm("none")

learner$param_set$values$kernel = to_tune(c("polynomial", "radial"))
learner$param_set$values$degree = to_tune(1, 3)

#solution 1
foo = learner$param_set$get_tune_pair()
learner$param_set = foo$param_set #ohne tune token / das könnte inplace passieren
search_space = foo$search_space #das was mir param_set$tune_ps() geben würde

#solutin 2
learner$convert_for_tuning() # param_vals tune tokens löschen und in param_set umwandeln
search_space = learner$param_set$
# geht das hier schlecht, wenn ich param$vals

instance = TuningInstanceSingleCrit$new(
  task = task,
  learner = learner,
  resampling = resampling,
  measure = measure,
  search_space = search_space,
  terminator = terminator
)

tuner = tnr("grid_search", resolution = 1)
tuner$optimize(instance)
