library(mlr3learners)
devtools::load_all("../bbotk")
devtools::load_all(".")
devtools::load_all("../mlr3hyperband")

library(future)
plan(multicore)

# define hyperparameter and budget parameter
search_space = ps(
  nrounds = p_int(lower = 512, upper = 4096, tags = "budget"),
  eta = p_dbl(lower = 0, upper = 1),
  booster = p_fct(levels = c("gbtree", "gblinear", "dart"))
)

# hyperparameter tuning on the pima indians diabetes data set
instance = tune(
  method = "asha",
  task = tsk("pima"),
  learner = lrn("classif.xgboost", eval_metric = "logloss"),
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.ce"),
  search_space = search_space,
  term_evals = 100,
  allow_hotstart = TRUE,
  eta = 2
)

# plot

data = instance$archive$data
set(data, j = "time", value = data$timestamp - instance$archive$start_time) 

library(gganimate)

ggplot(instance$archive$data, aes(rung)) +
  geom_histogram() + 
  transition_time(timestamp)
