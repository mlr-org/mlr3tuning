devtools::load_all()

set.seed(123)
lg$set_threshold("info")

# return vector of valid test functions
valid_math_functions()

# init new tuning instance for test function
inst = MathTuningInstance$new(
  "Branin",
  term("evals", n_evals = 10)
)

# define tuner and evaluate
tuner = TunerGridSearch$new()
tuner$tune(inst)
print(inst$archive())


