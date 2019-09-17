
# Modified measure class, now responsible for evaluating the mathematical
# test function
MeasureMathTestFun = R6Class("MeasureMathTestfun",
  inherit = MeasureRegr,
  public = list(
    test_fun = NULL,

    initialize = function(test_fun) {
      self$test_fun = test_fun

      # init superclass
      super$initialize(
        id = "func_value",
        range = c(-Inf, Inf),
        minimize = TRUE
      )
    },

    # evaluate test function (ignore "prediction" argument) based on
    # hyperparameter values
    score_internal = function(prediction, learner, ...) {
      v = learner$param_set$values
      v$budget = NULL
      globalOptTests::goTest(unlist(v), self$test_fun)
    }
  )
)

# Modified learner class, now responsible for defining the domain of the math
# function
# tfun: character name of valid mathematical function
# budget is currently unused
# TODO: budget reduces noise
LearnerMathTestFun = R6Class(
  "LearnerMathTestFun",
  inherit = LearnerRegr,

  public = list(
    initialize = function(test_fun) {
      # get the upper and lower boundaries of each dimension of the test func
      b = globalOptTests::getDefaultBounds(test_fun)
      # get the domain dimensions of the test function
      k = globalOptTests::getProblemDimen(test_fun)
      # create a new parameter set based on the received func properties
      ps = Map(
        function(i) ParamDbl$new(paste0("x", i), lower = b$lower[i], upper = b$upper[i]),
        seq_len(k)
      )
      ps = ParamSet$new(ps)
      # add budget parameter; TODO: do something with it
      ps$add(ParamDbl$new("budget", lower = 0, upper = 100))

      # init superclass
      super$initialize(
        id = "regr.mtf",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response"),
        param_set = ps,
        properties = c("missings")
      )
    },

    train_internal = function(task) {
      n = length(task$row_roles$use)
      self$param_set$values$budget = n / length(task$row_ids)
      return(list())
    },

    predict_internal = function(task) {
      response = rep(99, task$nrow)
      PredictionRegr$new(task, response = response)
    }
  )
)

# TODO: multi crit with ZDT and DTLZ family
# A tuning instance used for running tuners on mathematical test functions
# function_name: character of a valid mathematical function
# terminator: mlr3tuning terminator object responsible for termination of a 
# tuning run
MathTuningInstance = R6Class(
  "MathTuningInstance",
  inherit = TuningInstance,

  public = list(
    initialize = function(function_name, terminator) {
      # init class instances for function domain and evaluation
      m_learner = LearnerMathTestFun$new(function_name)
      m_measure = MeasureMathTestFun$new(function_name)

      # init TuningInstance
      inst = super$initialize(
        tsk("boston_housing"), 
        m_learner,
        rsmp("holdout"), 
        m_measure, 
        m_learner$param_set, 
        terminator
      )
    }
  )
)

# a vector of valid function names for MathTuningInstance
valid_math_functions = function() {
  c(
    "Ackleys", "AluffiPentini", "BeckerLago","Bohachevsky1", "Bohachevsky2", 
    "Branin","Camel3", "Camel6", "CosMix2", "CosMix4","DekkersAarts", "Easom", 
    "EMichalewicz","Expo", "GoldPrice", "Griewank", "Gulf","Hartman3", "Hartman6",
    "Hosaki", "Kowalik","LM1", "LM2n10", "LM2n5", "McCormic","MeyerRoth", 
    "MieleCantrell", "Modlangerman","ModRosenbrock", "MultiGauss", "Neumaier2",
    "Neumaier3", "Paviani", "Periodic","PowellQ", "PriceTransistor", "Rastrigin",
    "Rosenbrock", "Salomon", "Schaffer1","Schaffer2", "Schubert", "Schwefel",
    "Shekel10", "Shekel5", "Shekel7","Shekelfox5", "Wood", "Zeldasine10",
    "Zeldasine20"
  )
}
