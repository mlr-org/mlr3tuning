library(batchtools)

# reg = makeRegistry(file.dir = "/gscratch/mbecke16/benchmark-mrl3tuning", conf.file = "benchmark/batchtools.conf.R", packages = "renv", seed = 1)
# reg = loadRegistry(file.dir = "/gscratch/mbecke16/benchmark-mlr3tuning", writeable = TRUE)
# unlink("/gscratch/mbecke16/benchmark-mlr3tuning", recursive = TRUE)

source("benchmark/runner_random_search.R")

batchMap(
  fun = runner_random_search,
  renv_project = list("benchmark/snapshot-2023-10-24"),
  times = list(1)
)

submitJobs(resources = list(walltime = 60 * 60 * 24))

getStatus()
