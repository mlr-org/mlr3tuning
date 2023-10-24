cluster.functions = batchtools::makeClusterFunctionsSlurm("/home/mbecke16/slurm_wyoming.tmpl", array.jobs = FALSE)
default.resources = list(walltime = 28800L, memory = 4000L, ntasks = 1L, ncpus = 32L, nodes = 1L, clusters = "teton")
max.concurrent.jobs = 4000
