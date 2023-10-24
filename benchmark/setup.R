dir.create("benchmark/snapshot-2023-10-24")
renv::init("benchmark/snapshot-2023-10-24", bare = TRUE)
renv::load("benchmark/snapshot-2023-10-24")
renv::settings$snapshot.type("all")
renv::install(c("microbenchmark", "mlr-org/rush", "mlr-org/mlr3@hotstart_time", "mlr-org/bbotk@rush", "mlr-org/mlr3tuning@rush", "mlr3tuningspaces"))
renv::snapshot()
# renv::restore()


