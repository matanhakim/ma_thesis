# Run every unit test: Rscript tests/testthat.R
library(testthat)
targets::tar_source("R")
test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
