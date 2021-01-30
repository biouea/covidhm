###########################################
#SIMULATE TESTING
###########################################
rm(list=ls())

library(covidhm)
library(dplyr)
library(purrr)
library(tidyr)

source("inst/scripts/default_params.R")
future::plan("multiprocess")

# Simulate scenarios ------------------------------------------------------
intervention = c("primary_quarantine","secondary_quarantine")
tests = c(5,25,50)
scenarios <- expand_grid(intervention,tests)

res <- scenarios %>%
  mutate(results = map2(intervention,tests, ~ scenario_sim2(scenario = .x,
                                                           cap_max_tests = .y,
                                                           outside = 0.001,
                                                           testing = TRUE,
                                                           distancing = 0)))

saveRDS(res,"data-raw/testing.rds")
