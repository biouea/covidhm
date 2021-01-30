###########################################
#SIMULATE CONTACT TRACING SCENARIOS
###########################################
rm(list=ls())

library(covidhm)
library(dplyr)
library(purrr)

source("inst/scripts/default_params.R")
future::plan("multiprocess")

# Simulate scenarios ------------------------------------------------------

scenarios <- tibble(intervention = c("nothing","isolation", "primary_quarantine","secondary_quarantine"))

res <- scenarios %>%
  mutate(results = map(intervention, ~ scenario_sim2(scenario = .,
                                                 outside = 0.001,
                                                 distancing = 0,
                                                 testing = FALSE)))

saveRDS(res, file = "data-raw/scenarios.rds")
