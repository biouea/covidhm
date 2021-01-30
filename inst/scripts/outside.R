###########################################
#SIMULATE OUTSIDE INFECTION
###########################################
rm(list=ls())

library(covidhm)
library(dplyr)
library(purrr)
library(tidyr)

source("inst/scripts/default_params.R")
future::plan("multiprocess")

# Simulate scenarios ------------------------------------------------------
intervention = c("nothing","primary_quarantine","secondary_quarantine")
outside = c(0.0001,0.001,0.005,0.01)
scenarios <- expand_grid(intervention,outside)

res <- scenarios %>%
  mutate(results = map2(intervention,outside, ~ scenario_sim2(scenario = .x,
                                                           outside = .y,
                                                           distancing = 0,
                                                           testing = FALSE)))

saveRDS(res,"data-raw/outside.rds")

