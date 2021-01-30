###########################################
#SIMULATE EPIDIMIC WITH SOCIAL DISTANCING - main method
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
dist = c(0,0.3,0.6,0.9)
scenarios <- expand_grid(intervention,dist)

res <- scenarios %>%
  mutate(results = map2(intervention,dist, ~ scenario_sim2(scenario = .x,
                                                           distancing = .y,
                                                           outside = 0.001,
                                                           dist_func = dist_no_reall,
                                                           testing = FALSE)))

saveRDS(res,"data-raw/distancing.rds")


