#######################################################
#SIMULATE CONTACT TRACING SCENARIOS WITH DENSE MATRICES
#######################################################

rm(list=ls())

library(covidhm)
library(dplyr)
library(purrr)

load("data-raw/am_7 and am_16.RData")
source("inst/scripts/default_params.R")

scenario_sim2 <- partial(scenario_sim, n.sim = nreps, num.initial.cases = 1,
                         prop.asym=0.4, prop.ascertain = 0.9, cap_max_days = 69,
                         delay_shape = 1, delay_scale = 1.4,R = 0.8, presymrate = 0.2, testing = FALSE,
                         distancing = 0,outside = 0.001)

future::plan("multiprocess")

# Simulate scenarios ------------------------------------------------------

scenarios <- tibble(intervention = c("nothing","isolation", "primary_quarantine","secondary_quarantine"))

res <- scenarios %>%
  mutate(results = map(intervention, ~ scenario_sim2(scenario = .,
                                                     net = am_7)))

saveRDS(res, file = "data-raw/scenarios7.rds")

res <- scenarios %>%
  mutate(results = map(intervention, ~ scenario_sim2(scenario = .,
                                                     net = am_16)))

saveRDS(res, file = "data-raw/scenarios16.rds")
