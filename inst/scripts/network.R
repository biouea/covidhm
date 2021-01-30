###########################################
#SIMULATE INFECTION ON NULL NETWORKS
###########################################
rm(list=ls())

library(covidhm)
library(dplyr)
library(tidyr)
library(purrr)

source("inst/scripts/default_params.R")
future::plan("multiprocess")

# Simulate scenarios ------------------------------------------------------
intervention = c("nothing","primary_quarantine","secondary_quarantine")
null = c("edge","deg","latt","clust")
scenarios <- expand_grid(intervention,null)

res <- scenarios %>%
  mutate(results = map2(intervention,null, ~ scenario_sim2(scenario = .x,
                                                           null.net = .y,
                                                           outside = 0.001,
                                                           distancing = 0,
                                                           testing = FALSE)))

saveRDS(res,"data-raw/network.rds")
