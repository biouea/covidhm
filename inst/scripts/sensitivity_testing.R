###########################################
#SENSITIVITY TESTING OF EPIDEMIC MODEL
###########################################

library(covidhm)
library(tidyr)
library(tibble)
library(dplyr)
library(purrr)

future::plan("multiprocess")

load("data-raw/am_list.RData")
source("inst/scripts/default_params.R")
scenario_sim2 <- partial(scenario_sim, net = am_list[[1]], n.sim = nreps, cap_max_days = 69,
                         testing = FALSE, distancing = 0, outside = 0.001)


# Set up scenarios --------------------------------------------------------

scenarios <- list(
  delay_group = tibble(
    delay = c("Medium", "Short"),
    delay_shape = c(1.651524, 1),
    delay_scale = c(4.287786, 1.4)),
  tibble(presymrate = c(0.2,0.4)),
  tibble(prop.asym = c(0.2, 0.4)),
  tibble(num.initial.cases = c(1, 5)),
  tibble(R = c(0.5,0.8,2))) %>%
  map(~ expand_grid(.,prop.ascertain = c(0.3, 0.6, 0.9),
                    scenario = c("primary_quarantine", "secondary_quarantine"))) %>%
  bind_rows() %>%
  mutate(presymrate = replace_na(presymrate,0.2),
         prop.asym = replace_na(prop.asym,0.4),
         num.initial.cases = replace_na(num.initial.cases,1),
         R = replace_na(R,0.8),
         delay = replace_na(delay,"Short"),
         delay_shape = replace_na(delay_shape,1),
         delay_scale = replace_na(delay_scale,1.4))%>%
  distinct()


# Parameter sweep for sensitivity testing ---------------------------------

res <- scenarios %>%
  mutate(results = pmap(list(presymrate,
                             prop.asym,
                             num.initial.cases,
                             R,
                             delay_shape,
                             delay_scale,
                             scenario,
                             prop.ascertain),
                        ~ scenario_sim2(presymrate = ..1,
                                        prop.asym = ..2,
                                        num.initial.cases = ..3,
                                        R = ..4,
                                        delay_shape = ..5,
                                        delay_scale = ..6,
                                        scenario = ..7,
                                        prop.ascertain = ..8)))

saveRDS(res, file = "data-raw/sensitivity.rds")
