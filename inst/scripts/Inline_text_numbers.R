
sce <- read_rds("data-raw/scenarios.rds")
net <- read_rds("data-raw/network.rds")
dis <- read_rds("data-raw/distancing.rds")
dis2 <- read_rds("data-raw/distancing2.rds")
out <- read_rds("data-raw/outside.rds")
tes <- read_rds("data-raw/testing.rds")

#Scenarios
sce %>%
  unnest(cols = "results") %>%
  filter(week == 9) %>%
  group_by(intervention) %>%
  summarise(medcases = median(cumcases)/468,
            Ucases = quantile(cumcases,0.95)/468,
            Lcases = quantile(cumcases, 0.05)/468)

sce %>%
  unnest(cols = "results") %>%
  filter(week == 3) %>%
  group_by(intervention) %>%
  summarise(medcases = median(weekly_quarantine)/468,
            Ucases = quantile(weekly_quarantine, 0.95)/468,
            Lcases = quantile(weekly_quarantine, 0.05)/468)

sce %>%
  unnest(cols = "results") %>%
  filter(week == 9) %>%
  group_by(intervention) %>%
  summarise(medcases = median(weekly_quarantine)/468,
            Ucases = quantile(weekly_quarantine, 0.95)/468,
            Lcases = quantile(weekly_quarantine, 0.05)/468)


#testing
tes %>%
  unnest(cols = "results") %>%
  filter(week == 9) %>%
  group_by(intervention,tests) %>%
  summarise(medcases = median(cumcases)/468,
            Ucases = quantile(cumcases,0.95)/468,
            Lcases = quantile(cumcases, 0.05)/468)

tes %>% unnest(cols = "results") %>%
  filter(week == 3) %>%
  group_by(intervention,tests) %>%
  summarise(medcases = median(weekly_tests)/468,
            Ucases = quantile(weekly_tests,0.95)/468,
            Lcases = quantile(weekly_tests, 0.05)/468)


#distancing
dis %>%
unnest(cols = "results") %>%
  filter(week == 9) %>%
  group_by(intervention,dist) %>%
  summarise(medcases = median(cumcases)/468,
            Ucases = quantile(cumcases,0.95)/468,
            Lcases = quantile(cumcases, 0.05)/468)

dis %>%
  unnest(cols = "results") %>%
  filter(week == 3) %>%
  group_by(intervention,dist) %>%
  summarise(medcases = median(weekly_quarantine)/468,
            Ucases = quantile(weekly_quarantine,0.95)/468,
            Lcases = quantile(weekly_quarantine, 0.05)/468)
