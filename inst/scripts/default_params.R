library(covidhm)

# Load Haselmere network --------------------------------------------------

load("data-raw/am_list.RData")

# Set number of replicates ------------------------------------------------

nreps <- 2

# Set up partial function -------------------------------------------------

scenario_sim2 <- partial(scenario_sim, net = am_list[[1]], n.sim = nreps, num.initial.cases = 1,
                         prop.asym=0.4, prop.ascertain = 0.9, cap_max_days = 69,
                         delay_shape = 1, delay_scale = 1.4,R = 0.8, presymrate = 0.2)
