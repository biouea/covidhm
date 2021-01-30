# Using a real-world network to model localised COVID-19 control strategies


This repository contains code for simulating COVID-19 dynamics in a range of scenarios across a real-world social network. The epidemic model is based conceptually on a branching-process model of contact-tracing and COVID-19, which can be accessed [here](https://github.com/cmmid/ringbp).



## Abstract
Case isolation and contact tracing can contribute to the control of COVID-19 outbreaks. However, it remains unclear how real-world social networks could influence the effectiveness and efficiency of such approaches. To address this issue, we simulated control strategies for SARS-CoV-2 transmission in a real-world social network generated from high resolution GPS data that was gathered in the course of a citizen-science experiment3,4. We found that tracing contacts-of-contacts reduced the size of simulated outbreaks more than tracing of only contacts, but this strategy also resulted in almost half of the local population being quarantined at a single point in time. Testing and releasing non-infectious individuals from quarantine led to increases in outbreak size, suggesting that contact tracing and quarantine might be most effective as a ‘local lockdown’ strategy when contact rates are high. Finally, we estimated that combining physical distancing with contact tracing could enable epidemic control while reducing the number of quarantined individuals. Our findings suggest that targeted tracing and quarantine strategies would be most efficient when combined with other control measures such as physical distancing.

## Usage

### Set up

Install the analysis by downloading the repository. Navigate to the parent directory, then run: 

```r
devtools::install("covidhm-master",dependencies = TRUE) #or whatever your folder is called
```


### Run a single scenario and plot a network

Run a single instance of the outbreak model over 20 days and overlay the infection data onto a network. See `?plot_network` for details of parameters

```r
library(covidhm)

#Load association matrices
load("data-raw/am_list.RData")

#First item in the list is data across all days
m <- am_list[[1]]

#Plot network
plot_network(
am = m,
day = 20,
num.initial.cases = 1,
prop.asym = 0.4,
delay_shape =  1,
delay_scale = 1.4,
prop.ascertain = 0.9,
presymrate = 0.2,
R = 0.8,
outside = 0.001,
testing = FALSE,
s = 333,
isolation = FALSE,
secondary = FALSE,
tracing = FALSE,
quarantine = FALSE)

```



### Run a single scenario multiple times

Run a single scenario for 10 simulations. Use `?scenario_sim` for an explanation of parameters.

```r
library(covidhm)
library(ggplot2)

res <- scenario_sim(net = m, n.sim = 10, num.initial.cases = 1,prop.asym=0.4,
                             prop.ascertain = 0.9, cap_max_days = 70,
                             delay_shape = 1, delay_scale = 1.4, R = 0.8, presymrate = 0.2, scenario = "nothing",
                             testing = FALSE, outside = 0.001, distancing = 0)

# Plot of raw cumulative cases
ggplot(data=res, aes(x=week, y=cumcases,col = sim)) +
geom_line(show.legend = FALSE, alpha=0.6, aes(group = sim)) +
scale_y_continuous(name="Weekly number of cases") +
  theme_bw()

```

### Run the full analysis

Run the analyses in the terminal with the following commands (NB - these take several hours):

```bash

Rscript inst/scripts/scenarios.R
Rscript inst/scripts/network.R
Rscript inst/scripts/distancing.R
Rscript inst/scripts/outside.R
Rscript inst/scripts/testing.R
Rscript inst/scripts/sensitivity_testing.R

```

### Generate figures

Render figures with the following:

```bash
Rscript inst/scripts/figures.R

```
