#Empirical estimate of R0 from a single infector

library(covidhm)
library(tidyverse)


#Set number of replicate simulations
nreps <- 100

#Generate empty vector
R0 <- rep(NA,nreps)

net <- format_network(am_list[[1]])

#Run models for 21 days from a single starting infector
for(i in 1:nreps)
{
  x <- outbreak_model(net = net,
                      num.initial.cases = 1,
                      prop.ascertain = 0,
                      cap_max_days = 21,
                      R = 0.8, presymrate = 0.2, delay_shape = 1,
                      delay_scale = 1.4, prop.asym = 0.4,
                      quarantine = FALSE, isolation = FALSE,
                      tracing = FALSE, secondary = FALSE,
                      outside = 0, sensitivity = "high",
                      testing = "none", cap_max_tests = NULL,
                      weekly = FALSE, s = NULL)

  #Calculate how many infections from the starting node (who has exposure time of 0)
  R0[i] <- sum(x$infector == filter(x,exposure == 0)$caseid,na.rm = T)

  #Get the exposure time of all individuals infected from the starting infector
  if(i == 1){
    gentime <- filter(x,infector == filter(x,exposure == 0)$caseid)$exposure
    } else
    {
      gentime <- c(gentime,filter(x,infector == filter(x,exposure == 0)$caseid)$exposure)
    }
}


#Estimate R0
mean(R0)

#Estimate generation time
median(gentime)
mean(gentime)

