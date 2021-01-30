library(tidyverse)
library(cowplot)
library(covidhm)

# Load in data ------------------------------------------------------------

sce <- read_rds("data-raw/scenarios.rds")
net <- read_rds("data-raw/network.rds")
sen <- read_rds("data-raw/sensitivity.rds") %>%
  unnest(cols = "results")
dis <- read_rds("data-raw/distancing.rds")
dis2 <- read_rds("data-raw/distancing2.rds")
out <- read_rds("data-raw/outside.rds")
tes <- read_rds("data-raw/testing.rds")
sce7 <- read_rds("data-raw/scenarios7.rds")
sce16 <- read_rds("data-raw/scenarios16.rds")


load("data-raw/am_list.RData")



# Figure 1 - network examples ---------------------------------------------

m <- am_list[[1]]

plot_network2 <- purrr::partial(plot_network,
                                am = m,
                                num.initial.cases = 1,
                                prop.asym = 0.4,
                                delay_shape =  1,
                                delay_scale = 1.4,
                                prop.ascertain = 0.9,
                                presymrate = 0.2,
                                R = 0.8,
                                outside = 0,
                                testing = FALSE,
                                s = 333)

pdf("inst/plots/Figure_1.pdf",
    width = 8,
    height = 16)

layout(matrix(c(1,1,1,1,1,1,
                2,2,3,3,4,4,
                5,5,5,5,5,5,
                6,6,6,7,7,7,
                8,8,8,8,8,8,
                9,9,9,10,10,10,
                11,11,11,11,11,11,
                12,12,12,13,13,13),
              8,6,byrow = TRUE,
),widths = c(1,1,2,2,1,1),
heights = c(0.1,1,0.1,1,0.1,1,0.1,1))

par(mar = c(1,0,0,0))

plot.new()
text(0.5,0.5,"Starting network",cex = 1.8)

plot.new()
legend("center",
       pch = c(19,19,19),
       col = c("darkgrey",
               "indianred1",
               "pink"),
       legend = c("Not infected",
                  "Infected",
                  "Recovered"),
       bty = "n",
       cex = 1.8)

par(mar = c(1,0,0,0))

plot_network2(day = 1,
              isolation = TRUE,
              quarantine = TRUE,
              tracing = TRUE,
              secondary = TRUE)
text(0.2,2,"A",cex = 1.6)

plot.new()
legend("left",
       pch = c(NA,NA,0),
       lty = c(1,1,NA),
       col = c("deepskyblue1",
               "indianred1",
               "black"),
       legend = c("Contacts",
                  "Infections",
                  "Isolated/\nquarantined"),
       bty = "n",
       cex = 1.8)


plot.new()
text(0.5,0.5,"Day 10",cex = 1.8)

par(mar = c(1,0,0,0))

plot_network2(day = 10,
              isolation = FALSE,
              quarantine = FALSE,
              tracing = FALSE,
              secondary = FALSE)
text(0.2,2,"B",cex = 1.6)

par(mar = c(1,0,0,0))

plot_network2(day = 10,
              isolation = TRUE,
              quarantine = TRUE,
              tracing = TRUE,
              secondary = TRUE)
text(0.2,2,"E",cex = 1.6)


plot.new()
text(0.5,0.5,"Day 20",cex = 1.8)

par(mar = c(1,0,0,0))

plot_network2(day = 20,
              isolation = FALSE,
              quarantine = FALSE,
              tracing = FALSE,
              secondary = FALSE)
text(0.2,2,"C",cex = 1.6)

par(mar = c(1,0,0,0))

plot_network2(day = 20,
              isolation = TRUE,
              quarantine = TRUE,
              tracing = TRUE,
              secondary = TRUE)
text(0.2,2,"F",cex = 1.6)


plot.new()
text(0.5,0.5,"Day 70",cex = 1.8)

par(mar = c(1,0,0,0))

plot_network2(day = 70,
              isolation = FALSE,
              quarantine = FALSE,
              tracing = FALSE,
              secondary = FALSE)
text(0.2,2,"D",cex = 1.6)

par(mar = c(1,0,0,0))

plot_network2(day = 70,
              isolation = TRUE,
              quarantine = TRUE,
              tracing = TRUE,
              secondary = TRUE)
text(0.2,2,"G",cex = 1.6)

dev.off()




# Figure 2 - intervention scenarios ---------------------------------------

sce_figa <- sce  %>%
  mutate(intervention = recode(intervention,
                               nothing = "No control",
                               isolation = "Case isolation",
                               primary_quarantine = "Primary tracing",
                               secondary_quarantine = "Secondary tracing")) %>%
  mutate(intervention = factor(intervention,
                               levels = c("No control",
                                          "Case isolation",
                                          "Primary tracing",
                                          "Secondary tracing"))) %>%
  unnest(cols = "results") %>%
  case_plot(nrow = 1)+
  theme(legend.position = "top")


plot_network2 <- purrr::partial(plot_network,
                                am = am_list[[1]],
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
                                s = 333)

sce_figb <- function(){

  par(mar = c(1,0,0,0))
  plot_network2(isolation = FALSE,
                quarantine = FALSE,
                tracing = FALSE,
                secondary = FALSE)
}

sce_figc <- function(){

  par(mar = c(1,0,0,0))
  plot_network2(isolation = TRUE,
                quarantine = FALSE,
                tracing = FALSE,
                secondary = FALSE)
}

sce_figd <- function(){

  par(mar = c(1,0,0,0))
  plot_network2(isolation = TRUE,
                quarantine = TRUE,
                tracing = TRUE,
                secondary = FALSE)
}

sce_fige <- function(){

  par(mar = c(1,0,0,0))
  plot_network2(isolation = TRUE,
                quarantine = TRUE,
                tracing = TRUE,
                secondary = TRUE)
}


sce_fig <- plot_grid(sce_figa,
                     plot_grid(NULL,sce_figb,sce_figc,sce_figd,sce_fige,nrow = 1,
                               rel_widths = c(0.25,1,1,1,1)),
                     nrow = 2,
                     rel_heights = c(1,0.7),
                     labels = "AUTO")


pdf("inst/plots/Figure_2.pdf",
    width = 12,
    height = 8)
sce_fig
dev.off()






# Figure 3 - test and release and distancing ---------------------------------------------

tes_fig <- tes %>%
  mutate(tests = paste(tests,"tests per day")) %>%
  bind_rows(sce %>%
              filter(intervention %in% c("primary_quarantine",
                                         "secondary_quarantine")) %>%
              mutate(tests = "No testing")) %>%
  mutate(tests = factor(tests, levels = c("No testing",
                                          "5 tests per day",
                                          "25 tests per day",
                                          "50 tests per day")),
         intervention = recode(intervention,
                               primary_quarantine = "Primary tracing",
                               secondary_quarantine = "Secondary tracing")) %>%
  unnest(cols = "results") %>%
  case_plot(facet = "grid",gridvar = "tests",testing = TRUE)+
  theme(legend.position = "top")



dis_fig <- dis  %>%
  mutate(dist = paste0(dist*100,"% reduction"),
         intervention = recode(intervention,
                               nothing = "No control",
                               isolation = "Case isolation",
                               primary_quarantine = "Primary tracing",
                               secondary_quarantine = "Secondary tracing")) %>%
  unnest(cols = "results") %>%
  case_plot(facet = "grid", gridvar = "dist")+
  theme(legend.position = "none")

fig3 <- plot_grid(tes_fig,dis_fig,nrow = 2,labels = "AUTO",rel_heights = c(0.45,0.55))

pdf("inst/plots/Figure_3.pdf",
    width = 12,
    height = 16)
fig3
dev.off()




# Figure 4 - null networks ------------------------------------------------

net_figa <- net  %>%
  mutate(null = recode(null, latt = "Lattice null",
                       deg = "Degree null",
                       edge = "Edge null",
                       clust = "Cluster null")) %>%
  mutate(intervention = recode(intervention,
                               nothing = "No control",
                               isolation = "Case isolation",
                               primary_quarantine = "Primary tracing",
                               secondary_quarantine = "Secondary tracing")) %>%
  mutate(intervention = factor(intervention,
                               levels = c("No control",
                                          "Case isolation",
                                          "Primary tracing",
                                          "Secondary tracing")),
         null = factor(null, levels = c("Edge null",
                                        "Degree null",
                                        "Lattice null",
                                        "Cluster null"))) %>%
  unnest(cols = results) %>%
  case_plot(facet = "grid",gridvar = "null") +
  theme(legend.position = "top")


plot_network2 <- purrr::partial(plot_network,
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
                                isolation = TRUE,
                                quarantine = TRUE,
                                tracing = TRUE,
                                secondary = TRUE,
                                s = 12345)

net_exa <- function(){
  net1 <- network_null(am_list[[1]],returns = "matrix",null = "edge")
  par(mar = c(1,0,0,0))
  plot_network2(am = net1)
}

net_exb <- function(){
  net1 <- network_null(am_list[[1]],returns = "matrix",null = "deg")
  par(mar = c(1,0,0,0))
  plot_network2(am = net1)
}

net_exc <- function(){
  par(mar = c(1,0,0,0))
  net1 <- network_null(am_list[[1]],returns = "matrix",null = "latt")
  par(mar = c(1,0,0,0))
  plot_network2(am = net1)
}

net_exd <- function(){
  net1 <- network_null(am_list[[1]],returns = "matrix",null = "clust")
  par(mar = c(1,0,0,0))
  plot_network2(am = net1)
}


net_fig <- plot_grid(net_figa,
                     plot_grid(NULL,net_exa,net_exb,net_exc,net_exd,nrow = 1,
                               rel_widths = c(0.25,1,1,1,1)),
                     nrow = 2,
                     rel_heights = c(1,0.3),
                     labels = "AUTO")


pdf("inst/plots/Figure_4.pdf",
    width = 12,
    height = 12)
net_fig
dev.off()




# Figure S1 - network distance thresholds ---------------------------------

#Done by Josh Firth


# Figure S2 - network edge weight options ---------------------------------

#Done by Josh Firth






# Figure S3 - R0 ---------------------------------------------

dd <- sen %>%
  filter(delay == "Short",
         presymrate == 0.2,
         prop.asym == 0.4,
         num.initial.cases == 1) %>%
  mutate(prop.ascertain = recode(prop.ascertain,
                                        `0.3` = "30% traced",
                                        `0.6` = "60% traced",
                                        `0.9` = "90% traced"),
         scenario = recode(scenario,
                           primary_quarantine = "Primary tracing",
                           secondary_quarantine = "Secondary quarantine")) %>%
  rename(intervention = scenario)

  r_figa <- dd %>% filter(R == 0.5) %>%
  case_plot(facet = "grid",gridvar = "prop.ascertain")

legend <- get_legend(r_figa)
r_figa <- r_figa +
  theme(legend.position = "none")+
  ggtitle("R = 2")+
  ylim(c(0,350))

r_figb <- dd %>%
  filter(R == 0.8) %>%
  case_plot(facet = "grid",gridvar = "prop.ascertain")+
  theme(legend.position = "none")+
  ggtitle("R = 2.8")+
  ylim(c(0,350))

r_figc <- dd %>%
  filter(R == 2) %>%
  case_plot(facet = "grid",gridvar = "prop.ascertain")+
  theme(legend.position = "none")+
  ggtitle("R = 3.5")+
  ylim(c(0,350))


r_fig <- plot_grid(plot_grid(r_figa,r_figb, r_figc,nrow = 3), legend, nrow = 1, rel_widths = c(1,0.5))


#Write to pdf
pdf("inst/plots/EDF_3.pdf",
    width = 8,
    height = 16)

r_fig

dev.off()




# Figure S4 - asym, theta ---------------------------------------------
dd <- sen %>%
  filter(delay == "Short",
         presymrate == 0.2,
         R == 0.8,
         num.initial.cases == 1) %>%
  mutate(prop.ascertain = recode(prop.ascertain,
                                 `0.3` = "30% traced",
                                 `0.6` = "60% traced",
                                 `0.9` = "90% traced"),
         scenario = recode(scenario,
                           primary_quarantine = "Primary tracing",
                           secondary_quarantine = "Secondary quarantine")) %>%
  rename(intervention = scenario)

asym_figa <- dd %>%
  filter(prop.asym == 0.2) %>%
  case_plot(facet = "grid",gridvar = "prop.ascertain")

legend <- get_legend(asym_figa)
asym_figa <- asym_figa +
  theme(legend.position = "none")+
  ggtitle("Proportion asymptomatic = 0.2")+
  ylim(c(0,350))

asym_figb <- dd %>%
  filter(prop.asym == 0.4) %>%
  case_plot(facet = "grid",gridvar = "prop.ascertain")+
  theme(legend.position = "none")+
  ggtitle("Proportion asymptomatic = 0.4")+
  ylim(c(0,350))

asym_fig <- plot_grid(asym_figa,asym_figb, legend,rel_widths = c(1,1,0.5),nrow = 1)



#Theta
dd <- sen %>%
  filter(delay == "Short",
         R == 0.8,
         prop.asym == 0.4,
         num.initial.cases == 1) %>%
  mutate(prop.ascertain = recode(prop.ascertain,
                                 `0.3` = "30% traced",
                                 `0.6` = "60% traced",
                                 `0.9` = "90% traced"),
         scenario = recode(scenario,
                           primary_quarantine = "Primary tracing",
                           secondary_quarantine = "Secondary quarantine")) %>%
  rename(intervention = scenario)

theta_figa <- dd %>%
  filter(presymrate == 0.2) %>%
  case_plot(facet = "grid",gridvar = "prop.ascertain")

legend <- get_legend(theta_figa)
theta_figa <- theta_figa +
  theme(legend.position = "none")+
  ggtitle("Theta = 0.2")+
  ylim(c(0,350))

theta_figb <- dd %>%
  filter(presymrate == 0.4) %>%
  case_plot(facet = "grid",gridvar = "prop.ascertain")+
  theme(legend.position = "none")+
  ggtitle("Theta = 0.4")+
  ylim(c(0,350))


theta_fig <- plot_grid(theta_figa,theta_figb,rel_widths = c(1,1,0.5),nrow = 1)



#Write to pdf
pdf("inst/plots/EDF_4.pdf",
    width = 12,
    height = 10)

plot_grid(asym_fig,theta_fig,nrow = 2)

dev.off()



# Figure S5 - delay, starting number -------------------------------------------------------
dd <- sen %>%
  filter(R == 0.8,
         presymrate == 0.2,
         prop.asym == 0.4,
         num.initial.cases == 1) %>%
  mutate(prop.ascertain = recode(prop.ascertain,
                                 `0.3` = "30% traced",
                                 `0.6` = "60% traced",
                                 `0.9` = "90% traced"),
         scenario = recode(scenario,
                           primary_quarantine = "Primary tracing",
                           secondary_quarantine = "Secondary quarantine")) %>%
  rename(intervention = scenario)

delay_figa <- dd %>%
  filter(delay == "Short") %>%
  case_plot(facet = "grid",gridvar = "prop.ascertain")+
  ylim(c(0,350))

legend <- get_legend(delay_figa)
delay_figa <- delay_figa +
  theme(legend.position = "none")+
  ggtitle("Short delay")

delay_figb <- dd %>%
  filter(delay == "Medium") %>%
  case_plot(facet = "grid",gridvar = "prop.ascertain")+
  theme(legend.position = "none")+
  ggtitle("Medium delay")+
  ylim(c(0,350))


delay_fig <- plot_grid(delay_figa,delay_figb,legend, rel_widths = c(1,1,0.5),nrow = 1)


#initial cases
dd <- sen %>%
  filter(delay == "Short",
         presymrate == 0.2,
         prop.asym == 0.4,
         R == 0.8) %>%
  mutate(prop.ascertain = recode(prop.ascertain,
                                 `0.3` = "30% traced",
                                 `0.6` = "60% traced",
                                 `0.9` = "90% traced"),
         scenario = recode(scenario,
                           primary_quarantine = "Primary tracing",
                           secondary_quarantine = "Secondary quarantine")) %>%
  rename(intervention = scenario)

initial_case_figa <- dd %>%
  filter(num.initial.cases == 1) %>%
  case_plot(facet = "grid",gridvar = "prop.ascertain")+
  ylim(c(0,350))

legend <- get_legend(initial_case_figa)
initial_case_figa <- initial_case_figa +
  theme(legend.position = "none")+
  ggtitle("Initial cases = 1")

initial_case_figb <- dd %>%
  filter(num.initial.cases == 5) %>%
  case_plot(facet = "grid",gridvar = "prop.ascertain")+
  theme(legend.position = "none")+
  ggtitle("Initial cases = 5")+
  ylim(c(0,350))


initial_case_fig <- plot_grid(initial_case_figa,initial_case_figb, legend,rel_widths = c(1,1,0.5),nrow = 1)

#Write to pdf
pdf("inst/plots/EDF_5.pdf",
    width = 12,
    height = 10)

plot_grid(delay_fig,initial_case_fig,nrow = 2)

dev.off()




# Figure S6 - outside infection ------------------------------------------

out_fig <- out  %>%
  unnest(cols = "results") %>%
  mutate(outside = paste("outside =", outside)) %>%
  mutate(outside = recode(outside,"outside = 1e-04" = "outside = 0.0001")) %>%
  case_plot(facet = "grid", gridvar = "outside")


pdf("inst/plots/EDF_6.pdf",
    width = 12,
    height = 8)
out_fig
dev.off()





# Figure S7 - interventions with dense networks ---------------------------------------

sced_figa <- sce7  %>%
  unnest(cols = "results") %>%
mutate(intervention = recode(intervention,
                             nothing = "No control",
                             isolation = "Case isolation",
                             primary_quarantine = "Primary tracing",
                             secondary_quarantine = "Secondary tracing")) %>%
  mutate(intervention = factor(intervention,
                               levels = c("No control",
                                          "Case isolation",
                                          "Primary tracing",
                                          "Secondary tracing"))) %>%
  case_plot(nrow = 1)+
  theme(legend.position = "top")


sced_figb <- sce16  %>%
  unnest(cols = "results") %>%
  mutate(intervention = recode(intervention,
                               nothing = "No control",
                               isolation = "Case isolation",
                               primary_quarantine = "Primary tracing",
                               secondary_quarantine = "Secondary tracing")) %>%
  mutate(intervention = factor(intervention,
                               levels = c("No control",
                                          "Case isolation",
                                          "Primary tracing",
                                          "Secondary tracing"))) %>%
  case_plot(nrow = 1)+
  theme(legend.position = "top")

#Write to pdf
pdf("inst/plots/EDF_7.pdf",
    width = 12,
    height = 10)

plot_grid(sced_figa,sced_figb,nrow = 2,labels = "AUTO")

dev.off()





# Figure S8 - distancing with advanced method ----------------------------


dis2_fig <- dis2  %>%
  mutate(dist = paste0(dist*100,"% reduction"),
         intervention = recode(intervention,
                               nothing = "No control",
                               isolation = "Case isolation",
                               primary_quarantine = "Primary tracing",
                               secondary_quarantine = "Secondary tracing")) %>%
  unnest(cols = "results") %>%
  case_plot(facet = "grid", gridvar = "dist")+
  theme(legend.position = "none")

pdf("inst/plots/EDF_8.pdf",
    width =12,
    height = 8)
dis2_fig
dev.off()


# Figure S9 - null network examples ---------------------------------------

#Done by Josh Firth


# Figure S10 - distancing examples -----------------------------------------

#Done by Josh Firth

