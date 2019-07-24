#######################################################
###
###
### Mock data for statistical model

Na_data <- read.csv("H:/Ellie/Evidence synthesis/Evidence synthesis data_aggregated_N2.csv", header = TRUE)
Nb_data <- read.csv("H:/Ellie/Evidence synthesis/Evidence synthesis data_aggregated_N4.csv", header = TRUE)

data_list1 = list(N_studies_a = length(unique(Na_data$study_count)),
                  len_a = nrow(Na_data),
                  X_a = structure(c(Na_data$Ntotaldied_IRS,Na_data$Nbloodfed_IRS),
                                  .Dim = c(nrow(Na_data),2)),
                  Na = Na_data$Ntotalfemalemosq_IRS,
                  time_a = Na_data$Months_since_IRS,
                  
                  N_studies_b = length(unique(Nb_data$study_count)),
                  len_b = nrow(Nb_data),
                  X_b = structure(c(Nb_data$Nsf + Nb_data$Nsfe, ### Success_fed
                                    Nb_data$Ndf + Nb_data$Ndfe, ### Dead_fed 
                                    Nb_data$Nsn + Nb_data$Nsne, ### Success_unfed
                                    Nb_data$Ndn + Nb_data$Ndne),### Dead_unfed
                                  .Dim = c(nrow(Nb_data),4)),
                  Nb = Nb_data$Ntotalfemalemosq_IRS,
                  time_b = Nb_data$Months_since_IRS) ## added time as think will be needed


library(adegenet)
library(rstan)
library(shinystan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_base <- stan(file = "[INSERT YOUR MODEL FILE HERE].rstan",
                  data = data_list1, 
                  warmup = 1000,
                  control = list(adapt_delta = 0.8,
                                 max_treedepth = 20),
                  iter = 2000,
                  chains = 1) ## Keep chains 1 until happy then 4

launch_shinystan(stan_base)
base <- extract(stan_base)
