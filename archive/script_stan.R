#
# script for running stan
#

library(adegenet)
library(rstan)
library(shinystan)


Na_data <- read.csv(here::here("data input", "N2_data.csv"), header = TRUE)
Nb_data <- read.csv(here::here("data input", "N4_data.csv"), header = TRUE)

data_list <-
  list(N_studies_a = length(unique(Na_data$study_id)),
       len_a = nrow(Na_data),
       X_a = structure(c(Na_data$N_dead,
                         Na_data$N_fed),
                       .Dim = c(nrow(Na_data), 2)),
       Na = Na_data$N_total,
       time_a = Na_data$months_since_IRS,
       studyid_a = Na_data$study_id,
       
       N_studies_b = length(unique(Nb_data$study_id)),
       len_b = nrow(Nb_data),
       X_b = structure(c(Nb_data$Nsf + Nb_data$Nsfe,  # success fed
                         Nb_data$Ndf + Nb_data$Ndfe,  # dead fed 
                         Nb_data$Nsn + Nb_data$Nsne,  # success unfed
                         Nb_data$Ndn + Nb_data$Ndne), # dead unfed
                       .Dim = c(nrow(Nb_data), 4)),
       Nb = Nb_data$N_total,
       time_b = Nb_data$months_since_IRS,
       studyid_b = Nb_data$study_id)


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_base <- stan(
  # file = "script.stan",
  file = "stan_simplex.stan",

  data = data_list, 
  warmup = 100,#000,
  control = list(adapt_delta = 0.9,
                 max_treedepth = 20),
  iter = 200,#000,
  chains = 1) # 1 chain testing; 4 chains proper

launch_shinystan(stan_base)
base <- extract(stan_base)
