
#
# script for running stan
#


library(adegenet)
library(rstan)
library(shinystan)

Na_data <- read.csv(here::here("data input", "N2_data.csv"), header = TRUE)
Nb_data <- read.csv(here::here("data input", "N4_data.csv"), header = TRUE)


data_list <-
  list(N_groups = 4,
       
       # aggregate
       N_agg_exp = nrow(Na_data),
       k1_agg = Na_data$N_dead,                   # dead 
       k2_agg = Na_data$N_fed,                    # fed
       N_trials_agg = Na_data$N_total,
       time_agg = Na_data$months_since_IRS,
       study_idx_agg = Na_data$study_id,
       
       # individual
       N_indiv_exp = nrow(Nb_data),
       N_responses_indiv =
         structure(c(Nb_data$Nsf + Nb_data$Nsfe,  # success fed
                     Nb_data$Ndf + Nb_data$Ndfe,  # dead fed 
                     Nb_data$Nsn + Nb_data$Nsne,  # success unfed
                     Nb_data$Ndn + Nb_data$Ndne), # dead unfed
                   .Dim = c(nrow(Nb_data), 4)),
       time_indiv = Nb_data$months_since_IRS)

data_list <- c(data_list,
               list(
                 N_studies =
                   length(unique(Na_data$study_id)) +
                   length(unique(Nb_data$study_id)),
                 study_idx_indiv = max(data_list$study_idx_agg) + Nb_data$study_id))
               

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

n_iter <- 200#000
n_warmup <- 100#000

## run model

stan_base <- stan(
  file = here::here("stan", "synthesis_betan_NG.stan"),

  data = data_list, 
  warmup = n_warmup,
  control = list(adapt_delta = 0.9,
                 max_treedepth = 20),
  iter = n_iter,
  chains = 1) # 1 chain testing; 4 chains proper

launch_shinystan(stan_base)
base <- extract(stan_base)   # iter x chain x parameter
