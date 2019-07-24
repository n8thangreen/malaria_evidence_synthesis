
library(adegenet)
library(rstan)
library(shinystan)

Nb_data <- read.csv(here::here("code", "Nb_data.csv"), header = TRUE)

data = list(N_studies_b = length(unique(Nb_data$data_set2)),
            len_b = nrow(Nb_data),
            X_b = with(data_b,
                       cbind(N_dead_fed, N_dead_unfed, N_survived_fed, N_survived_unfed)),
            #Nb = Nb_data$N_total2,
            studyid_b = data_b$data_set2,
            time_b = Nb_data$Time_since_intervention*30)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_base <- stan(file = "script.rstan",
                  data = data, 
                  warmup = 1000,
                  control = list(adapt_delta = 0.8,
                                 max_treedepth = 20),
                  iter = 2000,
                  chains = 2)

launch_shinystan(stan_base)
base <- extract(stan_base)
