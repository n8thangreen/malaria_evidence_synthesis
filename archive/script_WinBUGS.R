
#' ---
#' title: "Evidence synthesis IRS model: jags"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---


library(R2jags)
library(R2WinBUGS)
library(purrr)

data_a = read.csv(here::here("code", "Na_data.csv"), header = TRUE)
data_b = read.csv(here::here("code", "Nb_data.csv"), header = TRUE)

jags_dat_input =
  list(
    # a
    len_a = nrow(data_a), #number of data points of type a
    Na = data_a$N_total, #total number of mosquitos in each trial type a
    Xd = data_a$N_dead,
    Xf = data_a$N_fed,
    time_a = data_a$Time_since_intervention*30,
    N_studies_a = 10, #number of studies of type a
    studyid_a = data_a$data_set1,
    # b
    len_b = nrow(data_b), #number of data points of type b
    Nb = data_b$N_total2,  #total number of mosquitos in each trial type b
    time_b = data_b$Time_since_intervention*30,
    N_studies_b = 3, #number of studies of type b
    studyid_b = data_b$data_set2,
    Xdfsu = with(data_b, cbind(N_dead_fed, N_dead_unfed, N_survived_fed, N_survived_unfed))
  )
    

params <-
  c("mu_beta0d", "sigma_beta0d",
    "mu_beta1d", "sigma_beta1d",
    # "beta0d", "beta1d",
    "Yd_pred",
    "mu_beta0f", "sigma_beta0f",
    "mu_beta1f", "sigma_beta1f",
    # "beta0f", "beta1f",
    "Yf_pred",
    "mu_beta0", "sigma_beta0",
    "mu_beta1", "sigma_beta1",
    "predd_pred", "predf_pred",
    "prob_pred"
    # "ppost",
    # "delta_c",
    # "thresh"
  )

# inits <- function(){
#   list(
#     mu_beta0d = ,
#     logsigma_beta0d = ,
#     mu_beta1d = ,
#     logsigma_beta1d =
#   )
# }


#test
n_iter <- 10000
n_burnin <- 100
n_thin <- 10

# n_iter <- 1e6
# n_burnin <- 1e3
# n_thin <- 1e2 #floor((n_iter - n_burnin)/500)


##############
## run MCMC ##
##############

out <- jags(jags_dat_input,
            # inits = list(inits(), inits()),
            parameters.to.save = params,
            model.file = here::here("code", "BUGS_code.txt"),
            n.chains = 2,
            n.iter = n_iter,
            n.burnin = n_burnin,
            n.thin = n_thin,
            DIC = TRUE,
            working.directory = here::here("code"),
            progress.bar = "text")

BUGSoutput <- out$BUGSoutput

save(BUGSoutput, file = here::here("code", "BUGSoutput.RData"))


