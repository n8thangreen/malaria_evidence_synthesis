
#' ---
#' title: "Evidence synthesis IRS model:
#' jags using aggregated data and afterwards taking product of probs"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---

# load packages
library(R2jags)
library(R2WinBUGS)
library(purrr)

# readin data
data_a <- read.csv(here::here("data input", "N2_data.csv"), header = TRUE)

# create input data
jags_dat_input <- 
  list(
    len_b = nrow(data_a), #number of data points of type a
    Nb = data_a$N_total, #total number of mosquitos in each trial type a
    X_d = data_a$N_dead,
    X_f = data_a$N_fed,
    time_b = data_a$months_since_IRS,
    N_studies_b = length(unique(data_a$study_id)),
    studyid_b = data_a$study_id
  )

# monitored values
params <-
  c("mu_beta0", "sigma_beta0",
    "mu_beta1", "sigma_beta1",
    "mu_beta0d", "sigma_beta0d",
    "mu_beta1d", "sigma_beta1d",
    "pred_f", "pred_sf", "pred_d",
    "predj_f", "predj_sf", "predj_d"
  )


# mcmc settings
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
            model.file = here::here("code", "BUGS_code_aggr_after.txt"),
            n.chains = 2,
            n.iter = n_iter,
            n.burnin = n_burnin,
            n.thin = n_thin,
            DIC = TRUE,
            working.directory = here::here("code"),
            progress.bar = "text")

BUGSoutput <- out$BUGSoutput

folder_nm <- "BUGSoutput_aggr_after"
dir.create(here::here("code", "data output", folder_nm), showWarnings = FALSE)

save(BUGSoutput, file = here::here("code", "data output", folder_nm, "BUGSoutput.RData"))
save(jags_dat_input, file = here::here("code", "data output", folder_nm, "jags_dat_input.RData"))
