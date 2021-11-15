
#' ---
#' title: "Evidence synthesis IRS model:
#' jags using aggregated data empirical joint probabilities only"
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

# read-in data
data_a <- read.csv(here::here("data input", "N2_data.csv"), header = TRUE)

# remove unpublished study
data_a <- data_a[data_a$study_id != 20, ]
data_a$study_id <- as.numeric(as.factor(data_a$study_id))

# create input data
jags_dat_input <- 
  list(
    len_b = nrow(data_a), #number of data points of type a
    Nb = data_a$N_total, #total number of mosquitos in each trial type a
    X_d = data_a$N_dead,
    X_sf = round(with(data_a, (1 - N_dead/N_total)*N_fed), 0),
    time_b = data_a$months_since_IRS,
    N_studies_b = length(unique(data_a$study_id)),
    studyid_b = data_a$study_id)

# monitored values
params <-
  c("mu_beta0", "sigma_beta0",
    "mu_beta1", "sigma_beta1",
    "mu_beta0d", "sigma_beta0d",
    "mu_beta1d", "sigma_beta1d",
    "pred_sf", "pred_d",
    "predj_sf", "predj_d")


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
            model.file = here::here("BUGS/BUGS_code_comp_only.txt"),
            n.chains = 2,
            n.iter = n_iter,
            n.burnin = n_burnin,
            n.thin = n_thin,
            DIC = TRUE,
            working.directory = here::here(),
            progress.bar = "text")

BUGSoutput <- out$BUGSoutput

folder_nm <- "BUGSoutput_aggr_before"
dir.create(here::here("code", "data output", folder_nm), showWarnings = FALSE)

save(BUGSoutput, file = here::here("data output", folder_nm, "BUGSoutput.RData"))
save(jags_dat_input, file = here::here("data output", folder_nm, "jags_dat_input.RData"))
