
#' ---
#' title: "Evidence synthesis IRS model:
#' jags using 4 category data and aggregated data"
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

data_a <- read.csv(here::here("data input", "N2_data.csv"), header = TRUE)
data_b <- read.csv(here::here("data input", "N4_data.csv"), header = TRUE)

# remove trial
data_a <-
  dplyr::filter(data_a, study_id != 20) %>% 
  dplyr::mutate(study_id = ifelse(study_id > 20, study_id - 1, study_id))

data_b <- data_b[data_b$study_id != 3, ]
data_b$study_id <- as.numeric(as.factor(data_b$study_id))

jags_dat_input <- 
  list(
    # a
    len_a = nrow(data_a), #number of data points of type a
    Na = data_a$N_total,  #total number of mosquitoes in each trial type a
    Xd = data_a$N_dead,
    Xf = data_a$N_fed,
    time_a = data_a$months_since_IRS,
    N_studies_a = length(unique(data_a$study_id)),
    studyid_a = data_a$study_id,
    # b
    len_b = nrow(data_b), #number of data points of type b
    Nb = data_b$N_total,  #total number of mosquitoes in each trial type b
    time_b = data_b$months_since_IRS,
    N_studies_b = length(unique(data_b$study_id)),
    studyid_b = data_b$study_id,
    X_b = with(data_b,
               cbind(N_survived_fed = Nsf + Nsfe,
                     N_dead_fed = Ndf + Ndfe, 
                     N_survived_unfed = Nsn + Nsne,
                     N_dead_unfed = Ndn + Ndne))
  )

params <-
  c("mu_beta0", "sigma_beta0",
    "mu_beta1", "sigma_beta1",
    "pred_d", "pred_sf",
    "pred_f",
    "OR"
    # "prob_pred"
    # "ppost",
    # "delta_c",
    # "thresh"
  )

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
            parameters.to.save = params,
            model.file = here::here("BUGS/BUGS_code_evidsynth.txt"),
            n.chains = 2,
            n.iter = n_iter,
            n.burnin = n_burnin,
            n.thin = n_thin,
            DIC = TRUE,
            working.directory = here::here(),
            progress.bar = "text")

BUGSoutput <- out$BUGSoutput

folder_nm <- "BUGSoutput_evidsynth"

dir.create(here::here("code", "data output", folder_nm), showWarnings = FALSE)

save(BUGSoutput,
     file = here::here("data output", folder_nm, "BUGSoutput.RData"))

save(jags_dat_input,
     file = here::here("data output", folder_nm, "jags_dat_input.RData"))
