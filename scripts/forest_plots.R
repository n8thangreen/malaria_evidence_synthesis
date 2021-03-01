
# forest plot of posterior distns


library(shinystan)
library(coda)
library(bayesplot)


folder_nm <- "BUGSoutput_aggr_before"
load(here::here("data output", folder_nm, "BUGSoutput.RData")) 

dat_aggb <- BUGSoutput$sims.matrix
colnames(dat_aggb) <- paste0("aggb:", colnames(dat_aggb))

folder_nm <- "BUGSoutput_aggr_after"
load(here::here("data output", folder_nm, "BUGSoutput.RData"))

dat_agga <- BUGSoutput$sims.matrix
colnames(dat_agga) <- paste0("agga:", colnames(dat_agga))

folder_nm <- "BUGSoutput_comp_only"
load(here::here("data output", folder_nm, "BUGSoutput.RData"))

dat_comp <- BUGSoutput$sims.matrix
colnames(dat_comp) <- paste0("comp:", colnames(dat_comp))

folder_nm <- "BUGSoutput_evidsynth"
load(here::here("data output", folder_nm, "BUGSoutput.RData"))

dat_evidsynth <- BUGSoutput$sims.matrix


plot_dat <- 
  cbind(exp(dat_agga[, c("agga:mu_beta0d", "agga:mu_beta0")]),
        exp(dat_aggb[, c("aggb:mu_beta0d", "aggb:mu_beta0")]),
        # exp(dat_comp[, c("comp:mu_beta0d", "comp:mu_beta0")]),
        exp(dat_evidsynth[, c("mu_beta0[2]", "mu_beta0[3]", "mu_beta0[4]", "mu_beta1[2]")]),
        dat_evidsynth[, c("OR[1]", "OR[2]")])
        
mcmc_intervals(plot_dat,
               prob = 0.8)

