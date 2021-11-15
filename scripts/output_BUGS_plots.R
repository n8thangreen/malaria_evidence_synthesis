#' ---
#' title: "Evidence synthesis IRS model: plots"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---

## to view in shinystan
# xx <- coda::as.mcmc.list(BUGSoutput)
# yy <- as.shinystan(xx)
# launch_shinystan(yy)


library(adegenet)
library(shinystan)
library(coda)

source("R/IRS_BUGS_plot.R")


folder_nm <- "BUGSoutput_aggr_before"
load(here::here("data output", folder_nm, "BUGSoutput.RData"))
load(here::here("data output", folder_nm, "jags_dat_input.RData"))

IRS_BUGS_plot(BUGSoutput, jags_dat_input, file = "plots/aggr_before")


folder_nm <- "BUGSoutput_aggr_after"
load(here::here("data output", folder_nm, "BUGSoutput.RData"))
load(here::here("data output", folder_nm, "jags_dat_input.RData"))

IRS_BUGS_plot(BUGSoutput, jags_dat_input, file = "plots/aggr_after")


folder_nm <- "BUGSoutput_comp_only"
load(here::here("data output", folder_nm, "BUGSoutput.RData"))
load(here::here("data output", folder_nm, "jags_dat_input.RData"))

IRS_BUGS_plot(BUGSoutput, jags_dat_input, file = "plots/comp_only")


folder_nm <- "BUGSoutput_evidsynth"
load(here::here("data output", folder_nm, "BUGSoutput.RData"))
load(here::here("data output", folder_nm, "jags_dat_input.RData"))

IRS_BUGS_plot(BUGSoutput, jags_dat_input, file = "plots/evidsynth", save = FALSE)

