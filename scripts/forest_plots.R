
# forest plot of posterior distributions


library(shinystan)
library(coda)
library(bayesplot)
library(ggplot2)
library(tidybayes)
library(dplyr)


#############
# load data #
#############

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


#########
# plots #
#########

plot_dat <- 
  cbind(dat_agga[, c("agga:pred_sf[3]", "agga:pred_d[3]", "agga:pred_sf[6]", "agga:pred_d[6]", "agga:pred_sf[12]", "agga:pred_d[12]")],
        dat_aggb[, c("aggb:pred_sf[3]", "aggb:pred_d[3]", "aggb:pred_sf[6]", "aggb:pred_d[6]", "aggb:pred_sf[12]", "aggb:pred_d[12]")],
        dat_comp[, c("comp:pred_sf[3]", "comp:pred_d[3]", "comp:pred_sf[6]", "comp:pred_d[6]", "comp:pred_sf[12]", "comp:pred_d[12]")],
        dat_evidsynth[, c("pred_sf[3]", "pred_d[3]", "pred_sf[6]", "pred_d[6]", "pred_sf[12]", "pred_d[12]")]
        )

plot_dat <-
  plot_dat[, c("agga:pred_sf[3]", "aggb:pred_sf[3]", "comp:pred_sf[3]", "pred_sf[3]",
               "agga:pred_sf[6]", "aggb:pred_sf[6]", "comp:pred_sf[6]", "pred_sf[6]",
               "agga:pred_sf[12]", "aggb:pred_sf[12]", "comp:pred_sf[12]", "pred_sf[12]",
               "agga:pred_d[3]", "aggb:pred_d[3]", "comp:pred_d[3]", "pred_d[3]",
               "agga:pred_d[6]", "aggb:pred_d[6]", "comp:pred_d[6]", "pred_d[6]",
               "agga:pred_d[12]", "aggb:pred_d[12]", "comp:pred_d[12]", "pred_d[12]")]


mcmc_intervals(plot_dat,
               prob = 0.8) +
  scale_y_discrete("", 
                   labels = c("Success fed; Adjust before (3 months)", "Success fed; Adjust after (3 months)", "Success fed; Comprehensive (3 months)", "Success fed; All (3 months)",
                              "Success fed; Adjust before (6 months)", "Success fed; Adjust after (6 months)", "Success fed; Comprehensive (6 months)", "Success fed; All (6 months)",
                              "Success fed; Adjust before (12 months)", "Success fed; Adjust after (12 months)", "Success fed; Comprehensive (12 months)", "Success fed; All (12 months)",
                              "Dead; Adjust before (3 months)", "Dead; Adjust after (3 months)", "Dead; Comprehensive (3 months)", "Dead; All (3 months)",
                              "Dead; Adjust before (6 months)", "Dead; Adjust after (6 months)", "Dead; Comprehensive (6 months)", "Dead; All (6 months)",
                              "Dead; Adjust before (12 months)", "Dead; Adjust after (12 months)", "Dead; Comprehensive (12 months)", "Dead; All (12 months)")) +
  xlab("Posterior probability") +
  theme(axis.text = element_text(size = 12))


# tidybayes::

dat_tidy <-
  reshape2::melt(plot_dat) %>% 
  tidyr::separate(Var2, c("model", "Time"), "\\[") %>%
  tidyr::separate(model, c("data", "Outcome"), "_") %>%
  group_by(data, Outcome, Time) %>% 
  summarise(value = quantile(value, c(0.25, 0.5, 0.75)), q = c(0.25, 0.5, 0.75)) %>% 
  reshape2::dcast(data+Time+Outcome~q) %>% 
  mutate(Time = gsub("]", "", Time),
         Time = factor(Time, c("3","6","12")),
         Outcome = ifelse(Outcome == "d", "Dead", "Survived and fed")) %>% 
  rename(`Time (months)` = Time)
            
# dat_tidy %>% 
#   ggplot(aes(x = value, y = Var2)) +
#   stat_eye()

dat_tidy %>% 
  ggplot(aes(x = `0.5`, y = data, xmin= `0.25`, xmax = `0.75`, colour = `Time (months)`, shape = Outcome)) +
  geom_pointinterval(position = position_dodge(width = 0.3)) +
  geom_point(position = position_dodge(width = 0.3), aes(x = `0.5`, y = data, colour = `Time (months)`, shape = Outcome), size = 3, inherit.aes = F) +
  scale_y_discrete("", 
                   labels = c("Aggregare after", "Aggregate before", "Comprehensive", "All")) +
  xlab("Posterior probability") +
  theme(axis.text = element_text(size = 12)) +
  theme_bw()
                            
ggsave(filename = "plots/forestplot_tidy.png")
  

# regression parameters --------------------------------------------------------------------

plot_dat <- 
  cbind(exp(dat_agga[, c("agga:mu_beta0d", "agga:mu_beta0", "agga:mu_beta1d", "agga:mu_beta1")]),
        exp(dat_aggb[, c("aggb:mu_beta0d", "aggb:mu_beta0", "aggb:mu_beta1d", "aggb:mu_beta1")]),
        exp(dat_comp[, c("comp:mu_beta0d", "comp:mu_beta0", "comp:mu_beta1d", "comp:mu_beta1")]),
        exp(dat_evidsynth[, c("mu_beta0[2]", "mu_beta0[3]", "mu_beta0[4]", "mu_beta1[2]", "mu_beta1[3]", "mu_beta1[4]")])
  )

plot_dat <-
  plot_dat[, c("agga:mu_beta0d",
               "aggb:mu_beta0d",
               "comp:mu_beta0d",
               "agga:mu_beta0",
               "aggb:mu_beta0",
               "comp:mu_beta0",
               "agga:mu_beta1d",
               "aggb:mu_beta1d",
               "comp:mu_beta1d",
               "agga:mu_beta1",
               "aggb:mu_beta1",
               "comp:mu_beta1",
               "mu_beta0[2]",
               "mu_beta0[3]",
               "mu_beta0[4]",
               "mu_beta1[2]",
               "mu_beta1[3]",
               "mu_beta1[4]")]


mcmc_intervals(plot_dat,
               prob = 0.8) +
  xlim(0, 4) +
  scale_y_discrete("", 
                   labels = c("adjust before: death intercept",
                              "adjust after: death intercept",
                              "comprehensive: death intercept",
                              "adjust before: fed intercept",
                              "adjust after: success fed intercept",
                              "comprehensive: success fed intercept",
                              "adjust before: d time slope",
                              "adjust before: d time slope",
                              "adjust after: d time slope",
                              "comprehensive: sf time slope",
                              "adjust after: f time slope",
                              "comprehensive: sf time slope",
                              "full model: df intercept",
                              "full model: s not f intercept",
                              "full model: d not f intercept",
                              "full model: df time slope",
                              "full model: s not f time slope",
                              "full model: d not f time slope"
                   )) +
  geom_vline(xintercept = 1, linetype = "dashed")
