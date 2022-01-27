
#' ---
#' title: "Evidence synthesis IRS model:
#' simulate trials with different correlation between
#' death and fed"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---

# copula example:
# http://www.di.fc.ul.pt/~jpn/r/copula/index.html


library(copula)
library(ggplot2)
library(dplyr)


load(here::here("data output/BUGSoutput_evidsynth/BUGSoutput.RData"))

# global values over time
summary_f <- BUGSoutput$summary[grepl(pattern = "pred_f", rownames(BUGSoutput$summary)), ]
summary_d <- BUGSoutput$summary[grepl(pattern = "pred_d", rownames(BUGSoutput$summary)), ]


# 2 category aggregate data:

## https://stackoverflow.com/questions/10535235/generate-correlated-random-numbers-from-binomial-distributions-in-r

## at trial level

n <- 10
N_mqto <- 100

# plot(x2)

tmp <- normalCopula(0.9, dim = 2)

x <- rCopula(n = n, copula = tmp)

sim_dat <- NULL

for (t in 1:12) {
  
  p_d <- summary_d[t, "mean"]
  p_f <- summary_f[t, "mean"]
  
  sim_dat <-
    rbind(sim_dat,
          cbind.data.frame(
            n = as.character(1:n),
            t,
            X_d = qbinom(p = x[, 1], size = N_mqto, prob = p_d),
            X_f = qbinom(p = x[, 2], size = N_mqto, prob = p_f)))
}

sim_dat %>% 
  ggplot(aes(x = t, y = X_d, col = n, group = n)) +
  geom_line() +
  geom_line(aes(x = t, y = X_f, group = n, col = n), inherit.aes = FALSE)


## at mosquito level

copula_trial <- function(copula_param = 1) {
  
  n_trial <- 1
  N_mqto <- 10000
  
  tmp <- normalCopula(copula_param, dim = 2)
  
  sim_dat <- NULL
  
  for (i in 1:n_trial) {
    x <- rCopula(n = N_mqto, copula = tmp)
    
    for (t in 1:12) {
      p_d <- summary_d[t, "mean"]
      p_f <- summary_f[t, "mean"]
      
      sim_dat <-
        rbind(sim_dat,
              cbind.data.frame(
                trial = as.character(i),
                id = 1:N_mqto,
                t,
                X_d = qbinom(p = x[, 1], size = 1, prob = p_d),
                X_f = qbinom(p = x[, 2], size = 1, prob = p_f)))

      ##TODO: use copula:: functions
      ##      what are shape params? transform from mean and sd?
      # myMvd <- mvdc(copula = myCop,
      #               margins = c("beta", "beta"),
      #               paramMargins = list(list(shape1=, shape2=),
      #                                   list(shape1=, shape2=))
      # z2 <- rMvdc(n, myMvd)
    }
  }
  
  plot_dat <- 
    sim_dat %>%
    group_by(trial, t) %>% 
    mutate(X_a = 1 - X_d) %>% 
    mutate(X_df = X_d*X_f,
           X_af = X_a*X_f) %>% 
    summarise(dead = mean(X_d),
              alive = mean(X_a),
              fd = mean(X_df),
              fa = mean(X_af),
              fed = mean(X_f)) %>% 
    mutate(empir_fa = alive*fed,
           empir_fd = dead*fed)
  
  plot_dat
}

plot_dat <- list()
plot_dat[[1]] <- copula_trial()
plot_dat[[2]] <- copula_trial(0)
plot_dat[[3]] <- copula_trial(-1)

# separate
ggplot(plot_dat, aes(x = t, y = dead, col = trial, group = trial)) +
  geom_line() +
  geom_line(aes(x = t, y = fed, group = trial, col = trial), inherit.aes = FALSE) +
  ylim(0,1)

# joint
gg <- list()
for (i in seq_along(plot_dat)) {
  
  gg[[i]] <-
    ggplot(plot_dat[[i]], aes(x = t, y = empir_fa, group = trial)) + #col = trial
    geom_line(size = 2) +
    geom_line(aes(x = t, y = fa, group = trial), size = 2, col = "red", inherit.aes = FALSE) +
    # geom_line(aes(x = t, y = fd, group = trial), col = "red", inherit.aes = FALSE) +
    ylim(0,1) +
    ylab("Prob(alive and fed)") +
    theme_bw() +
    theme(legend.position = "none",
          text = element_text(size = 20)) +
    scale_x_continuous(name = "Time (months)", breaks = seq(1, 12, 1))
}


copula_plot <- gridExtra::grid.arrange(gg[[1]], gg[[2]], gg[[3]], nrow = 1)

ggsave(copula_plot, filename = "plots/copula_plots.png", width = 10, height = 5, dpi = 640)


#####################################################
# 4 category data:
# samples Bernoulli values and then sums up to give category frequencies

# http://r.789695.n4.nabble.com/generating-correlated-Bernoulli-random-variables-td828988.html

require(bindata)
require(dplyr)

n <- 100
p_d <- 0.5
p_f <- 0.3
rho <- 0.2

rmvbin(n, c(p_d, p_f), bincorr = (1 - rho)*diag(2) + rho)

# use the odds ratio (which is unrestricted) to specify 
# the association between the two binary variables and then convert this 
# odds ratio, for given marginal probabilities p1 and p2, into a (valid) 
# correlation rho to be used in rmvbin(). 

# from odds ratio to binary correlation
bincorr <- function(OR, p1, p2) {
  
  if (OR == 1) p11 <- p2 - p2 + p1*p2
  else { 
    p11_1 <- p2 - (1/2/(1 - OR)*(1 - p1 + OR*p1 + p2 - OR*p2 - 
                                   sqrt((-1 + p1 - OR*p1 - p2 + OR*p2)^2 - 4*(1 - OR)*(p2 - p1*p2)))) 
    p11_2 <- p2 - (1/2/(1 - OR)*(1 - p1 + OR*p1 + p2 - OR*p2 - 
                                   sqrt((-1 + p1 - OR*p1 - p2 + OR*p2)^2) - 4*(1 - OR)*(p2 - p1*p2))) 
    if (p11_1 > 0 && p11_1 <= p1 && p11_1 < p2)
      p11 <- p11_1
    else p11 <- p11_2 
  } 
  
  (p11 - p1*p2)/sqrt(p1*(1 - p1)*p2*(1 - p2)) 
} 

odds_ratio <- c(0.0001, 1, 100)
sim_dat <- list()

for (t in 1:12) {
  for (or in odds_ratio) {
    
    p_d <- summary_d[t, "mean"]
    p_f <- summary_f[t, "mean"]
    
    # range of valid correlations for odds ratios 
    rho <- sapply(or, function(x) bincorr(x, p_d, p_f))
    xx <- rmvbin(n, c(p_d, p_f), bincorr = (1 - rho)*diag(2) + rho)
    counts <-
      data.frame(t,
                 N_dead_fed = sum(xx[,1] == 1 & xx[,2] == 1),
                 N_dead_unfed = sum(xx[,1] == 1 & xx[,2] == 0),
                 N_survived_fed = sum(xx[,1] == 0 & xx[,2] == 1),
                 N_survived_unfed = sum(xx[,1] == 0 & xx[,2] == 0))
    sim_dat[[as.character(or)]] <-
      rbind(sim_dat[[as.character(or)]], counts)
  }
}

sim_dat

for (i in seq_along(sim_dat)) {
  
  dat <- sim_dat[[i]]
  
  plot(dat$t, (dat$N_dead_fed + dat$N_dead_unfed)/n,
       type = "o", ylim = c(0, 1),
       ylab = "Probability", xlab = "Time in years",
       main = paste("OR =", odds_ratio[i]), xaxt = 'n')
  axis(1, at = seq(1, 12, by = 1))
  
  lines(dat$t,
        (1 - (dat$N_dead_fed + dat$N_dead_unfed)/n) * (dat$N_dead_fed + dat$N_survived_fed)/n,
        type = "o", col = "red")
  lines(dat$t, dat$N_survived_fed/n,
        type = "o", col = "blue")
  legend("topright", c("empirical", "true"),
         col = c("red", "blue"), lty = c(1,1), bty = 'n')
}
