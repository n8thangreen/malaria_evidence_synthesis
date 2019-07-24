
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


load(here::here("code/data output/BUGSoutput_evidsynth.RData"))

summary_f <- BUGSoutput$summary[grepl(pattern = "pred_f", rownames(BUGSoutput$summary)), ]
summary_d <- BUGSoutput$summary[grepl(pattern = "pred_d", rownames(BUGSoutput$summary)), ]


# 2 category aggregate data:
# this doesnt give the two-way interactions that we want

## https://stackoverflow.com/questions/10535235/generate-correlated-random-numbers-from-binomial-distributions-in-r

library(copula)

n <- 10
N_mqto <- 100

# plot(x2)

tmp <- normalCopula(1, dim = 2)

x <- rCopula(n = n, copula = tmp)

sim_dat <- NULL

for (t in 1:12) {
  
  p_d <- summary_d[t, "mean"]
  p_f <- summary_f[t, "mean"]
  
  sim_dat <-
    rbind(sim_dat,
          cbind.data.frame(
            t,
            X_d = qbinom(p = x[, 1], size = N_mqto, prob = p_d),
            X_f = qbinom(p = x[, 2], size = N_mqto, prob = p_f)))
}


# 4 category data:
# samples bernoulli values and then sums up to give category frequencies

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
  
  bincorr <- (p11 - p1*p2)/sqrt(p1*(1 - p1)*p2*(1 - p2)) 
  return(bincorr) 
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
