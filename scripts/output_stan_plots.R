
# output stan plots
# probability over time line plots


########
# prep #
########

load(here::here("data output", "stan_output.RData"))

p_pred <- base$p_pred

summary_d <- NULL

for (i in 1:12) {
  summary_d <- 
    rbind(summary_d,
          quantile(p_pred[, 2, i] + p_pred[, 4, i],
                   probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))
}

summary_d

summary_sf <- NULL

for (i in 1:12) {
  summary_sf <- 
    rbind(summary_sf,
          quantile(p_pred[, 1, i],
                   probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))
}

summary_sf

tmax <- 12


########
# plot #
########

par(mfrow = c(1,2))

plot(NULL, ylim = c(0, 1), xlim = c(1, tmax),
     xlab = "Time in months", ylab = "Proportion of mosquitoes killed",
     main = "", las = 1, xaxt = 'n')

axis(1, at = seq(1, tmax, by = 1))

# medians
lines(summary_d[, "50%"], type = "l", col = "darkgrey", lwd = 2)

# uncertainty ribbons
polygon(x = c(1:tmax, tmax:1),
        y = c(summary_d[, "25%"],
              rev(summary_d[, "75%"])),
        col = transp("blue", 0.2), border = FALSE)
polygon(x = c(1:tmax, tmax:1),
        y = c(summary_d[, "2.5%"],
              rev(summary_d[, "97.5%"])),
        col = transp("blue", 0.2), border = FALSE)

plot(NULL, ylim = c(0, 1), xlim = c(1, tmax),
     xlab = "Time in months", ylab = "Proportion of mosquitoes successfully fed",
     main = "", las = 1, xaxt = 'n')

axis(1, at = seq(1, tmax, by = 1))

# medians
lines(summary_sf[, "50%"], type = "l", col = "darkgrey", lwd = 2)

# uncertainty ribbons
polygon(x = c(1:tmax, tmax:1),
        y = c(summary_sf[, "25%"],
              rev(summary_sf[, "75%"])),
        col = transp("orange", 0.2), border = FALSE)
polygon(x = c(1:tmax, tmax:1),
        y = c(summary_sf[, "2.5%"],
              rev(summary_sf[, "97.5%"])),
        col = transp("orange", 0.2), border = FALSE)

