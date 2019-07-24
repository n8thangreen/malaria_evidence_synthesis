
#' ---
#' title: "Evidence synthesis IRS model: plots"
#'
#' author: "N Green"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     keep_md: TRUE
#' ---

# load packages
library(adegenet)
library(shinystan)
library(coda)

# readin data
load(here::here("code", "data output", "BUGSoutput_realdata.RData"))

xx <- coda::as.mcmc.list(BUGSoutput)
yy <- as.shinystan(xx)
# launch_shinystan(yy)

# months or days?
# tmax <- 365
tmax <- 12


# posterior predicted: multinomial all categories

# extract summary statistics
summary_sf <- BUGSoutput$summary[grepl(pattern = "prob_pred\\[\\d\\d?,1\\]", rownames(BUGSoutput$summary)), ]
summary_df <- BUGSoutput$summary[grepl(pattern = "prob_pred\\[\\d\\d?,2\\]", rownames(BUGSoutput$summary)), ]
summary_su <- BUGSoutput$summary[grepl(pattern = "prob_pred\\[\\d\\d?,3\\]", rownames(BUGSoutput$summary)), ]
summary_du <- BUGSoutput$summary[grepl(pattern = "prob_pred\\[\\d\\d?,4\\]", rownames(BUGSoutput$summary)), ]

x11()

plot(NULL, ylim = c(0,1), xlim = c(1,12),
     xlab = "Time in months", ylab = "Proportion of mosquitoes successfully fed", main = "", las = 1, xaxt = 'n')
axis(1, at = seq(1, 12, by = 1))

# medians
lines(summary_sf[,"50%"], type = "l", col = "black")
lines(summary_df[,"50%"], type = "l", col = "red")
lines(summary_su[,"50%"], type = "l", col = "blue")
lines(summary_du[,"50%"], type = "l", col = "green")

legend(7.5, 0.95, legend = c("Survived fed", "Dead fed", "Survived unfed", "Dead unfed"),
                           col = c("black", "red", "blue", "green"), lty = 1)

# uncertainty ribbons
polygon(x = c(1:tmax, tmax:1),
        y = c(summary_sf[,"25%"],
              rev(summary_sf[,"75%"])),
        col = transp("yellow",0.2), border = FALSE)
polygon(x = c(1:tmax, tmax:1),
        y = c(summary_df[,"25%"],
              rev(summary_df[,"75%"])),
        col = transp("orange",0.2), border = FALSE)
polygon(x = c(1:tmax, tmax:1),
        y = c(summary_su[,"25%"],
              rev(summary_su[,"75%"])),
        col = transp("aquamarine3",0.2), border = FALSE)
polygon(x = c(1:tmax, tmax:1),
        y = c(summary_du[,"25%"],
              rev(summary_du[,"75%"])),
        col = transp("green",0.2), border = FALSE)


par(mfrow = c(1,2))

### dead only
plot(NULL, ylim = c(0,1), xlim = c(1,12),
     xlab = "Time in months", ylab = "Proportion of mosquitoes killed", main = "Model 3", las = 1, xaxt = 'n')
axis(1, at = seq(1, 12, by = 1))
lines(summary_d[,"50%"], type = "l", col = "black") #median

# uncertainty ribbons
polygon(x = c(1:tmax, tmax:1),
        y = c(summary_d[,"25%"],
              rev(summary_d[,"75%"])),
        col = transp("blue",0.2), border = FALSE)
polygon(x = c(1:tmax, tmax:1),
        y = c(summary_d[,"2.5%"],
              rev(summary_d[,"97.5%"])),
        col = transp("blue",0.2), border = FALSE)


### fed alive only
plot(NULL, ylim = c(0,1), xlim = c(1,12),
     xlab = "Time in months", ylab = "Proportion of mosquitoes successfully fed", main = "", las = 1, xaxt = 'n')
axis(1, at = seq(1, 12, by = 1))
lines(summary_sf[,"50%"], type = "l", col = "black") #median

# uncertainty ribbons
polygon(x = c(1:tmax, tmax:1),
        y = c(summary_sf[,"25%"],
              rev(summary_sf[,"75%"])),
        col = transp("orange",0.2), border = FALSE)
polygon(x = c(1:tmax, tmax:1),
        y = c(summary_sf[,"2.5%"],
              rev(summary_sf[,"97.5%"])),
        col = transp("orange",0.2), border = FALSE)
