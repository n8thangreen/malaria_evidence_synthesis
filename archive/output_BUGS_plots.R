
## posterior predicted: multinomial all categories

plot(colMeans(BUGSoutput$sims.list$prob_pred[,,1]), type = "l", ylim = c(0,1))
lines(colMeans(BUGSoutput$sims.list$prob_pred[,,2]), type = "l", col = "red")
lines(colMeans(BUGSoutput$sims.list$prob_pred[,,3]), type = "l", col = "blue")
lines(colMeans(BUGSoutput$sims.list$prob_pred[,,4]), type = "l", col = "green")

polygon(x = c(1:365, 365:1),
        y = c(colMeans(BUGSoutput$sims.list$prob_pred[,,1]) - 1.96*apply(BUGSoutput$sims.list$prob_pred[,,1], 2, sd),
              rev(colMeans(BUGSoutput$sims.list$prob_pred[,,1]) + 1.96*apply(BUGSoutput$sims.list$prob_pred[,,1], 2, sd))),
        col = transp("aquamarine3",0.2), border = FALSE)
polygon(x = c(1:365, 365:1),
        y = c(colMeans(BUGSoutput$sims.list$prob_pred[,,2]) - 1.96*apply(BUGSoutput$sims.list$prob_pred[,,2], 2, sd),
              rev(colMeans(BUGSoutput$sims.list$prob_pred[,,2]) + 1.96*apply(BUGSoutput$sims.list$prob_pred[,,2], 2, sd))),
        col = transp("green",0.2), border = FALSE)
polygon(x = c(1:365, 365:1),
        y = c(colMeans(BUGSoutput$sims.list$prob_pred[,,3]) - 1.96*apply(BUGSoutput$sims.list$prob_pred[,,3], 2, sd),
              rev(colMeans(BUGSoutput$sims.list$prob_pred[,,3]) + 1.96*apply(BUGSoutput$sims.list$prob_pred[,,3], 2, sd))),
        col = transp("orange",0.2), border = FALSE)
polygon(x = c(1:365, 365:1),
        y = c(colMeans(BUGSoutput$sims.list$prob_pred[,,4]) - 1.96*apply(BUGSoutput$sims.list$prob_pred[,,4], 2, sd),
              rev(colMeans(BUGSoutput$sims.list$prob_pred[,,4]) + 1.96*apply(BUGSoutput$sims.list$prob_pred[,,4], 2, sd))),
        col = transp("yellow",0.2), border = FALSE)

##TEST
## all == 1
colMeans(BUGSoutput$sims.list$prob_pred[,,1]) + colMeans(BUGSoutput$sims.list$prob_pred[,,2]) + colMeans(BUGSoutput$sims.list$prob_pred[,,3]) + colMeans(BUGSoutput$sims.list$prob_pred[,,4])


library(adegenet)

## posterior predicted: summed multinomial as death and fed

plot(colMeans(BUGSoutput$sims.list$predd_pred), type = "l", ylim = c(0,1))
lines(colMeans(BUGSoutput$sims.list$predf_pred), type = "l", col = "red")
polygon(x = c(1:365, 365:1),
        y = c(colMeans(BUGSoutput$sims.list$predd_pred) - 1.96*apply(BUGSoutput$sims.list$predd_pred, 2, sd),
              rev(colMeans(BUGSoutput$sims.list$predd_pred) + 1.96*apply(BUGSoutput$sims.list$predd_pred, 2, sd))),
        # y = c(quantile(BUGSoutput$sims.list$predd_pred, probs = 0.25),
        #       rev(quantile(BUGSoutput$sims.list$predd_pred, probs = 0.75))),
        col = transp("orange",0.2), border = FALSE)
polygon(x = c(1:365, 365:1),
        y = c(colMeans(BUGSoutput$sims.list$predf_pred) - 1.96*apply(BUGSoutput$sims.list$predf_pred, 2, sd),
              rev(colMeans(BUGSoutput$sims.list$predf_pred) + 1.96*apply(BUGSoutput$sims.list$predf_pred, 2, sd))),
        col = transp("aquamarine3",0.2), border = FALSE)


## posterior predicted: binomial death and fed

plot(colMeans(BUGSoutput$sims.list$Yd_pred), type = "l", ylim = c(0,1))
lines(colMeans(BUGSoutput$sims.list$Yf_pred), type = "l", col = "red")
polygon(x = c(1:365, 365:1), 
        y = c(colMeans(BUGSoutput$sims.list$Yd_pred) - 1.96*apply(BUGSoutput$sims.list$Yd_pred, 2, sd),
              rev(colMeans(BUGSoutput$sims.list$Yd_pred) + 1.96*apply(BUGSoutput$sims.list$Yd_pred, 2, sd))),
        col = transp("orange",0.2), border = FALSE)
polygon(x = c(1:365, 365:1),
        y = c(colMeans(BUGSoutput$sims.list$Yf_pred) - 1.96*apply(BUGSoutput$sims.list$Yf_pred, 2, sd),
              rev(colMeans(BUGSoutput$sims.list$Yf_pred) + 1.96*apply(BUGSoutput$sims.list$Yf_pred, 2, sd))),
        col = transp("aquamarine3",0.2), border = FALSE)
