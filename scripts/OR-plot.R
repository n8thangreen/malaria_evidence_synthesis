
# OR plots
#
#

OR_rows <- grepl(pattern = "OR",
                 rownames(BUGSoutput$summary))
summary_OR <- BUGSoutput$summary[OR_rows, ]

# empty plot
plot(NULL,
     ylim = c(0,1.5),
     # ylim = c(0,300),
     xlim = c(1,12),
     xlab = "Time in months", ylab = "OR", main = "", las = 1, xaxt = 'n')
axis(1, at = seq(1, 12, by = 1))


tmax <- nrow(summary_OR)

# uncertainty ribbons
polygon(x = c(1:tmax, tmax:1),
        y = c(summary_OR[,"2.5%"],
              rev(summary_OR[,"97.5%"])),
        # col = transp("blue",0.2),
        col = "lightblue",
        border = FALSE)
polygon(x = c(1:tmax, tmax:1),
        y = c(summary_OR[,"25%"],
              rev(summary_OR[,"75%"])),
        # col = transp("blue",0.2),
        col = "blue",
        border = FALSE)
# medians
lines(summary_OR[,"50%"], type = "l", col = "black", lwd = 2)
# lines(summary_OR[,"mean"], type = "l", col = "black", lwd = 2)
