
# posterior time series
# pair of plots for proportion killed
# and proportion successfully fed

IRS_BUGS_plot <- function(BUGSoutput,
                          jags_dat_input,
                          save = TRUE,
                          file = "") {
  
  # months or days?
  # tmax <- 365
  tmax <- 12
  
  # posterior predicted: multinomial all categories
  
  all_param_nm <- rownames(BUGSoutput$summary)
  N_studies <- jags_dat_input$N_studies_b
  
  # extract summary statistics
  summary_sf <- BUGSoutput$summary[grepl(pattern = "pred_sf", all_param_nm), ]
  summary_d <- BUGSoutput$summary[grepl(pattern = "pred_d", all_param_nm), ]
  
  summary_jd <- list()
  summary_jsf <- list()
  
  for (i in seq_len(N_studies)) {
    
    summary_jd[[i]] <- BUGSoutput$summary[grepl(pattern = paste0("predj_d\\[", i,","), all_param_nm), ]
    summary_jsf[[i]] <- BUGSoutput$summary[grepl(pattern = paste0("predj_sf\\[", i,","), all_param_nm), ]
  }
  
  # x11()
  if (save) {
    png(sprintf("%s_plot.png", file), width = 800, height = 400)
    on.exit(dev.off())
  }
  
  par(mfrow = c(1,2))
  
  plot(NULL, ylim = c(0,1), xlim = c(1,tmax),
       xlab = "Time in months", ylab = "Proportion of mosquitoes killed", main = "", las = 1, xaxt = 'n')
  axis(1, at = seq(1, tmax, by = 1))
  
  # medians
  lines(summary_d[,"50%"], type = "l", col = "darkgrey", lwd = 2)
  
  for (i in seq_len(N_studies)) {
    lines(summary_jd[[i]][,"50%"], type = "l", col = "darkgrey")
    # text(x = 12, y = summary_jd[[i]][,"50%"][12], labels = i)
  }
  
  # uncertainty ribbons
  polygon(x = c(1:tmax, tmax:1),
          y = c(summary_d[,"25%"],
                rev(summary_d[,"75%"])),
          col = transp("blue",0.2), border = FALSE)
  polygon(x = c(1:tmax, tmax:1),
          y = c(summary_d[,"2.5%"],
                rev(summary_d[,"97.5%"])),
          col = transp("blue",0.2), border = FALSE)
  
  # raw data
  try(points(jags_dat_input$time_b, jags_dat_input$X_d/jags_dat_input$Nb,
         pch = jags_dat_input$studyid_b), silent = TRUE)
  
  
  plot(NULL, ylim = c(0,1), xlim = c(1,tmax),
       xlab = "Time in months", ylab = "Proportion of mosquitoes successfully fed", main = "", las = 1, xaxt = 'n')
  axis(1, at = seq(1, tmax, by = 1))
  
  # mdeians
  lines(summary_sf[,"50%"], type = "l", col = "darkgrey", lwd = 2)
  
  for (i in seq_len(N_studies)) {
    lines(summary_jsf[[i]][,"50%"], type = "l", col = "darkgrey")
  }
  
  # uncertainty ribbons
  polygon(x = c(1:tmax, tmax:1),
          y = c(summary_sf[,"25%"],
                rev(summary_sf[,"75%"])),
          col = transp("orange",0.2), border = FALSE)
  polygon(x = c(1:tmax, tmax:1),
          y = c(summary_sf[,"2.5%"],
                rev(summary_sf[,"97.5%"])),
          col = transp("orange",0.2), border = FALSE)
  
  # raw data
  try(points(jags_dat_input$time_b, jags_dat_input$X_sf/jags_dat_input$Nb,
         pch = jags_dat_input$studyid_b), silent = TRUE)
  # text(jags_dat_input$time_b, jags_dat_input$X_sf/jags_dat_input$Nb,
  #      labels = jags_dat_input$studyid_b)
 
}
