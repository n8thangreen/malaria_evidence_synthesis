
#' BUGS forest plot
#'
#' @param BUGS_list Named list of BUGS output
#' @param save 
#' 
#' @importFrom epicontacts, adegenet
#' @import dplyr ggplot2 tidybayes purrr
#'
#' @return
#' @export
#'
BUGS_forest_plot <- function(BUGS_list,
                             save = FALSE) {
  tmax <- 12
  dat <- map(BUGS_list, create_forest_data)
  names(dat) <- names(BUGS_list)
  
  dat <-
    purrr::transpose(dat) %>% 
    map(bind_rows, .id = "data")
  
  dat$tidy <-
    dat$tidy %>%
    mutate(Outcome = ifelse(Outcome == "pred_d", "Dead", "Survived Fed")) %>% 
    rename(`Time (months)` = Time)
  
  dat$j <- rename(dat$j, `Time (months)` = Time)
  
  out <- 
    dat$tidy %>%
    ggplot(aes(x = `50%`, y = data, xmin= `25%`, xmax = `75%`, colour = `Time (months)`, shape = Outcome)) +
    geom_point(data = dat$j,
               position = position_dodge(width = 0.3),
               aes(x = `50%`, y = data, group = interaction(Outcome, `Time (months)`)),
               size = 1.5, colour = "lightgrey", inherit.aes = FALSE) +
    geom_pointinterval(position = position_dodge(width = 0.3)) +
    geom_point(position = position_dodge(width = 0.3),
               aes(x = `50%`, y = data, colour = `Time (months)`, shape = Outcome),
               size = 3, inherit.aes = FALSE) +
    theme_bw() +
    theme(axis.text = element_text(size = 15)) +
    xlab("Posterior probability") +
    scale_y_discrete("", 
                     labels = c("Aggregate data only\n (Model 1)", "Aggregate data only\n (Model 2a)", "Comprehensive data only\n (Model 2b)", "All data\n (Model 3)"))
  
  if (save) {
    ggsave(filename = "forest_plot.png", dpi = 640, height = 8, width = 8)
  }
  
  out
}


#
create_forest_data <- function(BUGSoutput) {
  
  all_param_nm <- rownames(BUGSoutput$summary)
  
  # extract summary statistics
  summary_sf <- BUGSoutput$summary[grepl(pattern = "pred_sf\\[(3|6|12)\\]", all_param_nm), ]
  summary_d <- BUGSoutput$summary[grepl(pattern = "pred_d\\[(3|6|12)\\]", all_param_nm), ]
  
  summary_jd <- list()
  summary_jsf <- list()
  
  N_studies <- ncol(BUGSoutput$sims.list$predj_d)
  
  if (is.null(N_studies)) {
    dat_j <- NULL
  } else {
    
    for (i in seq_len(N_studies)) {
      
      summary_jd[[i]] <-
        BUGSoutput$summary[grepl(pattern = paste0("predj_d\\[", i,",", "(3|6|12)\\]"), all_param_nm), ]
      
      summary_jsf[[i]] <-
        BUGSoutput$summary[grepl(pattern = paste0("predj_sf\\[", i,",", "(3|6|12)\\]"), all_param_nm), ]
    }
    
    dat_jd <- do.call(rbind, summary_jd)
    dat_jsf <- do.call(rbind, summary_jsf)
    
    dat_j <-
      rbind(dat_jsf, dat_jd) %>% 
      as.data.frame() %>% 
      select(`50%`, `25%`, `75%`) %>% 
      mutate(var = row.names(.)) %>% 
      tidyr::separate(var, c("Outcome", "Time"), "\\[") %>% 
      mutate(Time = gsub("]", "", Time),
             Time = gsub("^[0-9]+,", "", Time),
             Time = factor(Time, c("3","6","12")),
             Outcome = gsub("j", "", Outcome))
  }
  
  dat_tidy <-
    rbind(summary_sf, summary_d) %>% 
    as.data.frame() %>% 
    select(`50%`, `25%`, `75%`) %>% 
    mutate(var = row.names(.)) %>% 
    tidyr::separate(var, c("Outcome", "Time"), "\\[") %>% 
    mutate(Time = gsub("]", "", Time),
           Time = factor(Time, c("3","6","12")))
  
  list(tidy = dat_tidy,
       j = dat_j)
}

