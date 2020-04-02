#
# create raw data histograms to estimate change in probabilities
#
#

library(reshape2)
library(ggplot2)
library(dplyr)

dat <-
  jags_dat_input$X_b %>%
  as.data.frame() %>% 
  mutate(study_id = 1:nrow(.)) %>% 
  melt(id.vars = "study_id") %>% 
  group_by(study_id) %>% 
  mutate(p = value/sum(value))
  

ggplot(dat, aes(x = p, fill = variable)) +
  # geom_histogram(alpha = 0.3, position = "identity") +
  geom_density(alpha = 0.3) +
  theme_bw()

dat %>% 
  group_by(variable, add = FALSE) %>% 
  summarise(mean(p))
  