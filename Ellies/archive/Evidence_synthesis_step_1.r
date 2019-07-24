#######################################################
###
###
### Mock data for statistical model

Na_data = read.csv("Q:/Na_data.csv",header=TRUE)
Nb_data = read.csv("Q:/Nb_data.csv",header=TRUE)

data_list1 = list(N2 = nrow(Na_data),
                  Na = Na_data$N_total,
                  Xd = Na_data$N_dead,
                  Xf = Na_data$N_fed,
                  time_a = Na_data$Time_since_intervention*30,
                  N_len_a = 10,
                  len_a = Na_data$data_set1,
                  N4 = nrow(Nb_data),
                  Nb = Nb_data$N_total2,
                  Xdf = Nb_data$N_dead_fed,
                  Xdu = Nb_data$N_dead_unfed,
                  Xsf = Nb_data$N_survived_fed,
                  Xsu = Nb_data$N_survived_unfed,
                  N_bins = 4,
                  time_b = Nb_data$Time_since_intervention*30,
                  N_len_b = 3,
                  len_b = Nb_data$data_set2)

N_bins = 4
N_len_b = 3
len_b = array(dim=c(length(Nb_data$N_total2[Nb_data$data_set2 == 1]),N_bins,N_len_b))
for(i in 1:3){
  len_b[,,i] = data = c(Nb_data$N_dead_fed[Nb_data$data_set2 == i],
                        Nb_data$N_dead_unfed[Nb_data$data_set2 == i],
                        Nb_data$N_survived_fed[Nb_data$data_set2 == i],
                        Nb_data$N_survived_unfed[Nb_data$data_set2 == i])

}
data_list1 = list(N_len_b = 3,
                  N_bins = 4,
                  N_max_time_reps = 10,
                  Nb = array(dim=c(length(Nb_data$N_total2[Nb_data$data_set2 == 1]),N_len_b),
                             data=Nb_data$N_total2),
                  time_b = array(dim=c(length(Nb_data$N_total2[Nb_data$data_set2 == 1]),N_len_b),
                             data= Nb_data$Time_since_intervention*30),
                  len_b = len_b)

library(adegenet)
library(rstan)
library(shinystan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_base <- stan(#file="Q:\\RProjects\\Parameterising_IRS\\Probabilistic_missing_data_model.stan", 
                  file="Q:\\Rprojects\\Parameterising_IRS\\Evidence_synthesis_model_rstan_step1_multinomial.rstan",
                  data=data_list1, 
                  warmup=1000,
                  control = list(adapt_delta = 0.8,
                                 max_treedepth = 20),
                  iter=2000, chains=1)

launch_shinystan(stan_base)
base <- extract(stan_base)


## Pull of the posterior draws generated by the likelihood 
## (can also get these out mathematically from the median/0.975/0.025 estimates for the parameters)
Yd_pred = Yd_pred_u95ci = Yd_pred_l95ci = array(dim=c(365,10))
Yf_pred = Yf_pred_u95ci = Yf_pred_l95ci = array(dim=c(365,10))

Ydf_pred = Ydf_pred_u95ci = Ydf_pred_l95ci = array(dim=c(365,3))
Ydu_pred = Ydu_pred_u95ci = Ydu_pred_l95ci = array(dim=c(365,3))
Ysf_pred = Ysf_pred_u95ci = Ysf_pred_l95ci = array(dim=c(365,3))
Ysu_pred = Ysu_pred_u95ci = Ysu_pred_l95ci = array(dim=c(365,3))

days = 1:365
for(s in 1:10){ ##Number of studies
  for(i in 1:365){ ##Number of days we are predicting across
    Yd_pred[i,s] = quantile(base$Yd_ppc[,s,i],0.5)
    Yd_pred_u95ci[i,s] = quantile(base$Yd_ppc[,s,i],0.975)
    Yd_pred_l95ci[i,s] = quantile(base$Yd_ppc[,s,i],0.025)
    
    Yf_pred[i,s] = quantile(base$Yf_ppc[,s,i],0.5)
    Yf_pred_u95ci[i,s] = quantile(base$Yf_ppc[,s,i],0.975)
    Yf_pred_l95ci[i,s] = quantile(base$Yf_ppc[,s,i],0.025)
  }
}
for(s in 1:3){
  for(i in 1:365){
    Ydf_pred[i,s] = quantile(base$Ydf_ppc[,s,i],0.5)
    Ydf_pred_u95ci[i,s] = quantile(base$Ydf_ppc[,s,i],0.975)
    Ydf_pred_l95ci[i,s] = quantile(base$Ydf_ppc[,s,i],0.025)
    
    Ydu_pred[i,s] = quantile(base$Ydu_ppc[,s,i],0.5)
    Ydu_pred_u95ci[i,s] = quantile(base$Ydu_ppc[,s,i],0.975)
    Ydu_pred_l95ci[i,s] = quantile(base$Ydu_ppc[,s,i],0.025)
    
    Ysf_pred[i,s] = quantile(base$Ysf_ppc[,s,i],0.5)
    Ysf_pred_u95ci[i,s] = quantile(base$Ysf_ppc[,s,i],0.975)
    Ysf_pred_l95ci[i,s] = quantile(base$Ysf_ppc[,s,i],0.025)
    
    Ysu_pred[i,s] = quantile(base$Ysu_ppc[,s,i],0.5)
    Ysu_pred_u95ci[i,s] = quantile(base$Ysu_ppc[,s,i],0.975)
    Ysu_pred_l95ci[i,s] = quantile(base$Ysu_ppc[,s,i],0.025)
  }  
}


plot(Yd_pred[,1] ~ days,ylab="Probability of mosquito outcome",xlab="Time in days",pch="",ylim=c(0,1))
for(i in 1:10){
  polygon(c(days,rev(days)),c(Yd_pred_u95ci[,i],rev(Yd_pred_l95ci[,i])),border=NA,col=transp("blue",0.2))
  lines(Yd_pred[,i] ~ days,lty=1,lwd=2,col="blue")  
}
for(i in 1:3){
  polygon(c(days,rev(days)),c(Ydf_pred_u95ci[,i],rev(Ydf_pred_l95ci[,i])),border=NA,col=transp("orange",0.2))
  lines(Ydf_pred[,i] ~ days,lty=1,lwd=2,col="orange")  
  
  polygon(c(days,rev(days)),c(Ydu_pred_u95ci[,i],rev(Ydu_pred_l95ci[,i])),border=NA,col=transp("aquamarine3",0.2))
  lines(Ydu_pred[,i] ~ days,lty=1,lwd=2,col="aquamarine3")  
}
N_dead_a = data_list1$Xd/data_list1$Xa
N_dead_fed_b = data_list1$Xdf/data_list1$Xb
N_dead_unfed_b = data_list1$Xdu/data_list1$Xb
N_fed_a = data_list1$Xf/data_list1$Xa

plotting_studies_separately_f=function(i,j){
  plot(Yd_pred[,i] ~ days,ylab="Probability of mosquito outcome",xlab="Time in days",pch="",ylim=c(0,1))
  
  polygon(c(days,rev(days)),c(Yd_pred_u95ci[,i],rev(Yd_pred_l95ci[,i])),border=NA,col=transp("blue",0.2))
  lines(Yd_pred[,i] ~ days,lty=1,lwd=2,col="blue")  
  points(N_dead_a[Na_data$data_set1 == i] ~ data_list1$time_a[Na_data$data_set1 == i],pch=20,col="blue")
  
  polygon(c(days,rev(days)),c(Ydf_pred_u95ci[,j],rev(Ydf_pred_l95ci[,j])),border=NA,col=transp("orange",0.2))
  lines(Ydf_pred[,j] ~ days,lty=1,lwd=2,col="orange")  
  points(N_dead_fed_b[Nb_data$data_set2 == j] ~ data_list1$time_b[Nb_data$data_set2 == j],pch=20,col="orange")
  
  polygon(c(days,rev(days)),c(Ydu_pred_u95ci[,j],rev(Ydu_pred_l95ci[,j])),border=NA,col=transp("aquamarine3",0.2))
  lines(Ydu_pred[,j] ~ days,lty=1,lwd=2,col="aquamarine3")  
  points(N_dead_unfed_b[Nb_data$data_set2 == j] ~ data_list1$time_b[Nb_data$data_set2 == j],pch=20,col="aquamarine3")
  
}
par(mfrow = c(3,3))
plotting_studies_separately_f(1,1)
plotting_studies_separately_f(2,2)
plotting_studies_separately_f(3,3)
plotting_studies_separately_f(4,1)
plotting_studies_separately_f(5,1)
plotting_studies_separately_f(6,1)
plotting_studies_separately_f(7,1)
plotting_studies_separately_f(8,1)
plotting_studies_separately_f(9,1)


##Repeating for surviving and fed
plot(Yf_pred[,1] ~ days,ylab="Probability of mosquito outcome",xlab="Time in days",pch="",ylim=c(0,1))
for(i in 1:10){
  polygon(c(days,rev(days)),c(Yf_pred_u95ci[,i],rev(Yf_pred_l95ci[,i])),border=NA,col=transp("darkred",0.2))
  lines(Yf_pred[,i] ~ days,lty=1,lwd=2,col="darkred")  
}
for(i in 1:3){
  polygon(c(days,rev(days)),c(Ysf_pred_u95ci[,i],rev(Ysf_pred_l95ci[,i])),border=NA,col=transp("orange",0.2))
  lines(Ysf_pred[,i] ~ days,lty=1,lwd=2,col="orange")  
  
  polygon(c(days,rev(days)),c(Ysu_pred_u95ci[,i],rev(Ysu_pred_l95ci[,i])),border=NA,col=transp("aquamarine3",0.2))
  lines(Ysu_pred[,i] ~ days,lty=1,lwd=2,col="aquamarine3")  
}
N_dead_a = data_list1$Xd/data_list1$Xa
N_survived_fed_b = data_list1$Xsf/data_list1$Xb
N_survived_unfed_b = data_list1$Xsu/data_list1$Xb
N_fed_a = data_list1$Xf/data_list1$Xa

plotting_studies_separately_f=function(i,j){
  plot(Yf_pred[,i] ~ days,ylab="Probability of mosquito outcome",xlab="Time in days",pch="",ylim=c(0,1))
  
  polygon(c(days,rev(days)),c(Yf_pred_u95ci[,i],rev(Yf_pred_l95ci[,i])),border=NA,col=transp("darkred",0.2))
  lines(Yf_pred[,i] ~ days,lty=1,lwd=2,col="darkred")  
  points(N_fed_a[Na_data$data_set1 == i] ~ data_list1$time_a[Na_data$data_set1 == i],pch=20,col="darkred")
  
  polygon(c(days,rev(days)),c(Ysf_pred_u95ci[,j],rev(Ysf_pred_l95ci[,j])),border=NA,col=transp("orange",0.2))
  lines(Ysf_pred[,j] ~ days,lty=1,lwd=2,col="orange")  
  points(N_survived_fed_b[Nb_data$data_set2 == j] ~ data_list1$time_b[Nb_data$data_set2 == j],pch=20,col="orange")
  
  polygon(c(days,rev(days)),c(Ysu_pred_u95ci[,j],rev(Ysu_pred_l95ci[,j])),border=NA,col=transp("aquamarine3",0.2))
  lines(Ysu_pred[,j] ~ days,lty=1,lwd=2,col="aquamarine3")  
  points(N_survived_unfed_b[Nb_data$data_set2 == j] ~ data_list1$time_b[Nb_data$data_set2 == j],pch=20,col="aquamarine3")
  
}
par(mfrow = c(3,3))
plotting_studies_separately_f(1,1)
plotting_studies_separately_f(2,2)
plotting_studies_separately_f(3,3)
plotting_studies_separately_f(4,1)
plotting_studies_separately_f(5,1)
plotting_studies_separately_f(6,1)
plotting_studies_separately_f(7,1)
plotting_studies_separately_f(8,1)
plotting_studies_separately_f(9,1)
