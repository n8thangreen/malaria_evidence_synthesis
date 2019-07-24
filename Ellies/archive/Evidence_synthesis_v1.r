#######################################################
###
###
### Mock data for statistical model

Na_data = read.csv("H:/Ellie/Evidence synthesis/Evidence synthesis data_aggregated_N2.csv",header=TRUE)
Nb_data = read.csv("H:/Ellie/Evidence synthesis/Evidence synthesis data_aggregated_N4.csv",header=TRUE)

data_list1 = list(N_studies_a = length(unique(Na_data$study_count)),
                  len_a = nrow(Na_data),
                  X_a = structure(c(Na_data$Ntotaldied_IRS,Na_data$Nbloodfed_IRS),
                                  .Dim = c(nrow(Na_data),2)),
                  Na = Na_data$Ntotalfemalemosq_IRS,
                  time_a = Na_data$Months_since_IRS,
                  
                  N_studies_b = length(unique(Nb_data$study_count)),
                  len_b = nrow(Nb_data),
                  X_b = structure(c(Nb_data$Nsf + Nb_data$Nsfe, ### Success_fed
                                    Nb_data$Ndf + Nb_data$Ndfe, ### Dead_fed 
                                    Nb_data$Nsn + Nb_data$Nsne, ### Success_unfed
                                    Nb_data$Ndn + Nb_data$Ndne),### Dead_unfed
                                  .Dim = c(nrow(Nb_data),4)),
                  Nb = Nb_data$Ntotalfemalemosq_IRS,
                  time_b = Nb_data$Months_since_IRS) ## added time as think will be needed


library(adegenet)
library(rstan)
library(shinystan)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

stan_base <- stan(file="[INSERT YOUR MODEL FILE HERE].rstan",
                  data=data_list1, 
                  warmup=1000,
                  control = list(adapt_delta = 0.8,
                                 max_treedepth = 20),
                  iter=2000, chains=1) ## Keep chains 1 until happy then 4

launch_shinystan(stan_base)
base <- extract(stan_base)

Na_data$prop_killed = Na_data$Ntotaldied_IRS/Na_data$Ntotalfemalemosq_IRS
Nb_data$prop_killed = Nb_data$Ntotaldied_IRS/Nb_data$Ntotalfemalemosq_IRS

Na_data$prop_fed = Na_data$Nbloodfed_IRS/Na_data$Ntotalfemalemosq_IRS
Nb_data$prop_fed = Nb_data$Nbloodfed_IRS/Nb_data$Ntotalfemalemosq_IRS

Nb_data$prop_killed_fed = (Nb_data$Ndf+Nb_data$Ndfe)/Nb_data$Ntotalfemalemosq_IRS
Nb_data$prop_live_fed = (Nb_data$Nsf+Nb_data$Nsfe)/Nb_data$Ntotalfemalemosq_IRS

par(mfrow = c(2,2))
plot(Na_data$prop_killed ~ Na_data$Months_since_IRS,pch="",ylim=c(0,1),yaxt="n",
     ylab="Proportion of mosquitoes killed", xlab = "Time in months",xlim=c(0,12),xaxt="n",cex.axis=1.6,
     cex.lab=1.6)
axis(1,at=0:12,cex.axis=1.6)
axis(2,las=2,at=seq(0,1,0.2),cex.axis=1.6)

for(i in 1:length(unique(Nb_data$Mosquito))){
  points(Nb_data$prop_killed[Nb_data$Mosquito == unique(Nb_data$Mosquito)[i]] ~ 
           Nb_data$Months_since_IRS[Nb_data$Mosquito == unique(Nb_data$Mosquito)[i]],
         pch=19,col="aquamarine",cex=2)
}
for(i in 1:length(unique(Na_data$Mosquito))){
  points(Na_data$prop_killed[Na_data$Mosquito == unique(Na_data$Mosquito)[i]] ~ 
           Na_data$Months_since_IRS[Na_data$Mosquito == unique(Na_data$Mosquito)[i]],
         pch=i,col="blue",cex=2)
}

plot(Na_data$prop_fed ~ Na_data$Months_since_IRS,pch="",ylim=c(0,1),yaxt="n",
     ylab="Proportion of mosquitoes blood-fed", xlab = "Time in months",xlim=c(0,12),xaxt="n",cex.axis=1.6,
     cex.lab=1.6)
axis(1,at=0:12,cex.axis=1.6)
axis(2,las=2,at=seq(0,1,0.2),cex.axis=1.6)
for(i in 1:length(unique(Nb_data$Mosquito))){
  points(Nb_data$prop_fed[Nb_data$Mosquito == unique(Nb_data$Mosquito)[i]] ~ 
           Nb_data$Months_since_IRS[Nb_data$Mosquito == unique(Nb_data$Mosquito)[i]],
         pch=19,col="aquamarine",cex=2)
}
for(i in 1:length(unique(Na_data$Mosquito))){
  points(Na_data$prop_fed[Na_data$Mosquito == unique(Na_data$Mosquito)[i]] ~ 
           Na_data$Months_since_IRS[Na_data$Mosquito == unique(Na_data$Mosquito)[i]],
         pch=i,col="blue",cex=2)
}

plot(Nb_data$prop_fed ~ Nb_data$Months_since_IRS,pch="",ylim=c(0,1),yaxt="n",
     ylab="Proportion of mosquitoes blood-fed and killed", xlab = "Time in months",xlim=c(0,12),xaxt="n",cex.axis=1.6,
     cex.lab=1.6)
axis(1,at=0:12,cex.axis=1.6)
axis(2,las=2,at=seq(0,1,0.2),cex.axis=1.6)
for(i in 1:length(unique(Nb_data$Mosquito))){
  points(Nb_data$prop_killed_fed[Nb_data$Mosquito == unique(Nb_data$Mosquito)[i]] ~ 
           Nb_data$Months_since_IRS[Nb_data$Mosquito == unique(Nb_data$Mosquito)[i]],
         pch=19,col="aquamarine",cex=2)
}

plot(Nb_data$prop_fed ~ Nb_data$Months_since_IRS,pch="",ylim=c(0,1),yaxt="n",
     ylab="Proportion of mosquitoes blood-fed and survived", xlab = "Time in months",xlim=c(0,12),xaxt="n",cex.axis=1.6,
     cex.lab=1.6)
axis(1,at=0:12,cex.axis=1.6)
axis(2,las=2,at=seq(0,1,0.2),cex.axis=1.6)
for(i in 1:length(unique(Nb_data$Mosquito))){
  points(Nb_data$prop_live_fed[Nb_data$Mosquito == unique(Nb_data$Mosquito)[i]] ~ 
           Nb_data$Months_since_IRS[Nb_data$Mosquito == unique(Nb_data$Mosquito)[i]],
         pch=19,col="aquamarine",cex=2)
}