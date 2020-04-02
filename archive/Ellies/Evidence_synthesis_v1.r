#######################################################
###
###
### Mock data for statistical model

Na_data = read.csv("H:/Ellie/Evidence synthesis/Evidence synthesis data_aggregated_N2.csv",header=TRUE)
Nb_data = read.csv("H:/Ellie/Evidence synthesis/Evidence synthesis data_aggregated_N4.csv",header=TRUE)
#############################
### Plotting data

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


### Data for model 1
data_list1 = list(N = nrow(Na_data),                   ## rows of data
                  
                  n_t = Na_data$Ntotalfemalemosq_IRS,## Total number of mosquitoes entering IRS huts
                  d = Na_data$Ntotaldied_IRS,        ## mosquitoes dead 
                  ud_f = round(Na_data$Nbloodfed_IRS * 
                                 (1 - Na_data$Ntotaldied_IRS/Na_data$Ntotalfemalemosq_IRS),0),  ## mosquitoes alive and fed sprayed hut
                  
                  time = Na_data$Months_since_IRS*30)## predictor

data_list1_rf = list(N = nrow(Na_data),                   ## rows of data
                  
                  n_t = Na_data$Ntotalfemalemosq_IRS,## Total number of mosquitoes entering IRS huts
                  d = Na_data$Ntotaldied_IRS,        ## mosquitoes dead 
                  ud_f = round(Na_data$Nbloodfed_IRS * 
                                 (1 - Na_data$Ntotaldied_IRS/Na_data$Ntotalfemalemosq_IRS),0),  ## mosquitoes alive and fed sprayed hut
                  
                  time = Na_data$Months_since_IRS*30,## predictor
                  
                  N_IRS = length(unique(Na_data$study_count)),
                  IRS = Na_data$study_count)


### Data for model 2
data_list2 = list(N = nrow(Nb_data),                   ## rows of data
                  
                  n_t = Nb_data$Ntotalfemalemosq_IRS,## Total number of mosquitoes entering IRS huts
                  d_f = Nb_data$Ndfe + Nb_data$Ndf,        ## mosquitoes dead and fed (ignoring whether exited or not)
                  d_uf = Nb_data$Ndn + Nb_data$Ndne,        ## mosquitoes dead and unfed (ignoring whether exited or not)
                  ud_f = Nb_data$Nsf + Nb_data$Nsfe,        ## mosquitoes undead and fed (ignoring whether exited or not)
                  ud_uf = Nb_data$Nsn + Nb_data$Nsne,        ## mosquitoes undead and fed (ignoring whether exited or not)
                  
                  d = Nb_data$Ndfe + Nb_data$Ndf + Nb_data$Ndn + Nb_data$Ndne, ## all dead
                  
                  time = Na_data$Months_since_IRS*30)## predictor

data_list2 = list(N = nrow(Nb_data),                   ## rows of data
                  
                  n_t = Nb_data$Ntotalfemalemosq_IRS,## Total number of mosquitoes entering IRS huts
                  d_f = Nb_data$Ndfe + Nb_data$Ndf,        ## mosquitoes dead and fed (ignoring whether exited or not)
                  d_uf = Nb_data$Ndn + Nb_data$Ndne,        ## mosquitoes dead and unfed (ignoring whether exited or not)
                  ud_f = Nb_data$Nsf + Nb_data$Nsfe,        ## mosquitoes undead and fed (ignoring whether exited or not)
                  ud_uf = Nb_data$Nsn + Nb_data$Nsne,        ## mosquitoes undead and fed (ignoring whether exited or not)
                  
                  d = Nb_data$Ndfe + Nb_data$Ndf + Nb_data$Ndn + Nb_data$Ndne, ## all dead
                  
                  time = Nb_data$Months_since_IRS*30, ## predictor
                  
                  N_IRS = length(unique(Nb_data$study_count)),
                  IRS = Nb_data$study_count)

### Data for model 3
data_list3 = list(N_studies_a = length(unique(Na_data$study_count)),
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
                  time_b = Nb_data$Months_since_IRS,
                  studyid_b = Nb_data$study_count) ## added time as think will be needed



############################################################
## Fitting models
library(rstan)
library(adegenet)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())## Actellic 300 CS
library(shinystan)

### Model 1
stan1 <- stan(file="H:\\Ellie\\Evidence synthesis\\Model_1_assuming_independence.stan", 
              data=data_list1, 
              warmup=500,
              control = list(adapt_delta = 0.9,
                             max_treedepth = 20),
              iter=1000, chains=4)

### Model 1
stan1rf <- stan(file="H:\\Ellie\\Evidence synthesis\\Model_1_random_effects.stan", 
              data=data_list1_rf, 
              warmup=500,
              control = list(adapt_delta = 0.9,
                             max_treedepth = 20),
              iter=1000, chains=4)

### Model 2
stan2 <- stan(file="H:\\Ellie\\Evidence synthesis\\Model_2_full_subset_data.stan", 
              data=data_list2, 
              warmup=500,
              control = list(adapt_delta = 0.9,
                             max_treedepth = 20),
              iter=1000, chains=4)

### Model 2
stan2rf <- stan(file="H:\\Ellie\\Evidence synthesis\\Model_1_random_effects.stan", 
                data=data_list2, 
              warmup=500,
              control = list(adapt_delta = 0.9,
                             max_treedepth = 20),
              iter=1000, chains=4)


### Model 3 [*Currently in WinBUGS]
stan3 <- stan(file="H:\\Ellie\\Evidence synthesis\\Model_3.stan",
                  data=data_list1, 
                  warmup=1000,
                  control = list(adapt_delta = 0.8,
                                 max_treedepth = 20),
                  iter=2000, chains=1) ## Keep chains 1 until happy then 4


############################################################
## Checking models

launch_shinystan(stan1)
base1 <- extract(stan1)

launch_shinystan(stan1rf)
base1rf <- extract(stan1rf)

launch_shinystan(stan2)
base2 <- extract(stan2)

launch_shinystan(stan2rf)
base2rf <- extract(stan2rf)

############################################################
## plotting predictions from models

Na_data$prop_killed ## total killed
Nb_data$prop_killed ## total killed

Na_data$prop_live_fed = round(Na_data$Nbloodfed_IRS * 
                                (1 - Na_data$Ntotaldied_IRS/Na_data$Ntotalfemalemosq_IRS),0)/Na_data$Ntotalfemalemosq_IRS ## alive and fed assuming independence
Nb_data$prop_live_fed = (Nb_data$Nsf+Nb_data$Nsfe)/Nb_data$Ntotalfemalemosq_IRS ## alive and fed


par(mfrow = c(2,2)) ## plot out a row for ecah model - data and overlaid model predictions with 50% CrI showing

Na_data$time = Na_data$Months_since_IRS*30
Nb_data$time = Nb_data$Months_since_IRS*30
## Model 1 assuming independence
plot(Na_data$prop_killed ~ Na_data$time,pch="",ylim=c(0,1),yaxt="n",
     main = "Model 1",
     ylab="Proportion of mosquitoes killed", xlab = "Time in months",xlim=c(0,365),xaxt="n",cex.axis=1.6,
     cex.lab=1.6)
axis(1,at=seq(1,365,30),labels = 0:12, cex.axis=1.6)
axis(2,las=2,at=seq(0,1,0.2),cex.axis=1.6)

for(i in 1:length(unique(Na_data$Mosquito))){
  points(Na_data$prop_killed[Na_data$Mosquito == unique(Na_data$Mosquito)[i]] ~ 
           Na_data$time[Na_data$Mosquito == unique(Na_data$Mosquito)[i]],
         pch=i,col="blue",cex=2)
}
time = 1:365
pred_mort = 1/(1 + exp(-mean(base1$alpha1) - mean(base1$alpha2)*time))
pred_mort_crI = array(dim=c(365,2000))
for(i in 1:length(base1$alpha1)){
  pred_mort_crI[,i] = 1/(1 + exp(-base1$alpha1[i] - base1$alpha2[i]*time))
  
}

## Add in random effects predictions
pred_mortrf = array(dim=c(365,ncol(base1rf$alpha1)))
for(j in 1:ncol(base1rf$alpha1)){
  pred_mortrf[,j] = 1/(1 + exp(-mean(base1rf$alpha1[,j]) - mean(base1rf$alpha2[,j])*time))
}
for(j in 1:ncol(base1rf$alpha1)){
  lines(pred_mortrf[,j] ~ time,col="blue")
}

segments(x0=0,x1=90,y0=pred_mort[90],y1=pred_mort[90],lty=2,lwd=2)
segments(x0=90,x1=90,y0=0,y1=pred_mort[90],lty=2,lwd=2)

segments(x0=0,x1=180,y0=pred_mort[180],y1=pred_mort[180],lty=2,lwd=2)
segments(x0=180,x1=180,y0=0,y1=pred_mort[180],lty=2,lwd=2)

segments(x0=0,x1=270,y0=pred_mort[270],y1=pred_mort[270],lty=2,lwd=2)
segments(x0=270,x1=270,y0=0,y1=pred_mort[270],lty=2,lwd=2)

for(i in 1:length(base1$alpha1)){
  lines(pred_mort_crI[,i] ~ time,col=adegenet::transp("grey",0.2))
}
lines(pred_mort ~ time)


plot(Na_data$prop_live_fed ~ Na_data$time,pch="",ylim=c(0,1),yaxt="n",
     ylab="Proportion of mosquitoes successfully fed", xlab = "Time in months",xlim=c(0,365),xaxt="n",cex.axis=1.6,
     cex.lab=1.6)
axis(1,at=seq(1,365,30),labels = 0:12, cex.axis=1.6)
axis(2,las=2,at=seq(0,1,0.2),cex.axis=1.6)

for(i in 1:length(unique(Na_data$Mosquito))){
  points(Na_data$prop_live_fed[Na_data$Mosquito == unique(Na_data$Mosquito)[i]] ~ 
           Na_data$time[Na_data$Mosquito == unique(Na_data$Mosquito)[i]],
         pch=i,col="grey",cex=2)
}
time = 1:365
pred_fed = 1/(1 + exp(-mean(base1$beta1) - mean(base1$beta2)*time))
pred_fed_crI = array(dim=c(365,2000))
for(i in 1:length(base1$alpha1)){
  pred_fed_crI[,i] = 1/(1 + exp(-base1$beta1[i] - base1$beta2[i]*time))
  
}

## Add in random effects predictions
pred_fedrf = array(dim=c(365,ncol(base1rf$beta1)))
for(j in 1:ncol(base1rf$beta1)){
  pred_fedrf[,j] = 1/(1 + exp(-mean(base1rf$beta1[,j]) - mean(base1rf$beta2[,j])*time))
}
for(j in 1:ncol(base1rf$alpha1)){
  lines(pred_fedrf[,j] ~ time,col="grey")
}

segments(x0=0,x1=90,y0=pred_fed[90],y1=pred_fed[90],lty=2,lwd=2)
segments(x0=90,x1=90,y0=0,y1=pred_fed[90],lty=2,lwd=2)

segments(x0=0,x1=180,y0=pred_fed[180],y1=pred_fed[180],lty=2,lwd=2)
segments(x0=180,x1=180,y0=0,y1=pred_fed[180],lty=2,lwd=2)

segments(x0=0,x1=270,y0=pred_fed[270],y1=pred_fed[270],lty=2,lwd=2)
segments(x0=270,x1=270,y0=0,y1=pred_fed[270],lty=2,lwd=2)

for(i in 1:length(base1$alpha1)){
  lines(pred_fed_crI[,i] ~ time,col=adegenet::transp("grey",0.2))
}
lines(pred_fed ~ time)


## Model 2 assuming independence
plot(Na_data$prop_killed ~ Na_data$time,pch="",ylim=c(0,1),yaxt="n",
     main = "Model 2",
     ylab="Proportion of mosquitoes killed", xlab = "Time in months",xlim=c(0,365),xaxt="n",cex.axis=1.6,
     cex.lab=1.6)
axis(1,at=seq(1,365,30),labels = 0:12, cex.axis=1.6)
axis(2,las=2,at=seq(0,1,0.2),cex.axis=1.6)

for(i in 1:length(unique(Nb_data$Mosquito))){
  points(Nb_data$prop_killed[Nb_data$Mosquito == unique(Nb_data$Mosquito)[i]] ~ 
           Nb_data$time[Nb_data$Mosquito == unique(Nb_data$Mosquito)[i]],
         pch=19,col="blue",cex=2)
}

time = 1:365
pred_mort = 1/(1 + exp(-mean(base2$alpha1) - mean(base2$alpha2)*time))
pred_mort_crI = array(dim=c(365,2000))
for(i in 1:length(base1$alpha1)){
  pred_mort_crI[,i] = 1/(1 + exp(-base2$alpha1[i] - base2$alpha2[i]*time))
  
}

## Add in random effects predictions
pred_fedrf = array(dim=c(365,ncol(base2rf$alpha1)))
for(j in 1:ncol(base2rf$alpha1)){
  pred_fedrf[,j] = 1/(1 + exp(-mean(base2rf$alpha1[,j]) - mean(base2rf$alpha2[,j])*time))
}
for(j in 1:ncol(base2rf$alpha1)){
  lines(pred_fedrf[,j] ~ time,col="blue")
}

segments(x0=0,x1=90,y0=pred_mort[90],y1=pred_mort[90],lty=2,lwd=2)
segments(x0=90,x1=90,y0=0,y1=pred_mort[90],lty=2,lwd=2)

segments(x0=0,x1=180,y0=pred_mort[180],y1=pred_mort[180],lty=2,lwd=2)
segments(x0=180,x1=180,y0=0,y1=pred_mort[180],lty=2,lwd=2)

segments(x0=0,x1=270,y0=pred_mort[270],y1=pred_mort[270],lty=2,lwd=2)
segments(x0=270,x1=270,y0=0,y1=pred_mort[270],lty=2,lwd=2)

for(i in 1:length(base2$alpha1)){
  lines(pred_mort_crI[,i] ~ time,col=adegenet::transp("grey",0.2))
}
lines(pred_mort ~ time)


plot(Na_data$prop_live_fed ~ Na_data$time,pch="",ylim=c(0,1),yaxt="n",
     ylab="Proportion of mosquitoes successfully fed", xlab = "Time in months",xlim=c(0,365),xaxt="n",cex.axis=1.6,
     cex.lab=1.6)
axis(1,at=seq(1,365,30),labels = 0:12, cex.axis=1.6)
axis(2,las=2,at=seq(0,1,0.2),cex.axis=1.6)

for(i in 1:length(unique(Nb_data$Mosquito))){
  points(Nb_data$prop_live_fed[Nb_data$Mosquito == unique(Nb_data$Mosquito)[i]] ~ 
           Nb_data$time[Nb_data$Mosquito == unique(Nb_data$Mosquito)[i]],
         pch=19,col="blue",cex=2)
}

time = 1:365
pred_fed = 1/(1 + exp(-mean(base2$beta1) - mean(base2$beta2)*time))
pred_fed_crI = array(dim=c(365,2000))
for(i in 1:length(base1$alpha1)){
  pred_fed_crI[,i] = 1/(1 + exp(-base2$beta1[i] - base2$beta2[i]*time))
  
}

## Add in random effects predictions
pred_fedrf = array(dim=c(365,ncol(base2rf$beta1)))
for(j in 1:ncol(base2rf$beta1)){
  pred_fedrf[,j] = 1/(1 + exp(-mean(base2rf$beta1[,j]) - mean(base2rf$beta2[,j])*time))
}
for(j in 1:ncol(base2rf$alpha1)){
  lines(pred_fedrf[,j] ~ time,col="blue")
}

segments(x0=0,x1=90,y0=pred_fed[90],y1=pred_fed[90],lty=2,lwd=2)
segments(x0=90,x1=90,y0=0,y1=pred_fed[90],lty=2,lwd=2)

segments(x0=0,x1=180,y0=pred_fed[180],y1=pred_fed[180],lty=2,lwd=2)
segments(x0=180,x1=180,y0=0,y1=pred_fed[180],lty=2,lwd=2)

segments(x0=0,x1=270,y0=pred_fed[270],y1=pred_fed[270],lty=2,lwd=2)
segments(x0=270,x1=270,y0=0,y1=pred_fed[270],lty=2,lwd=2)

for(i in 1:length(base2$alpha1)){
  lines(pred_fed_crI[,i] ~ time,col=adegenet::transp("grey",0.2))
}
lines(pred_fed ~ time)
