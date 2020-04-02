// bernoulli_logistic transformed data function
data {
  
  int<lower=1> Na;                       // The number of rows of data for aggregated set
  
  int<lower=0> Xa[Na];                   // Total number of mosquitoes entering IRS huts
  int<lower=0> Xd[Na];                   // Number mosquites dead sprayed hut
  int<lower=0> Xf[Na];                   // Number mosquites fed sprayed hut
  
  vector<lower=0>[Na] time_a;            // time predictor for aggregated set

  int<lower=1> N_IRS_a;                  // IRS treatments for aggregated data (this is 10)
  int<lower=1, upper=N_IRS_a> IRS_a[Na]; // Rep for each IRS treatment


  int<lower=1> Nb;                // The number of rows of data for full set
  
  int<lower=0> Xb[Nb];             // Total number of mosquitoes entering IRS huts
  int<lower=0> Xdf[Nb];             // Number mosquites fed and dead sprayed hut
  int<lower=0> Xdu[Nb];             // Number mosquites unfed and dead sprayed hut
  int<lower=0> Xsf[Nb];             // Number mosquites fed and survived sprayed hut
  int<lower=0> Xsu[Nb];             // Number mosquites unfed and survived sprayed hut
   
  vector<lower=0>[Nb] time_b;            // time predictor for full set

  int<lower=1> N_IRS_b;                  // IRS treatments for full data (this is 3)
  int<lower=1, upper=N_IRS_b> IRS_b[Nb]; // Rep for each IRS treatment
  

}

parameters {
  //Consider death. This is the proportion of mosquitoes dying (d_t) in treated huts (n_t)
  real beta0d[N_IRS_a];
  real beta1d[N_IRS_a];
  
  //Consider feeding. This is the proportion of mosquitoes that successfully fed in treatment (f_t)
  real beta0f[N_IRS_a];
  real beta1f[N_IRS_a];
 
 
  //Consider dead and fed. 
  real beta0df[N_IRS_b];
  real beta1df[N_IRS_b];
  
  //Consider dead and unfed. 
  real beta0du[N_IRS_b];
  real beta1du[N_IRS_b];

  //Consider survived and fed. 
  real beta0sf[N_IRS_b];
  real beta1sf[N_IRS_b];
  
  //Consider survived and unfed.
  real beta0su[N_IRS_b];
  real beta1su[N_IRS_b];
 
}

model {
  real Yd[Na];
  real Yf[Na];

  real Ydf[Nb];
  real Ydu[Nb];
  real Ysf[Nb];
  real Ysu[Nb];
  

  beta0d ~ normal(0,100);
  beta1d ~ normal(0,100);
  
  beta0f ~ normal(0,100);
  beta1f ~ normal(0,100);


  beta0df ~ normal(0,100);
  beta1df ~ normal(0,100);

  beta0du ~ normal(0,100);
  beta1du ~ normal(0,100);

  beta0sf ~ normal(0,100);
  beta1sf ~ normal(0,100);

  beta0su ~ normal(0,100);
  beta1su ~ normal(0,100);
  
  for (na in 1:Na) {
    Yd[na] = beta0d[IRS_a[na]] + beta1d[IRS_a[na]] * time_a[na];
    Yf[na] = beta0f[IRS_a[na]] + beta1f[IRS_a[na]] * time_a[na];
  }
    
  Xd ~ binomial_logit(Xa, Yd);
  Xf ~ binomial_logit(Xa, Yf);

  for (nb in 1:Nb) {
    Ydf[nb] = beta0df[IRS_b[nb]] + beta1df[IRS_b[nb]] * time_b[nb];
    Ydu[nb] = beta0du[IRS_b[nb]] + beta1du[IRS_b[nb]] * time_b[nb];

    Ysf[nb] = beta0sf[IRS_b[nb]] + beta1sf[IRS_b[nb]] * time_b[nb];
    Ysu[nb] = beta0su[IRS_b[nb]] + beta1su[IRS_b[nb]] * time_b[nb];
  }
    
  Xdf ~ binomial_logit(Xb, Ydf);
  Xdu ~ binomial_logit(Xb, Ydu);
 
  Xsf ~ binomial_logit(Xb, Ysf);
  Xsu ~ binomial_logit(Xb, Ysu);
 
}
generated quantities{
  real Yd_ppc[N_IRS_a, 365];
  real Yf_ppc[N_IRS_a, 365];

  real Ydf_ppc[N_IRS_b, 365];
  real Ydu_ppc[N_IRS_b, 365];
  real Ysf_ppc[N_IRS_b, 365];
  real Ysu_ppc[N_IRS_b, 365];
  
  for(va in 1:N_IRS_a){
    for(t in 1:365){
      Yd_ppc[va, t] = binomial_rng(365, inv_logit(beta0d[va] + beta1d[va] * t)) / 365.0;
      Yf_ppc[va, t] = binomial_rng(365, inv_logit(beta0f[va] + beta1f[va] * t)) / 365.0;
      
    }
  }

  for(vb in 1:N_IRS_b){
    for(t in 1:365){
      Ydf_ppc[vb, t] = binomial_rng(365, inv_logit(beta0df[vb] + beta1df[vb] * t)) / 365.0;
      Ydu_ppc[vb, t] = binomial_rng(365, inv_logit(beta0du[vb] + beta1du[vb] * t)) / 365.0;

      Ysf_ppc[vb, t] = binomial_rng(365, inv_logit(beta0sf[vb] + beta1sf[vb] * t)) / 365.0;
      Ysu_ppc[vb, t] = binomial_rng(365, inv_logit(beta0su[vb] + beta1su[vb] * t)) / 365.0;
      
    }
  }
}