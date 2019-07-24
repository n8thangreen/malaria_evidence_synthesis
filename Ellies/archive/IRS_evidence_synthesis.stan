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
  real mu0d[365];
  real mu1d[365];
  
  real mu0f[365];
  real mu1f[365];
 
  real mu0df[365];
  real mu1df[365];

  real mu0du[365];
  real mu1du[365];

  real mu0sf[365];
  real mu1sf[365];
  
  real mu0su[365];
  real mu1su[365];
 
}

model {
  real Yd[Na];
  real Yf[Na];

  real Ydf[Nb];
  real Ydu[Nb];
  real Ysf[Nb];
  real Ysu[Nb];

  mu0d ~ normal(0,100);
  tau0d ~ normal(0, 0.0001);
  mu1d ~ normal(0,100);
  tau1d ~ normal(0, 0.0001);
  
  mu0f ~ normal(0,100);
  tau0f ~ normal(0, 0.0001);
  mu1f ~ normal(0,100);
  tau1f ~ normal(0, 0.0001);
  
  mu0df ~ normal(0,100);
  tau0df ~ normal(0, 0.0001);
  mu1df ~ normal(0,100);
  tau1df ~ normal(0, 0.0001);
  
  mu0du ~ normal(0,100);
  tau0du ~ normal(0, 0.0001);
  mu1du ~ normal(0,100);
  tau1du ~ normal(0, 0.0001);
  
  mu0sf ~ normal(0,100);
  tau0sf ~ normal(0, 0.0001);
  mu1sf ~ normal(0,100);
  tau1sf ~ normal(0, 0.0001);
  
  mu0su ~ normal(0,100);
  tau0su ~ normal(0, 0.0001);
  mu1su ~ normal(0,100);
  tau1su ~ normal(0, 0.0001);
  
  for (n in 1:Na) {
    Yd[time_a[n]] = beta0d[IRS_a[n]] + beta1d[IRS_a[n]] * time_a[n];
    Yf[time_a[n]] = beta0f[IRS_a[n]] + beta1f[IRS_a[n]] * time_a[n];
  }
    
  beta0d ~ normal(mu0d, tau0d)
  beta1d ~ normal(mu1d, tau1d)
  
  beta0f ~ normal(mu0f, tau0f)
  beta1f ~ normal(mu1f, tau1f)
  
  row_vector[365] Pd;
  row_vector[365] Pf;
  
  Pd = inv_logit(Yd);
  Xd ~ binomial(Xa, Pd);
  
  Pf = inv_logit(Yf);
  Xf ~ binomial(Xa, Pf);
  
  for (m in 1:Nb) {
    Ydf[time_b[m]] = beta0df[IRS_b[m]] + beta1df[IRS_b[m]] * time_b[m];
    Ydu[time_b[m]] = beta0du[IRS_b[m]] + beta1du[IRS_b[m]] * time_b[m];

    Ysf[time_b[m]] = beta0sf[IRS_b[m]] + beta1sf[IRS_b[m]] * time_b[m];
    Ysu[time_b[m]] = beta0su[IRS_b[m]] + beta1su[IRS_b[m]] * time_b[m];
  }
    
  beta0df ~ normal(mu0df, tau0df)
  beta1df ~ normal(mu1df, tau1df)
  
  beta0du ~ normal(mu0du, tau0du)
  beta1du ~ normal(mu1du, tau1du)
  
  beta0sf ~ normal(mu0sf, tau0sf)
  beta1sf ~ normal(mu1sf, tau1sf)
  
  beta0su ~ normal(mu0su, tau0su)
  beta1su ~ normal(mu1su, tau1su)
  
  row_vector[365] Pdf;
  row_vector[365] Pdu;
  row_vector[365] Psf;
  row_vector[365] Psu;
  
  Pdf = inv_logit(Ydf);
  Xdf ~ binomial(Xb, Pdf);

  Pdu = inv_logit(Ydu);
  Xdu ~ binomial(Xb, Pdu);
  
  Psf = inv_logit(Ysf);
  Xsf ~ binomial(Xb, Psf);
  
  Psu = inv_logit(Ysu);
  Xsu ~ binomial(Xb, Psu);
  
  //multinomial()? categorical()?
  
  // constraints
  
  Pd = Pdf + Pdu
  Pf = Pdf + Psf
  
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