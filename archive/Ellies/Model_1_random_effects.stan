// bernoulli_logistic transformed data function
data {
  
  
  int<lower=1> N;                  // rows of data
  
  int<lower=0> n_t[N];              // Total number of mosquitoes entering IRS huts
  int<lower=0> d[N];              // # mosquitoes dead and fed sprayed hut
  int<lower=0> ud_f[N];             // # mosquitoes alive and fed sprayed hut
  
  vector<lower=0>[N] time;       // predictor

  int<lower=1> N_IRS;           // IRS treatments
  int<lower=1, upper=N_IRS> IRS[N];
  
}

parameters {
  //Consider death. This is the proportion of mosquitoes dying (d_t) in treated huts (n_t)
  real alpha1[N_IRS];
  real alpha2[N_IRS];
  
  //Consider feeding. This is the proportion of mosquitoes that successfully fed in treatment (f_t)
  real beta1[N_IRS];
  real beta2[N_IRS];
 }

model {
  real sp[N];
  real fp[N];
  
  alpha1 ~ normal(0,10);
  alpha2 ~ normal(0,10);
  
  beta1 ~ normal(0,10);
  beta2 ~ normal(0,10);
 
  for (n in 1:N) {
    sp[n] = alpha1[IRS[n]] + alpha2[IRS[n]] * time[n];
    fp[n] = beta1[IRS[n]]  + beta2[IRS[n]]  * time[n];
  }
  
  d ~ binomial_logit(n_t, sp);
  ud_f ~ binomial_logit(n_t, fp);

}

generated quantities{
  real sp_ppc[N_IRS, 365];
  real fp_ppc[N_IRS, 365];  
  
  for(v in 1:N_IRS){
    for(t in 1:365){
      sp_ppc[v, t] = binomial_rng(365, inv_logit(alpha1[v] + alpha2[v] * t)) / 365.0;
      fp_ppc[v, t] = binomial_rng(365, inv_logit(beta1[v] + beta2[v] * t)) / 365.0;
    }
  }
}
