// bernoulli_logistic transformed data function
data {
  
  int<lower=1> N;                  // rows of data
  
    int<lower=0> n_t[N];              // Total number of mosquitoes entering IRS huts
//  int<lower=0> d_f[N];              // # mosquitoes dead and fed sprayed hut
//  int<lower=0> d_uf[N];             // # mosquitoes dead and unfed sprayed hut
//  int<lower=0> ud_f[N];             // # mosquitoes alive and fed sprayed hut
//  int<lower=0> ud_uf[N];            // # mosquitoes alive and unfed sprayed hut
  
  int<lower=0> d[N];              // # mosquitoes dead 
  int<lower=0> ud_f[N];              // # mosquitoes alive and fed 

  vector<lower=0>[N] time;       // predictor

}

parameters {
  //Consider death. Parameters for probability that mosquitoes are dead
  // Dead and fed (d_f) in treated huts (n_t)
//  real alpha1;
//  real alpha2;
 
  // Dead and unfed (d_uf)
//  real alpha3;
//  real alpha4;
  
  // Dead 
  real alpha1;
  real alpha2;

  //Consider feeding. Parameters for probability that mosquitoes fed and survived
  // Alive and fed (ud_f) in treatment (n_t)
  real beta1;
  real beta2;
  
  //Consider feeding. Parameters for probability that mosquitoes fed and survived
  // Alive and unfed (ud_uf)
//  real omega1;
//  real omega2;
}

model {
//  real dfp[N];
//  real dufp[N];
//  real fp[N];
//  real ufp[N];
 
  real dp[N];
  real fp[N];

  alpha1 ~ normal(0,100);
  alpha2 ~ normal(0,100);
  
//  alpha3 ~ normal(0,100);
//  alpha4 ~ normal(0,100);
 
  beta1 ~ normal(0,100);
  beta2 ~ normal(0,100);
  
//  omega1 ~ normal(0,100);
//  omega2 ~ normal(0,100);

  for (n in 1:N) {
//    dfp[n] = alpha1  + alpha2 * time[n];
//    dufp[n] = alpha3  + alpha4 * time[n];
//    fp[n] = beta1  + beta2 * time[n];
//    ufp[n] = omega1  + omega2 * time[n];
    dp[n] = alpha1  + alpha2 * time[n];
    fp[n] = beta1  + beta2 * time[n];
  }
  
//  d_f ~ binomial_logit(n_t, dfp);
//  d_uf ~ binomial_logit(n_t, dufp);
//  ud_f ~ binomial_logit(n_t, fp);
//  ud_uf ~ binomial_logit(n_t, ufp);

  d ~ binomial_logit(n_t, dp);
  ud_f ~ binomial_logit(n_t, fp);
}
