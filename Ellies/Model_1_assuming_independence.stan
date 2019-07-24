// bernoulli_logistic transformed data function
data {
  
  int<lower=1> N;                  // rows of data
  
  int<lower=0> n_t[N];              // Total number of mosquitoes entering IRS huts
  int<lower=0> d[N];              // # mosquitoes dead and fed sprayed hut
  int<lower=0> ud_f[N];             // # mosquitoes alive and fed sprayed hut
  
  vector<lower=0>[N] time;       // predictor

}

parameters {
  //Consider death. Parameters for probability that mosquitoes are dead
  // Dead (d) in treated huts (n_t)
  real alpha1;
  real alpha2;
 
  //Consider feeding. Parameters for probability that mosquitoes fed and survived
  // Alive and fed (ud_f) in treatment (n_t)
  real beta1;
  real beta2;
}

model {
  real dp[N];
  real fp[N];
  
  alpha1 ~ normal(0,100);
  alpha2 ~ normal(0,100);
 
  beta1 ~ normal(0,100);
  beta2 ~ normal(0,100);

  for (n in 1:N) {
    dp[n] = alpha1  + alpha2 * time[n];
    fp[n] = beta1  + beta2 * time[n];
  }
  
  d ~ binomial_logit(n_t, dp);
  ud_f ~ binomial_logit(n_t, fp);
}
