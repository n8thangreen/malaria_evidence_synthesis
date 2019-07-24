// bernoulli_logistic transformed data function
data {

  int<lower=1> N;                  // rows of data giving numbers fed and survived, unfed and survived and dead...##so just datasets with all broken down data
  int<lower=1> M;                  // rows of data giving numbers DEAD OR FED ## so all data listed

  int<lower=0> n1_t[N];             // Total number of mosquitoes entering IRS huts WITH ALL DATA
  int<lower=0> n2_t[M];             // Total number of mosquitoes entering IRS huts WITH AGGREGATED DATA

  int<lower=0> fed_surv_t[N];           // Number of mosquitoes dead and unfed in sprayed huts
  int<lower=0> unfed_surv_t[N];           // Number of mosquitoes dead and unfed in sprayed huts
  
  int<lower=0> deterrence_IRS[M];  // All in sprayed huts
  int<lower=0> ded_t[M];  // All in control and sprayed huts
  int<lower=0> deterrence_total[M];  // All in control and sprayed huts

  vector<lower=0>[N] time1;       // predictor
  vector<lower=0>[M] time2;       // predictor
  
}

parameters {
  //Consider death. This is the proportion of mosquitoes feeding and surviving
  real alpha1;
  real alpha2;
  
  //Consider feeding. This is the proportion of mosquitoes that do not feed but survive
  real beta1;
  real beta2;
  
  //Consider feeding. This is the proportion of mosquitoes that die or deter
  real theta1;
  real theta2;

  real omega1;
  real omega2;
  
}

model {

  real pfs[N];              // fed and survived
  real pufs[N];             // unfed and survived
  real pd[M];               // dead
  real det_t[M]; //deterrence

  alpha1 ~ normal(0,10);
  alpha2 ~ normal(0,10);
  
  beta1 ~ normal(0,10);
  beta2 ~ normal(0,10);
  
  theta1 ~ normal(0,10);
  theta2 ~ normal(0,10);

  omega1 ~ normal(0,10);
  omega2 ~ normal(0,10);

  for (n in 1:N) {
    pfs[n] = alpha1  + alpha2 * time1[n]; //fitting for time
    pufs[n] = beta1  + beta2 * time1[n];  //the fed and survived, unfed and survived 
    pd[n] = 1 - pfs[n] - pufs[n];        //and the consequence so that those that died added to those that surive sum to 1  
}
 
  for(m in 1:M){
    pd[m] = theta1 + theta2 * time2[m];    
    det_t[m] = omega1 + omega2 * time2[m];    
  }
  
  for (n in 1:N) {
    fed_surv_t[n] ~ binomial_logit(n1_t[n], pfs); //FROM THE FINESCALE DATA
    unfed_surv_t[n] ~ binomial_logit(n1_t[n], pufs);
  }
  
  for (m in 1:M) {
    ded_t[m] ~ binomial_logit(n2_t[m], pd); //FROM THE AGGREGATED DATA  
 //   deterrence_IRS[m] ~ binomial_logit(deterrence_total, det_t);
    
  }
  
  
 
  
  }
