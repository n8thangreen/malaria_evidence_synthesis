data {
  int<lower=1> N_reponse; // Number of reponse categories
  
  // Number of experiments with aggregated responses
  int<lower=1> N_agg_exp;
  // Number of successes in first aggregation pattern
  int<lower=0> k1_agg[N_agg_exp]; 
  // Number of successes in second aggregation pattern
  int<lower=0> k2_agg[N_agg_exp];
  // Number of trials
  int<lower=1> N_trials_agg[N_agg_exp];
  
  int<lower=1> N_studies;
  int<lower=1, upper=N_studies> study_idx_agg[N_agg_exp]; // Study index
  
  real time_agg[N_agg_exp]; // Time of what?  In what units?
  
  // Number of experiments with individual responses
  int<lower=1> N_indiv_exp;
  // Counts of each response
  int<lower=1> N_responses_indiv[N_indiv_exp, N_reponse];

  int<lower=1, upper=N_studies> study_idx_indiv[N_indiv_exp]; // Study index

  real time_indiv[N_indiv_exp]; // Time of what?  In what units?
}

parameters {
  real mu_alpha;           // Intercept population location
  real<lower=0> tau_alpha; // Intercept population scale

  real alpha_tilde[study_idx, N_groups - 1]; // Noncentered intercepts
  
  //changed?
  real mu_beta;           // Slope population location
  real<lower=0> tau_beta // Slope population scale
  
  real beta_tilde[study_id, N_groups - 1]; // Noncentered slopes
}

model {
  // priors
  mu_alpha ~ normal(0, ???);
  tau_alpha ~ normal(0, ???);
  alpha_tilde ~ normal(0, 1);
  
  mu_beta ~ normal(0, ???);
  tau_beta ~ normal(0, ???);
  beta_tilde ~ normal(0, 1);

  // aggregate
  for (n in 1:N_agg_exp) {
    real eta[N_groups]; // Latent effect for each response
    real p[N_groups];   // Response probabilities
    
    eta[1] = 0;
    for (g in 2:N_groups) {
      real alpha = mu_alpha + tau_alpha * alpha_tilde[study_idx_agg[n], g - 1];
      real beta = mu_beta + tau_beta * beta_tilde[study_idx_agg[n], g - 1];
      eta[g] = alpha + beta * time_agg[n];
    }
    
    p = softmax(eta);
    
    k1_agg[n] ~ binomial(N_trials_agg[n], p[2] + p[4]);
    k2_agg[n] ~ binomial(N_trials_agg[n], p[1] + p[2]);
  }
  
  // individual
  for (n in 1:N_indiv_exp) {
    real eta[N_groups];
    real p[N_groups];
    
    eta[1] = 0;
    for (g in 2:N_groups) {
      real alpha = mu_alpha + tau_alpha * alpha_tilde[study_idx_indiv[n], g - 1];
      real beta = mu_beta + tau_beta * beta_tilde[study_idx_indiv[n], g - 1];
      eta[g] = alpha + beta * time_indiv[n];
    }
    
    p = softmax(eta);
    N_responses_indiv[n] ~ multinomial(p);
  }
}
