//
//

data {
  int<lower=1> N_groups; // Number of reponse categories
  
  int<lower=1> N_agg_exp;           // Number of experiments with aggregated responses
  int<lower=0> k1_agg[N_agg_exp];   // Number of successes in first aggregation pattern
  int<lower=0> k2_agg[N_agg_exp];   // Number of successes in second aggregation pattern
  int<lower=1> N_trials_agg[N_agg_exp]; // Number of trials
  
  int<lower=1> N_studies;
  int<lower=1, upper=N_studies> study_idx_agg[N_agg_exp]; // Study index
  
  real time_agg[N_agg_exp];
  
  int<lower=1> N_indiv_exp;   // Number of experiments with individual responses
  int<lower=0> N_responses_indiv[N_indiv_exp, N_groups]; // Counts of each response
  
  int<lower=1, upper=N_studies> study_idx_indiv[N_indiv_exp]; // Study index
  
  real time_indiv[N_indiv_exp];
}

parameters {
  real mu_alpha[N_groups];           // Intercept population location
  real<lower=0> tau_alpha;           // Intercept population scale
  
  real alpha_tilde[N_studies, N_groups - 1]; // Noncentered intercepts
  
  real mu_beta[N_groups];           // Slope population location
  real<lower=0> tau_beta;           // Slope population scale
  
  real beta_tilde[N_studies, N_groups - 1]; // Noncentered slopes
}

model {
  // priors
  //for (i in 1:N_groups)
    mu_alpha ~ normal(0, 2);
  
  tau_alpha ~ normal(0, 2);
  //to_vector(alpha_tilde) ~ normal(0, 1); //this should be a better for()
  for (n in 1:N_studies)
    alpha_tilde[n] ~ normal(0, 1);

  //for (i in 1:(N_groups - 1))  
    mu_beta ~ normal(0, 0.4);
  
  tau_beta ~ normal(0, 0.4);
  
  for (n in 1:N_studies)
    beta_tilde[n] ~ normal(0, 1);
  
  // aggregate
  for (n in 1:N_agg_exp) {
    
    //real eta[N_groups]; // Latent effect for each response
    vector[N_groups] eta;
    
    //real p[N_groups];   // Response probabilities
    vector[N_groups] p;
    
    eta[1] = 0;
    for (g in 2:N_groups) {
      
      real alpha = mu_alpha[g] + tau_alpha * alpha_tilde[study_idx_agg[n], g - 1];
      real beta = mu_beta[g] + tau_beta * beta_tilde[study_idx_agg[n], g - 1];
      eta[g] = alpha + beta * time_agg[n];
    }
    
    p = softmax(eta);
    
    k1_agg[n] ~ binomial(N_trials_agg[n], p[2] + p[4]);
    k2_agg[n] ~ binomial(N_trials_agg[n], p[1] + p[2]);
  }
  
  // individual
  for (n in 1:N_indiv_exp) {
    
    //real eta[N_groups]; // Latent effect for each response
    vector[N_groups] eta;
    
    //real p[N_groups];   // Response probabilities
    vector[N_groups] p;
    
    eta[1] = 0;
    for (g in 2:N_groups) {
      real alpha = mu_alpha[g] + tau_alpha * alpha_tilde[study_idx_indiv[n], g - 1];
      real beta = mu_beta[g] + tau_beta * beta_tilde[study_idx_indiv[n], g - 1];
      eta[g] = alpha + beta * time_indiv[n];
    }
    
    p = softmax(eta);
    
    N_responses_indiv[n] ~ multinomial(p);
  }
}

generated quantities {
  
  matrix[N_groups, 12] p_pred;
  
  for (n in 1:12) {
    vector[N_groups] eta;
    
    eta[1] = 0;
    for (g in 2:N_groups) {

      eta[g] = mu_alpha[g] + mu_beta[g] * n;
    }
    
    p_pred[, n] = softmax(eta);
  }
  
}

