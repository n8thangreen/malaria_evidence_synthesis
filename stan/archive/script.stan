data {
  
    //##  4 groups  ##
    
    int<lower = 1> len_b;              // number of rows of data
    int<lower = 1> N_studies_b;
    matrix[len_b, 4] X_b;
    vector[len_b] time_b;
    int studyid_b[len_b];
}

parameters {

   matrix[N_studies_b, 4] beta0c2;
   matrix[N_studies_b, 4] beta1c2;
   
   vector[4] logsigma_beta1;
   vector[4] mu_beta0;
   vector[4] logsigma_beta0;
   vector[4] mu_beta1;
   
   matrix[len_b, 4] phi_b;
   matrix[len_b, 4] prob_b;
   
}

transformed parameters {

  vector[4] sigma_beta1;
  vector[4] sigma2_beta1;
  vector[4] tau_beta1;
  vector[4] tau_beta0;
  vector[4] sigma_beta0;
  vector[4] sigma2_beta0;

  matrix[len_b, 4] XB;

  for (c in 1:4){
    sigma_beta1[c] = exp(logsigma_beta1[c]);	  // sd for between trials
    sigma2_beta1[c] = pow(sigma_beta1[c], 2);	// variance for between trials
    sigma_beta0[c] = exp(logsigma_beta0[c]);
    sigma2_beta0[c] = pow(sigma_beta0[c], 2);
  }
  
}

model {


# do vector of X then sum log likelihoods?
# target +=

  //priors
  
  for (c in 1:4){
    
    logsigma_beta1[c] ~ uniform(-5, 10);
    mu_beta0[c] ~ normal(0, 1.0E6);		  
    logsigma_beta0[c] ~ uniform(-5, 10);		    // log-sd for between trials
    mu_beta1[c] ~ normal(0, 1.0E6);		
  }
  
  for (i in 1:N_studies_b){
      
    for(c in 1:4){
      
      beta0c2[i,c] ~ normal(mu_beta0[c], sigma2_beta0[c]);
      beta1c2[i,c] ~ normal(mu_beta1[c], sigma2_beta1[c]);
    }
  }
  
  for (j in 1:len_b){
    for (c in 1:4){
      XB[j,c] = X_b[j,c] * beta1c2[studyid_b[j], c];
    }
  }
  
  // likelihood
  
  for (j in 1:len_b){
    
    // multinomial_lpmf(X_b[j, ], prob_b[j, ]);
    
    prob_b[j, ] ~ categorical_logit(XB[j, ]);
    
  }

  ## watch out for real-integer coercion!
  
  for (i in 1:N_studies_b){
  
    //set reference category to zero
    beta0c2[i,1] = 0;
    beta1c2[i,1] = 0;
  }

  for(j in 1:len_b){
  
    phi_b[j,1] = 1;
    prob_b[j,1] = 1/sum(phi_b[j, 1:4]);
    
    for(c in 2:4){
      
      phi_b[j, c] = exp(beta0c2[studyid_b[j], c] + beta1c2[studyid_b[j], c]*time_b[j]);
      prob_b[j, c] = phi_b[j,c]/sum(phi_b[j, 1:4]);
    }
  }
}

generated quantities {

}