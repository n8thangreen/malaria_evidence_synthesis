// https://stats.stackexchange.com/questions/297728/estimation-in-stan-help-modelling-a-multinomial
data {
  
  //##  4 groups  ##
    
    int<lower = 1> len_b;
    int X_b[len_b, 4];
    int<lower = 1> N_studies_b;
}

transformed data {
  
}

parameters {
  
  // simplex[4] prob_b1;
  // simplex[4] prob_b2;
  // simplex[4] prob_b3;
  
  matrix[1, 4] beta;
  // vector[4] phi_b1;
  // vector[4] phi_b2;
  // vector[4] phi_b3;
}

transformed parameters {
  
   vector[4] phi_b1;
   simplex[4] prob_b1;
   
   phi_b1[1] = 1;

    for(c in 2:4){
      
      phi_b1[c] = exp(beta[1, c]);
    }
    
   prob_b1[1] = 1/sum(phi_b1[1:4]);

    // phi_b2[1] = 1;
    // prob_b2[1] = 1/sum(phi_b2[1:4]);
    // 
    // phi_b3[1] = 1;
    // prob_b3[1] = 1/sum(phi_b3[1:4]);
    
    
    for(c in 2:4){
      
      prob_b1[c] = phi_b1[c]/sum(phi_b1[1:4]);
      
      // phi_b2[c] = exp(beta[2, c]);
      // prob_b2[c] = phi_b2[c]/sum(phi_b2[1:4]);
      // 
      // phi_b3[c] = exp(beta[3, c]);
      // prob_b3[c] = phi_b3[c]/sum(phi_b3[1:4]);
    }
}


model {
  
  //priors
      
    for(c in 2:4){
      
      beta[1, c] ~ normal(0, 100000);
      // beta[2, c] ~ normal(0, 1);
      // beta[3, c] ~ normal(0, 1);
    }
  
  // likelihood
  
  for(j in 1:len_b){
    
     X_b[j, ] ~ multinomial(prob_b1);
     // X_b[j, ] ~ multinomial(prob_b);
  }
}

generated quantities {
  
}

