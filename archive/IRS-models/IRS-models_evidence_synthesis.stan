// bernoulli_logistic transformed data function
data {

  int<lower=1> N;                  // rows of data

  int<lower=0> n_t;             // Total number of mosquitoes entering IRS huts
  int<lower=0> d_t;             // Number mosquites dead sprayed hut
  int<lower=0> fed_t;           // Number of mosquitoes feeding in IRS HUTS assuming equal feeding for dead and alive ones
  int<lower=0> deterrence_IRS;  // Number of mosquitoes in sprayed huts
  int<lower=0> deterrence_total; //Total number of mosquitoes in both sprayed and control huts

  vector<lower=0> time;       // predictor

}

parameters {
  //Consider death. This is the proportion of mosquitoes dying (d_t) in treated huts (n_t)
  real alpha1;
  real alpha2;

  //Consider feeding. This is the proportion of mosquitoes that successfully fed in treatment (f_t)
  real beta1;
  real beta2;

  //Consider feeding. This is the proportion of mosquitoes that successfully fed in treatment (f_t)
  real omega1;
  real omega2;

  //  vector[N_study] study_a;
  //  real<lower=0,upper=10> sigma;
}

// see BDS 16.6 Models for multivariate and multinomial responses
// for multinomial model

// use simplex vector?

model {

      for (i in 1:m) {
          Xfs[i] ~ Bin(M[i], y[1]);
          // y1[i] = betafs + study1[i];
         }

      for (i in 1:r) {
         Xd[i] ~ Bin(R[i], y[2]);
         // y2[i] = betad + study2[i];
        }

     for (i in 1:k) {
        Xufs[i] ~ Bin(K[i], y[3]);
        // y3[i] = betaufs + study3[i];
       }

    // pd = 1 - pfs - pufs;
    y ~ dirichlet(pi);

    pi[1] ~ dnorm(0, 0.001);
    pi[2] ~ dnorm(0, 0.001);
    pi[3] ~ dnorm(0, 0.001);

    //hyperpriors or fixed?

}
