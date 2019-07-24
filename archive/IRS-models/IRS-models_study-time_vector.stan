// bernoulli_logistic transformed data function
data {

    int<lower=1> N;                  // rows of data

    int<lower=0> n_t[N];             // Total number of mosquitoes entering IRS huts
    int<lower=0> d_t[N];             // Number mosquites dead sprayed hut
    int<lower=0> fed_t[N];           // Number of mosquitoes feeding in IRS HUTS assuming equal feeding for dead and alive ones
    int<lower=0> deterrence_IRS[N];  // Number of mosquitoes in sprayed huts
    int<lower=0> deterrence_total[N]; //Total number of mosquitoes in both sprayed and control huts

    vector<lower=0>[N] time;       // predictor

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

model {
    real sp;
    real fp;
    real det;

    alpha1 ~ normal(0,10);
    alpha2 ~ normal(0,10);

    beta1 ~ normal(0,10);
    beta2 ~ normal(0,10);

    omega1 ~ normal(0,10);
    omega2 ~ normal(0,10);

        for (i in 1:m) {
            Xfs[i] ~ Bin(M[i], pfs)
            logit(pfs) = alpha1 + alpha2 * timefs[i];
        }

        for (i in 1:r) {
            Xd[i] ~ Bin(R[i], pd)
            logit(pd) = omega1 + omega2 * timed[i];
        }

        for (i in 1:k) {
            Xufs[i] ~ Bin(K[i], pufs)
            logit(pufs) = beta1 + beta2 * timeufs[i];
        }

        pd = 1 - pfs - pufs

        pfs  ~ dnorm(0, 0.001)
        pd   ~ dnorm(0, 0.001)
        pufs ~ dnorm(0, 0.001)
    }



}
