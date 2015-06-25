////////////////////////////////
// Fiscal Policy and Financial Crises
// Model Version 0.1
// Stan Version 2.6.0
// Christopher Gandrud
// MIT License
////////////////////////////////

data {
    int<lower=0> N;                     // number of observations
    int<lower=0> K;                     // number of predictors
    int<lower=0> C;                     // number of countries
    int<lower=0,upper=C> country[N];    // names of countries
    matrix[N,K] X;                      // predictor matrix
    vector[N] y;                        // fiscal choice
}

parameters {
    vector[K] beta;                     // coefficients for predictors
    real alpha;                         // intercept
    vector[C] a;                        // country intercepts
    real<lower=0,upper=100> sigma_y;
    real<lower=0,upper=100> sigma_a;    // variance of country intercept
}

transformed parameters {
    vector[N] y_hat;

    for (n in 1:N)
        y_hat[n] <- X[n] * beta + alpha + a[country[n]];
}

model {
    sigma_y ~ uniform(0, 100);
    beta ~ normal(0, 1);
    alpha ~ normal(0, 1);
    a ~ normal(0, sigma_a);

    for (n in 1:N)
        y[n] ~ normal(y_hat, sigma_y);
}
