data {
  int<lower=0> N;
  int<lower=0> K;
  int y[N];
  vector[N] x;
  int igroup[N]; 
}

parameters {
  real alpha;
  real beta;
  real sigma;
  real z[K]; 
}

transformed parameters{
  vector[N] lambda;
  for (n in 1:N)
    lambda[n] = exp(alpha + beta*x[n] + z[igroup[n]]);
    
}

model {
  alpha ~ normal(0, 2);
  beta ~ normal(0,2);
  
  // Random effect
  z ~ normal(0, sigma); //note variance is random! hence 'random effect'
  sigma ~ exponential(1);
  
  //likelihood
  y ~ poisson(lambda);
  
  //for (n in 1:N)
   //target += poisson_lpmf(y[n] | lambda[n]);
}

generated quantities{
  int y_sim[N];
  y_sim = poisson_rng(lambda);
}




