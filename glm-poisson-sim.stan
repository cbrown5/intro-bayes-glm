data {
  int<lower=0> N;
  int y[N];
  vector[N] x;
}

parameters {
  real alpha;
  real beta;
}

transformed parameters{
  vector[N] lambda;
  lambda = exp(alpha + beta*x);
}

model {
  alpha ~ normal(0, 2);
  beta ~ normal(0,2);
  
  //likelihoods
  //y ~ poisson(lambda);
  
  //for (n in 1:N)
   //target += poisson_lpmf(y[n] | lambda[n]);
}

generated quantities{
  int y_sim[N];
  y_sim = poisson_rng(lambda);
}




