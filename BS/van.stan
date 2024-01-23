data {
int N;
vector[N] x; 
int y[N];
}
parameters {
real alpha;
real beta;
}
transformed parameters {
vector[N] lambda = exp(alpha + beta*x);
real bar_x = 1/beta*(log(10)-alpha);
}
model {
alpha ~ normal(3,25);
beta ~ normal(0,1);
y ~ poisson(lambda);
}

