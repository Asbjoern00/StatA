data {
int N;
vector[N] y;
}
parameters {
real<lower = 0, upper = 1> theta;
}
model {
y ~ normal(0, sqrt(theta));
}

