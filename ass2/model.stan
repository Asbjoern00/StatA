data {
int N;
vector[N] day1dummy;
vector[N] day2dummy;
vector[N] day3dummy;
vector[N] Z;
vector[N] SE;
}
parameters {
real theta_1;
real theta_2;
real theta_3;
real<lower = 0> tau;
}
transformed parameters {
real ratio_3_1 = theta_3/theta_1;
}
model {
Z ~ normal(theta_1*day1dummy + theta_2*day2dummy + theta_3*day3dummy, sqrt(tau^2 + SE^2));
}

