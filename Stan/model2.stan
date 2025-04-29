/*  
Some resources: 
https://github.com/paparker/Unit_Level_Models/blob/master/Model_2.stan#L70C22-L70C70
https://mc-stan.org/users/documentation/case-studies/icar_stan.html
https://mc-stan.org/users/documentation/case-studies/mbjoseph-CARStan.html
https://github.com/stan-dev/stan/wiki/prior-choice-recommendations
*/
functions {
    real icar_normal_lpdf(vector theta, int N, array[] int node1, array[] int node2) {
        return -0.5 * dot_self(theta[node1] - theta[node2]) + 
        normal_lpdf(sum(theta) | 0, 0.001 * N); 
        //the second term added for sum-to-zero contraits
    }
}
data {
    int<lower=0> NR; //number of admin1 areas (provinces)
    int<lower=0> NS; //number of admin2 areas (district)
    int<lower=0> NT; //number of timepoints
    int<lower=0> N; //toal number of households
    int<lower=0> B; // number of covariates

    array[N] int<lower=0, upper=1> y; //response - household classidied as food insecure or not
    matrix[N,B] X; // matrix gathering covariates

    array[N] int admin2; //admin2 area index for each obs
    array[N] int admin1; //admin1 area index for each obs
    array[N] int time; // time index for each obs

    int<lower=1> N_edges ; 
    array[N_edges] int<lower=1, upper=NS> node1 ;
    array[N_edges] int<lower=1, upper=NS> node2 ;
}
transformed data {
  real delta = 1e-9;
}
parameters {
  vector[NS] u1; //spatial random effects, unscaled
  vector[NS] u2; // iid random effects (area level)
  matrix[NT,NR] u5; // spatio-temporal interaction
  vector[NT] u4;// time random effect (main, rw)
  vector[NT] u3; // time random effect(main, iid)
  real<lower=0> sigma_phi; // Car precision param
  real<lower=0> sigma_theta; //iid RE presicion
  real<lower=0> sigma_psi; //space-time-interaction presition
  real<lower=0> sigma_xi; // time IID presition
  real<lower=0> sigma_nu; //time RW presition
  real b0;
  vector[B] betas;  
}
transformed parameters {
   real tau_phi = 1/(square(sigma_phi)); 
   real tau_theta = 1/(square(sigma_theta)); 
   real tau_nu = 1/(square(sigma_nu)); 
   real tau_xi = 1/(square(sigma_xi)); 
   real tau_psi = 1/(square(sigma_psi)); 
   vector[NS] phi = sigma_phi*u1;
   vector[NS] theta = sigma_theta*u2;
   vector[NT] nu = sigma_nu*u3;
   vector[NT] xi = sigma_xi*u4;
   matrix[NT, NR] psi = sigma_psi*u5;
}
model {
 vector[N] psi_vec;
 {
    for (i in 1:N) psi_vec[i] = psi[time[i],admin1[i]];
 }

 //likelihood
 target += bernoulli_logit_lpmf(y |  b0 + X*betas +  phi[admin2] + theta[admin2] + psi_vec +nu[time] + xi[time] );

 // priors 
 //phi
 target += icar_normal_lpdf(u1|NS, node1, node2);
 //theta 
 target += normal_lpdf(u2|0, 1);

// time rw 
target += normal_lpdf(u3[1]|0,1);// comment out for flat prior
for (t in 2:NT){
  target +=normal_lpdf(u3[t]|u3[t-1],1);
}
target += normal_lpdf(sum(u3)|0, 0.01 * NT);
// time iid effect
target += normal_lpdf(u4|0,1);


// space-time interaction iid 
 for (i in 1:NR){
  target += normal_lpdf(u5[1:NT,i]|0,1);
}
// priors
 target += normal_lpdf(b0|0,5);
 target += normal_lpdf(betas|0,5);          
// hyper priors 
 target += cauchy_lpdf(sigma_phi|0, 1);
 target += cauchy_lpdf(sigma_theta|0, 1); 
 target += cauchy_lpdf(sigma_nu|0, 1);
 target += cauchy_lpdf(sigma_xi|0, 1);
 target += cauchy_lpdf(sigma_psi|0, 1);
}