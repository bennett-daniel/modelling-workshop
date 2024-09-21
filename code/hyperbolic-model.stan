data {

  // Metadata
  int N;             // Number of participants
  int T;             // Number of trials

  // Indices
  array[T] int subj_ix;    // Participant number for each datapoint

  // Data
  array[T] int  response;       // Dependent variable: choice of SS (1) or LL (2) option
  array[T] real ss_amount;      // Smaller sooner amount
  array[T] real ss_delay;       // Smaller sooner delay
  array[T] real ll_amount;      // Larger later amount
  array[T] real ll_delay;       // Larger later delay

}

parameters {

  // Group-level means
  real k_mu_pr;
  real beta_mu_pr;

  // Group-level SDs
  real<lower=0> k_sigma_pr;
  real<lower=0> beta_sigma_pr;

  // Participant-level parameters
  vector[N] k_pr;
  vector[N] beta_pr;

}

transformed parameters {

  vector[N] k;
  vector[N] beta;

  for (p_ix in 1:N){
    k[p_ix]    = exp(k_mu_pr + k_sigma_pr * k_pr[p_ix]);
    beta[p_ix] = Phi_approx(beta_mu_pr + beta_sigma_pr * beta_pr[p_ix]);
  }
}

model {

  // group-level priors for means
  k_mu_pr    ~ normal(0, 1);
  beta_mu_pr ~ normal(0, 1);

  // group-level priors for SDs
  k_sigma_pr    ~ exponential(0.1);
  beta_sigma_pr ~ exponential(0.1);

  // subject-level priors
  k_pr    ~ normal(0, 1);
  beta_pr ~ normal(0, 1);

  // containers for ss and ll svalues
  vector[T] ss_val     = rep_vector(0, T);
  vector[T] ll_val     = rep_vector(0, T);
  vector[T] alpha      = rep_vector(0, T); //bernoulli logit input

  // loop over trials
  for (trial_ix in 1:T){

    // subjective value of ss
    ss_val[trial_ix] = ss_amount[trial_ix] * (1 / (1 + k[subj_ix[trial_ix]] * ss_delay[trial_ix]));
    
    // subjective value of ss
    ll_val[trial_ix] = ll_amount[trial_ix] * (1 / (1 + k[subj_ix[trial_ix]] * ll_delay[trial_ix]));
    
    // calculate bernoulli logit input
    alpha[trial_ix] = beta[subj_ix[trial_ix]] * (ll_val[trial_ix] - ss_val[trial_ix]);
    
  }

  // responses distributed as softmax
  response ~ bernoulli_logit(alpha);

}

generated quantities {

}
