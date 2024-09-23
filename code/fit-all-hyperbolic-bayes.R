# import libraries
require(cmdstanr)
require(tictoc)
require(posterior)

# set the cmdstan path
if (Sys.info()["sysname"] == "Darwin"){
  set_cmdstan_path("/Users/danielbennett/.cmdstan/cmdstan-2.35.0")
} else if (Sys.info()["sysname"] == "Linux"){
  set_cmdstan_path("/home/dbennett/cmdstan")
} else if (Sys.info()["sysname"] == "Windows"){
  set_cmdstan_path("C:/Users/dben0009/Documents/cmdstan/cmdstan-2.30.0/")
}

# verify cmdstan version
print(paste("cmdstan version is", cmdstan_version()))

## load in data
bruder_data <- read.csv(here::here("data", "bruder-all.csv"))

## convert data for stan
stan_data <- list(
  subj_ix   = match(bruder_data$SubID, unique(bruder_data$SubID)),
  ss_amount = bruder_data$value.now,
  ss_delay  = bruder_data$Delay / 365,
  ll_amount = bruder_data$Value.later,
  ll_delay  = bruder_data$Value.later.delay / 365,
  response  = bruder_data$Response,
  T         = nrow(bruder_data),
  N         = length(unique(bruder_data$SubID))
)

# pre-compile model
stan_model <- "
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
"
compiled_model <- cmdstan_model("/Users/danielbennett/Documents/Git/modelling-workshop/code/hyperbolic-model.stan", force_recompile = F)

# sample from model
tic()
fit <- compiled_model$sample(
  data = stan_data,
  chains = 4,
  parallel_chains = 4,
  refresh = 250,
  iter_warmup = 1750,
  iter_sampling = 1250,
  save_warmup = FALSE
)
toc()


# extract parameter samples (group-level)
group_pars <- c("k_mu_pr", "k_sigma_pr", "beta_mu_pr", "beta_sigma_pr")
group_par_samples <- read_cmdstan_csv(
  files=fit$output_files(),
  variables = group_pars,
  sampler_diagnostics = NULL,
  format = getOption("cmdstanr_draws_format", "draws_df")
)

group_par_samples <- as.matrix(group_par_samples$post_warmup_draws[,1:length(group_pars)])
group_par_est <- apply(group_par_samples, MARGIN=2, FUN=median)
for (i in 1:length(group_pars)){
  hist(group_par_samples[,i], main=group_pars[i], 100)
}

# extract parameter samples (individual-level)
indiv_pars <- c("k", "beta")
indiv_par_samples_all <- read_cmdstan_csv(
  files=fit$output_files(),
  variables = indiv_pars,
  sampler_diagnostics = NULL,
  format = getOption("cmdstanr_draws_format", "draws_df")
)
indiv_par_samples <- vector(mode="list", length=length(indiv_pars))
indiv_par_est <- matrix(NA, nrow= indiv_par_samples_all$metadata$stan_variable_sizes[[indiv_pars[1]]], ncol=length(indiv_pars))
colnames(indiv_par_est) <- indiv_pars
for (i in 1:length(indiv_par_samples)){
  indiv_par_samples[[i]] <- as.matrix(indiv_par_samples_all$post_warmup_draws[seq(
    from       = 1 + (i-1) * dim(indiv_par_est)[1],
    to         = i * dim(indiv_par_est)[1],
    length.out = dim(indiv_par_est)[1])
  ])
  indiv_par_est[,i] <- apply(indiv_par_samples[[i]], MARGIN=2, FUN=median)
  hist(indiv_par_est[,i], main=indiv_pars[i], 25)
}


# save to results folder
save("indiv_par_samples", "indiv_par_est", file="/Users/danielbennett/Documents/Git/modelling-workshop/results/bayes-results-hyperbolic-indiv.Rdata")
save("group_par_samples", "group_par_est", file="/Users/danielbennett/Documents/Git/modelling-workshop/results/bayes-results-hyperbolic-group.Rdata.Rdata")
