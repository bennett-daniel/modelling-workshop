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
compiled_model <- cmdstan_model("/Users/danielbennett/Documents/Git/modelling-workshop/code/hyperbolic-model.stan", force_recompile = F)

# sample from model
tic()
fit <- compiled_model$sample(
  data = stan_data,
  # chains = 1,
  # parallel_chains = 1,
  # refresh = 10,
  # iter_warmup = 50,
  # iter_sampling = 50,
  chains = 4,
  parallel_chains = 4,
  refresh = 250,
  iter_warmup = 1750,
  iter_sampling = 1250,
  save_warmup = FALSE
)
toc()


# extract parameter samples (group-level)
group_pars <- c("k_mu_pr", "beta_mu_pr")
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
