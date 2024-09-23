## set random number generator seed for reproducible results
set.seed(112358)

## load in data
bruder_data_file <- paste("data", "bruder-all.csv", sep=.Platform$file.sep)
bruder_data <- read.csv(bruder_data_file)

## specify the hyperbolic discounting function
hyperbolic_discount_function <- function(amount, delay, k){
  return(amount * (1 / (1 + k * delay)))
}

## specify the softmax choice likelihood function
softmax_function <- function(V_ss, V_ll, beta){
  V <- c(V_ss, V_ll)
  return(exp(V * beta) / sum(exp(V * beta)))
}

overall_likelihood_function <- function(par, data){
  
  ## assign the parameters to named variables
  k    <- par[1]
  beta <- par[2]
  
  ## create a container for the choice likelihoods
  n_trials <- nrow(data)
  choice_likelihood <- rep(NA, times=n_trials)
  
  ## loop over trials
  for (trial_ix in 1:n_trials){
    
    ## calculate the subjective value of the smaller sooner option
    V_ss <- hyperbolic_discount_function(
      amount = data$value.now[trial_ix], 
      delay  = data$Delay[trial_ix] / 365,
      k      = k
    )
    
    ## calculate the subjective value of the larger later option
    V_ll <- hyperbolic_discount_function(
      amount = data$Value.later[trial_ix], 
      delay  = data$Value.later.delay[trial_ix] / 365,
      k      = k
    )
    
    ## calculate softmax choice probability
    choice_probabilities <- softmax_function(
      V_ss = V_ss,
      V_ll = V_ll,
      beta = beta
    )
    
    ## calculate probability of chosen option
    if (data$Response[trial_ix] == 0){ ## if SS chosen
      choice_likelihood[trial_ix] <- choice_probabilities[1]
    } else if (data$Response[trial_ix] == 1){ ## if LL chosen
      choice_likelihood[trial_ix] <- choice_probabilities[2]
    }
  }
  
  ## calculate negative log likelihood of observed choices
  neg_ll <- -sum(log(choice_likelihood))
  
  ## return the negative LL as the output of the likelihood function
  return(neg_ll)
  
}

## create containers for model fit outputs
n_participants   <- length(unique(bruder_data$SubID))
all_neg_ll       <- rep(NA, times=n_participants)
all_k            <- rep(NA, times=n_participants)
all_beta         <- rep(NA, times=n_participants)
all_n_datapoints <- rep(NA, times=n_participants)
all_AIC          <- rep(NA, times=n_participants)
all_BIC          <- rep(NA, times=n_participants)
all_output_flag  <- rep(NA, times=n_participants)

## loop over participants
n_pars <- 2
for (p_ix in 1:n_participants){
  
  ## log progress to console
  print(sprintf("Fitting participant %.0f of %.0f", p_ix, n_participants))
  
  ## extract participant-level data
  single_participant_data <- subset(bruder_data, bruder_data$SubID == unique(bruder_data$SubID)[p_ix])
  
  ## fit model
  model_fit <- optim(
    par     = c(runif(n=1, min=-2, max=0), runif(n=1, min=0,max=2)), # k, beta
    fn      = overall_likelihood_function,
    method  = "L-BFGS-B",
    lower   = c(-Inf,0), # k, beta,
    data    = single_participant_data,
    control = list(factr = 1e-15)
  )
  
  ## extract details of model fit
  all_neg_ll[p_ix]             <- model_fit$value
  all_k[p_ix]                  <- model_fit$par[1]
  all_beta[p_ix]               <- model_fit$par[2]
  all_n_datapoints[p_ix]       <- nrow(single_participant_data)
  all_AIC[p_ix]                <- 2 * model_fit$value + 2 * nrow(single_participant_data)
  all_BIC[p_ix]                <- 2 * model_fit$value + n_pars * nrow(single_participant_data)
  all_output_flag[p_ix]        <- model_fit$convergence
  
  ## clear model fit
  rm(list="model_fit")
  
}

all_results_hyperbolic <- list(
  all_k            = all_k,
  all_beta         = all_beta,
  all_neg_ll       = all_neg_ll,
  all_AIC          = all_AIC,
  all_BIC          = all_BIC,
  all_output_flag  = all_output_flag,
  all_n_datapoints = all_n_datapoints
)

save("all_results_hyperbolic", file=paste("results", "mle-results-hyperbolic.Rdata", sep=.Platform$file.sep))

