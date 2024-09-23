## set seed
set.seed(112358)

## load in data
bruder_data <- read.csv(here::here("data", "bruder-all.csv"))

## specify the linear discount function
linear_discount_fun <- function(amount, delay, discount_parameter){
  return(amount + discount_parameter * delay)
}

## specify the softmax function
softmax <- function(V, beta){
  return(exp(V * beta) / sum(exp(V * beta)))
}

likelihood_function <- function(par, p_data){
  
  ## assign the parameters to named variables
  discount_parameter <- par[1]
  beta               <- par[2]
  
  ## create a container for the choice likelihoods
  n_trials <- nrow(p_data)
  choice_likelihood <- rep(NA, times=n_trials)
  
  ## loop over trials
  for (trial_ix in 1:n_trials){
    
    ## calculate the subjective value of the smaller sooner option
    V_ss <- linear_discount_fun(
      amount             = p_data$value.now[trial_ix], 
      delay              = p_data$Delay[trial_ix] / 365,
      discount_parameter = discount_parameter
    )
    
    ## calculate the subjective value of the larger later option
    V_ll <- linear_discount_fun(
      amount             = p_data$Value.later[trial_ix], 
      delay              = p_data$Value.later.delay[trial_ix] / 365,
      discount_parameter = discount_parameter
    )
    
    ## calculate softmax choice probability
    value_vector <- c(V_ss, V_ll)
    choice_probs <- softmax(
      V    = value_vector,
      beta = beta
    )
    
    ## calculate probability of chosen option
    if (p_data$Response[trial_ix] == 0){ ## if SS chosen
      choice_likelihood[trial_ix] <- choice_probs[1]
    } else if (p_data$Response[trial_ix] == 1){ ## if LL chosen
      choice_likelihood[trial_ix] <- choice_probs[2]
    }
  }
  
  ## calculate negative log likelihood of observed choices
  neg_ll <- -sum(log(choice_likelihood))
  
  ## return neg_ll
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
  p_data <- subset(bruder_data, bruder_data$SubID == unique(bruder_data$SubID)[p_ix])
  
  ## skip participants with errors
  # if (p_ix %in% c(7,12,20,21,22,23,24,31)){
  # if (p_ix %in% c(2,6,7,10,12,20,21,22,23,24,31,34)){
  #     next
  # }
  
  ## fit model
  model_fit <- optim(
    par    = c(runif(n=1, min=-2, max=0), runif(n=1, min=0,max=2)), # k, beta
    fn     = likelihood_function,
    method = "L-BFGS-B",
    lower  = c(-Inf,0), # k, beta,
    p_data = p_data,
    control = list(factr = 1e-15)
  )
  
  ## extract details of model fit
  all_neg_ll[p_ix]       <- model_fit$value
  all_k[p_ix]            <- model_fit$par[1]
  all_beta[p_ix]         <- model_fit$par[2]
  all_n_datapoints[p_ix] <- nrow(p_data)
  all_AIC[p_ix]          <- 2 * model_fit$value + 2 * nrow(p_data)
  all_BIC[p_ix]          <- 2 * model_fit$value + n_pars * nrow(p_data)
  all_output_flag[p_ix]  <- model_fit$convergence
  
  ## clear model fit
  rm(list="model_fit")
  
}

all_results_linear <- list(
  all_k            = all_k,
  all_beta         = all_beta,
  all_neg_ll       = all_neg_ll,
  all_AIC          = all_AIC,
  all_BIC          = all_BIC,
  all_output_flag  = all_output_flag,
  all_n_datapoints = all_n_datapoints
)

save("all_results_hyperbolic", file="/Users/danielbennett/Documents/Git/modelling-workshop/results/mle_results_linear.Rdata")
