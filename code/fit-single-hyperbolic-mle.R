## load in data
bruder_data_file <- paste("data", "bruder-all.csv", sep=.Platform$file.sep)
bruder_data <- read.csv(bruder_data_file)

## extract data from a single participant
participant_to_fit <- 1
single_participant_data <- subset(bruder_data, bruder_data$SubID == unique(bruder_data$SubID)[1])

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

## fit model
model_fit <- optim(
  par    = c(1, 0.5), # k, beta
  fn     = overall_likelihood_function,
  method = "L-BFGS-B",
  lower  = c(-Inf,0), # k, beta,
  data   = single_participant_data
)

## view output
print(model_fit)

## calculate AIC and BIC
n_pars <- 2
n_datapoints <- nrow(single_participant_data)
model_AIC <- 2 * model_fit$value + 2 * n_pars
model_BIC <- 2 * model_fit$value + n_pars * log(n_datapoints)