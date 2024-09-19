## load in data
bruder_data <- read.csv(here::here("data", "bruder-all.csv"))

## grab a single participant
p_data <- subset(bruder_data, bruder_data$SubID == unique(bruder_data$SubID)[1])

## specify the hyperbolic discounting function and its inverse
hyperbolic_discount_fun <- function(amount, delay, k){
  return(amount * (1 / (1 + k * delay)))
}

## specify the softmax function
softmax <- function(V, beta){
  return(exp(V * beta) / sum(exp(V * beta)))
}

likelihood_function <- function(par, p_data){
  
  ## assign the parameters to named variables
  k    <- par[1]
  beta <- par[2]
  
  ## create a container for the choice likelihoods
  n_trials <- nrow(p_data)
  choice_likelihood <- rep(NA, times=n_trials)
  
  ## loop over trials
  for (i in 1:n_trials){
    
    ## calculate the subjective value of the smaller sooner option
    V_ss <- hyperbolic_discount_fun(
      amount = p_data$value.now[i], 
      delay  = p_data$Delay[i],
      k      = k
    )
    
    ## calculate the subjective value of the larger later option
    V_ll <- hyperbolic_discount_fun(
      amount = p_data$Value.later[i], 
      delay  = p_data$Value.later.delay[i],
      k      = k
    )
    
    ## calculate softmax choice probability
    value_vector <- c(V_ss, V_ll)
    choice_probs <- softmax(
      V    = value_vector,
      beta = beta
    )
    
    ## calculate probability of chosen option
    if (p_data$Response[i] == 0){ ## if SS chosen
      choice_likelihood[i] <- choice_probs[1]
    } else if (p_data$Response[i] == 1){ ## if LL chosen
      choice_likelihood[i] <- choice_probs[2]
    }
  }
  
  ## calculate negative log likelihood of observed choices
  neg_ll <- -sum(log(choice_likelihood))
  
}

## fit model
model_fit <- optim(
  par    = c(0.1, 0.5), # k, beta
  fn     = likelihood_function,
  method = "L-BFGS-B",
  lower  = c(-Inf,0), # k, beta,
  p_data = p_data
)

## view output
print(model_fit)
