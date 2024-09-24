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


k <- seq(from=0,to=50,length.out=100)
beta_constant = 0.2
k_constant = 30
beta <- seq(from=0, to=1, length.out=100)
k_likelihood_sweep <- rep(NA, times=100)
beta_likelihood_sweep <- rep(NA, times=100)
for(i in 1:length(k)){
    k_likelihood_sweep[i] <- overall_likelihood_function(par=c(k[i],beta_constant), data = single_participant_data)
    beta_likelihood_sweep[i] <- overall_likelihood_function(par=c(k_constant,beta[i]), data = single_participant_data)
}

plot(k, k_likelihood_sweep, type="l", ylab="negative log likelihood", ylim=c(0,700), main = "(Negative log) likelihood surface for k\n(with beta held constant at 0.2)")
plot(beta, beta_likelihood_sweep, type="l", ylab="negative log likelihood", ylim=c(0,700), main = "(Negative log) likelihood surface for beta\n(with k held constant at 30)")


## code for MAP estimation
k_uniform_prior <- rep(1, times=100)
k_uniform_prior <- k_uniform_prior / sum(k_uniform_prior)
k_normal_prior <- dnorm(x=k, mean=0, sd=10)
k_normal_prior <- k_normal_prior / sum(k_normal_prior)

## plot priors
plot(k, k_uniform_prior, type="l", ylab="Prior probability (a.u.)", ylim=c(0,.07), main = "Uniform prior")
plot(k, k_normal_prior, type="l", ylab="Prior probability (a.u.)", ylim=c(0,.07), main = "Half-normal(0,10) prior")


## do map estimation
k_normal_posterior <- k_normal_prior * exp(-k_likelihood_sweep/188)
k_normal_posterior <- k_normal_posterior / sum(k_normal_posterior)
k_uniform_posterior <- k_uniform_prior * exp(-k_likelihood_sweep/188)
k_uniform_posterior <- k_uniform_posterior / sum(k_uniform_posterior)

plot(k, k_normal_posterior, type="l", ylab="Posterior probability (a.u.)", ylim=c(0,.07), main = "Posterior distribution")
plot(k, k_uniform_posterior, type="l", ylab="Posterior probability (a.u.)", ylim=c(0,.07), main = "Posterior distribution")

