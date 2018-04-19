generate_data = function(n, p){
  # browser()
  covariates = matrix(nrow = n, ncol = p)
  for(i in 1:n){
    trial = rnorm(n = p)
    covariates[i, ] = trial
  }
  responses = rnorm(n = n)
  return(list(covariates = covariates,
              responses = responses)
  )
}

model_select = function(covariates, responses, cutoff){
  # browser()
  model.lm = lm(responses ~ covariates)
  # exclude intercept
  valid.coefs = which(as.numeric(summary(model.lm)[["coefficients"]][-1, "Pr(>|t|)"]) <= cutoff)
  if(length(valid.coefs) == 0){return (c())}
  new_model.lm = lm(responses ~ covariates[, valid.coefs])
  return(as.numeric(summary(model.lm)[["coefficients"]][-1, "Pr(>|t|)"]))
}

run_simulation = function(n_trials = 10, n, p, cutoff){
  # browser()
  result = c()
  for(i in 1:n_trials){
    data = generate_data(n = n, p = p)
    result = cbind(result, 
                   model_select(covariates = data[[1]], responses = data[[2]], cutoff = cutoff))
  }
  hist(as.numeric(result), breaks = 50)
}

run_simulation(n = 100, p = 10, cutoff = 0.05)
run_simulation(n = 1000, p = 10, cutoff = 0.05)
run_simulation(n = 10000, p = 10, cutoff = 0.05)
run_simulation(n = 100, p = 20, cutoff = 0.05)
run_simulation(n = 1000, p = 20, cutoff = 0.05)
run_simulation(n = 10000, p = 20, cutoff = 0.05)
run_simulation(n = 100, p = 50, cutoff = 0.05)
run_simulation(n = 1000, p = 50, cutoff = 0.05)
run_simulation(n = 10000, p = 50, cutoff = 0.05)
