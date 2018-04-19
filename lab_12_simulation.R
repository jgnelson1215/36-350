generate_data = function(n, p){
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

