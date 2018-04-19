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

model_select = function(covariates, responses, cutoff){
  model.lm = lm(responses ~ covariates)
  valid.coefs = which(coef(model.lm)[, "Pr(>|t|)"] <= cutoff)
  if(length(valid.coefs) == 0){return (c())}
  new_model.lm = lm(responses ~ covariates[, valid.coefs])
  return(coef(new_model.lm)[, "Pr(>|t|)"])
}

