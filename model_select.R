model_select = function(covariates, responses, cutoff){
  model.lm = lm(responses ~ covariates)
  valid.coefs = which(coef(model.lm)[, "Pr(>|t|)"] <= cutoff)
  if(length(valid.coefs) == 0){return c()}
  new_model.lm = lm(responses ~ covariates[, valid.coefs])
  return(coef(new_model.lm)[, "Pr(>|t|)"])
}