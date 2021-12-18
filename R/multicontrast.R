multicontrast <- function(model, varname, coeff_mx, conf_int = 0.95) {
  if(!is.matrix(coeff_mx)) {
    coeff_mx <- matrix(coeff_mx, nrow=1)
  }

  intervals_bonf <- bonferroni_ints(model, varname, coeff_mx, conf_int)
  intervals_scheffe <- scheffe_ints(model, varname, coeff_mx, conf_int)

  result <- array(c(intervals_bonf, intervals_scheffe),
                  dim = c(nrow(coeff_mx), 3, 2))
  dimnames(result) <- list(rep("", nrow(coeff_mx)), c("lower", "upper", "length"),
                           c("Bonferroni", "Scheffe"))

  result
}

bonferroni_ints <- function(model, varname, coeff_mx, conf_int) {
  # Determine Bonferroni intervals

  bonf_conf <- 1 - (1 - conf_int) / nrow(coeff_mx)
  gfit <- gmodels::fit.contrast(model, varname, coeff_mx, conf.int = bonf_conf)
  intervals_bonf <- gfit[, c("lower CI", "upper CI")]
  intervals_bonf = cbind(intervals_bonf,
                         intervals_bonf[, 2] - intervals_bonf[, 1])
  colnames(intervals_bonf) <- c("lower", "upper", "length")

  intervals_bonf
}

scheffe_apply <- function(l, M, mu, C) {
  # Give the Scheffe bound for the vector l of coefficients.
  eta_hat <- t(l) %*% mu
  radius <- M * sigma(one_way_mean) * sqrt(t(l) %*% C %*% l)
  c(lower = eta_hat - radius, upper = eta_hat + radius, length = 2 * radius)
}

scheffe_ints <- function(model, varname, coeff_mx, conf_int) {
  # First, fit the mean model to get the correct design matrix
  mean_call <- model$call
  mean_call$formula <- formula(paste(deparse(formula(model)), "- 1"))
  mean_model <- eval(mean_call)

  # Determine Scheffe intervals
  nfactors <- length(mean_model$xlevels[[varname]])
  nobs <- length(resid(mean_model))
  scheffeM <- sqrt(nfactors * qf(conf_int, nfactors, nobs - nfactors))
  a <- model.matrix(mean_model)
  C <- solve(t(a) %*% a)
  means <- matrix(coef(mean_model))

  result <- matrix(nrow = nrow(coeff_mx), ncol = 3)
  for(i in 1:nrow(coeff_mx)) {
    result[i, ] <- scheffe_apply(coeff_mx[i, ], scheffeM, means, C)
  }
  colnames(result) <- c("lower", "upper", "length")

  result
}
