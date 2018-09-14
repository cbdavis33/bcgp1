initFunc <- function(initList, priors, xTrain){

  d <- length(priors$rhoG$alpha)
  initReturn <- vector("list")
  initReturn$mu <- rnorm(1, 0, 1)
  initReturn$w <- priors$w$a + rbeta(1, priors$w$alpha, priors$w$beta)*(priors$w$b - priors$w$a)
  initReturn$rhoG <- rbeta(d, priors$rhoG$alpha, priors$rhoG$beta)
  initReturn$rhoL <- initReturn$rhoG * rbeta(d, priors$rhoL$alpha, priors$rhoL$beta)
  initReturn$sig2eps <- max(.Machine$double.eps,
                            rgamma(1, shape = priors$sig2eps$alpha,
                                   scale = priors$sig2eps$beta))
  initReturn$muV <- rnorm(1, priors$muV$mu, sqrt(priors$muV$sig2))
  initReturn$rhoV <- rbeta(d, priors$rhoV$alpha, priors$rhoV$beta)
  ## FIX?: The values for sig2K seem very big
  initReturn$sig2K <- 1/rgamma(1, priors$sig2K$alpha, priors$sig2K$beta)
  ## FIX?: The initial log GP for sig2X  still needs sampled. MATLAB code is below
  K <- initReturn$sig2K * getCorMat(xTrain,initReturn$rhoV) + priors$epsV*diag(nrow(xTrain))
  initList$Vt <- exp(MASS::mvrnorm(1, initList$muV, K))
  return(initReturn)

}
