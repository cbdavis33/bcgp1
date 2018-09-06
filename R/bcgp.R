#' Draw samples from a bcgp model
#'
#' \code{bcgp} draws samples from the Bayesian Composite Gaussian Process model
#'
#' This draws samples from the posterior distribution for the Bayesian
#' Composite Gaussian Process (BCGP) model.
#'
#' @param xTrain An \code{n x d} matrix containing the independent variables
#' in the training set.
#' @param yTrain Avector containing the observed response values in the training
#' set.
#' @param prior A list containing the values for the prior parameters.
#' @param numUpdates The number of updates in the proposal stepsize adaptation phase.
#' @param numAdapt The number of samples within each update in the proposal stepsize
#' adaptation phase.
#' @param burnin The number of burnin samples to discard after the stepsize
#' adaptation phase is finished
#' @return An object of S4 class \code{bcgp} representing the fitted results.
#' @family Major functions
#' @examples
#'
#' createPrior(d = 2)
#' createPrior(noise = TRUE, d = 3)

bcgp  <- function(xTrain, yTrain, priors = createPrior(noise = TRUE, d = ncol(xTrain)),
                  numUpdates = 5,
                  numAdapt = 1000,
                  burnin = 1000,
                  nmcmc = 10000){
  priorList <- list(w = list(a = 0.5,
                             b = 1.0,
                             alpha = 1,
                             beta = 1),
                    rhoG = list(alpha = rep(1, d),
                                beta = rep(1, d)),
                    rhoL = list(alpha = rep(1, d),
                                beta = rep(1, d)),
                    muV = list(mu = -0.1,
                               sig2 = 0.1),
                    rhoV = list(alpha = rep(1, d),
                                beta = rep(1, d)),
                    sig2K = list(alpha = 2 + sqrt(0.1),
                                 beta = 100/(1+sqrt(1/10))),
                    tau2 = 0.08,
                    epsV = 1e-10)

  if(!noise){
    priorList$sig2eps <- list(alpha = 1e-3,
                              beta = 1e-3)
  }else{
    priorList$sig2eps <- list(alpha = 1e-1,
                              beta = 1e-1)
  }

  return(priorList)
}
