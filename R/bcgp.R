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
#' xTrain <- matrix(runif(20, 0, 10), nrow = 10, ncol = 2)
#' yTrain <- xTrain[, 1] + sin(xTrain[, 2])
#' priors <- createPrior(noise = FALSE, d = 2)
#' bcgp(xTrain, yTrain, priors)

bcgp  <- function(xTrain, yTrain, priors = createPrior(noise = TRUE, d = ncol(xTrain)),
                  numUpdates = 5,
                  numAdapt = 1000,
                  burnin = 1000,
                  nmcmc = 10000){

  y <- scale(yTrain, center = TRUE, scale = TRUE)
  x <- apply(xTrain, 2, rescale)

  bfit <- new("bcgpfit",
              )
  return(bfit)
}
