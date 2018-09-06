#' Create a list with prior information.
#'
#' \code{getCorMat} returns a list that contains default values for the priors.
#'
#' This creates a list that contains default values for the priors. The user
#' can change the values as they like prior to inputting the list into
#' \code{bcgp}.
#'
#' @param deterministic If the data is assumed to be noise-free, then
#' \code{deterministic} should be \code{TRUE}. Otherwise, it should be
#' \code{FALSE},
#' @param d The dimension of the training data.
#' @return A list containing the default values for all the prior parameters.
#' @family preprocessing functions
#' @examples
#' createPrior(d = 2)
#' createPrior(deterministic = FALSE, d = 3)

createPrior  <- function(deterministic = TRUE, d){
  priorList <- list(w = list(a = 0.5,
                             b = 1.0,
                             alpha = 1,
                             beta = 1),
                    rhoG = list(alpha = rep(1, d),
                                beta = rep(1,d)),
                    rhoL = list(alpha = rep(1, d),
                                beta = rep(1,d)),
                    muV = list(mu = -0.1,
                               sig2 = 0.1))

  if(deterministic){
    priorList$sig2eps <- list(alpha = 1e-3,
                              beta = 1e-3)
  }else{
    priorList$sig2eps <- list(alpha = 1e-1,
                              beta = 1e-1)
  }

  return(priorList)
}
