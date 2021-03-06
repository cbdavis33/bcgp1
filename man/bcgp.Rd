% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bcgp.R
\name{bcgp}
\alias{bcgp}
\title{Draw samples from a bcgp model}
\usage{
bcgp(xTrain, yTrain, priors = "default", inits = "random",
  numUpdates = 5, numAdapt = 1000, burnin = 1000, nmcmc = 10000,
  chains = 4, cores = getOption("mc.cores", 1L), noise = FALSE)
}
\arguments{
\item{xTrain}{An \code{n x d} matrix containing the independent variables
in the training set.}

\item{yTrain}{A vector containing the observed response values in the training
set.}

\item{priors}{Can be either the string "default" or a list containing the values
for the prior parameters.

\describe{
\code{priors = "default"} (default):
The priors are given default values.

\code{priors} via list:
Set prior values by providing a list equal in length to the number of Markov
chains. A call to \code{createPriors()} will assist in the correct creation of this list.
}}

\item{inits}{Can be either the string "random" or a list of length \code{chains}.
The elements of this list should be named lists, where each of these has the
name of a parameter and is used to specify the initial values for that parameter
for the corresponding chain.

\describe{
\code{inits = "random"} (default):
The initial values will be generated randomly from their respective prior
distributions.

\code{inits} via list:
Set initial values by providing a list equal in length to the number of Markov
chains. A call to \code{createInits()} will assist in the correct creation of this list.
}}

\item{numUpdates}{The number of updates in the proposal stepsize adaptation phase.}

\item{numAdapt}{The number of samples within each update in the proposal stepsize
adaptation phase.}

\item{burnin}{The number of burnin samples to discard after the stepsize
adaptation phase is finished}

\item{nmcmc}{The number of samples to be kept for each Markov chain.}

\item{chains}{The number of Markov chains. The default is 4.}
}
\value{
An object of S4 class \code{bcgp} representing the fitted results.
}
\description{
\code{bcgp} draws samples from the Bayesian Composite Gaussian Process model
}
\details{
This draws samples from the posterior distribution for the Bayesian
Composite Gaussian Process (BCGP) model.
}
\examples{

xTrain <- matrix(runif(20, 0, 10), nrow = 10, ncol = 2)
yTrain <- xTrain[, 1] + sin(xTrain[, 2])
priors <- createPriors(noise = FALSE, d = 2)
bcgp(xTrain, yTrain, priors)
}
\seealso{
\code{\link{createPriors}} \code{\link{createInits}}

Other Major functions: \code{\link{bcgpMCMC}}
}
\concept{Major functions}
