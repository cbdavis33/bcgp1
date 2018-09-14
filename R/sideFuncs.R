initFunc <- function(initList, priors){

  d <- length(priors$rhoG$alpha)
  initReturn <- vector("list")
  initReturn$mu <- rnorm(1, 0, 1)
  initReturn$w <- priors$w$a + rbeta(1, priors$w$alpha, priors$w$beta)*(priors$w$b - priors$w$a)
  initReturn$rhoG <- rbeta(d, priors$rhoG$alpha, priors$rhoG$beta)

  return(initReturn)

}
