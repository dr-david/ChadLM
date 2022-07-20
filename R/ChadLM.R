
#' ChadLM
#'
#' @param y respeonse vector
#' @param X covariates
#' @param lambda penalization parameter
#'
#' @return overestimated effect sizes
#' @export
#'
ChadLM <- function(y, X, lambda){
  stopifnot(lambda >= 0)
  optim(
    fn=function(x) {
      sum((y - X%*%x)^2) - lambda * sum(abs(x))
    },
    par = rep(0, ncol(X)),
    method = "L-BFGS-B"
  )$par
}
