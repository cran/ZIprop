#' @title The performance indicator
#' @description
#' Calculate the indicator for a vector \code{X}
#' and a zero-inflated proportions response \code{proba}.
#' @param X a vector.
#' @param proba a zero-inflated proportions response.
#' @return a scalar represents the performance indicator
#' and the vector \code{proba}.
#' @examples
#' X = rnorm(100)
#' proba = runif(100)
#' proba[sample(1:100,80)]=0
#' print(indicator(X,proba))
#' @export

indicator = function(X,proba)
{
  return(cor(X,proba,method = "spearman")^2/(1/(1+delta(X,proba))))
}
