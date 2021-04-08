#' @title The scalar delta
#' @description
#' Calculate the scalar delta.
#' This parameter comes from the optimal Spearman’s correlation
#' when the rank of two vectors \code{X} and \code{proba} are equal except on a given set of indices.
#' In our context, this set correspond to the zero-values of the vector \code{proba}.
#' @param X a vector.
#' @param proba a zero-inflated proportions response.
#' @return \code{Delta} the scalar Delta calculated for the vector \code{x}
#' and the vector \code{proba}.
#' @examples
#' X = rnorm(100)
#' proba = runif(100)
#' proba[sample(1:100,80)]=0
#' Delta = delta(X,proba)
#' print(Delta)
#' @export

delta = function(X, proba)
{
  Rz = rank(proba)
  ind_z0 = which(proba == 0)
  Rx = rank(X[ind_z0])
  n0=length(ind_z0)
  n = length(proba)
  Delta = (sum(Rx^2)-((n0*(n0+1)^2)/4))/((n-1)*var(Rz))
  return(Delta)
}
