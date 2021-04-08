#' @title Scale vector
#' @description
#' Scale a vector between 0 and 1.
#'
#' @param x a vector.
#' @return the scaled vector of \code{x}.
#' @examples
#' x = runif(100,-10,10)
#' x_scale = scale_01(x)
#' range(x_scale)
#' @export
scale_01 = function(x)
{
  a = min(x,na.rm = TRUE)
  b = max(x, na.rm = TRUE)
  return((x-a)/(b-a))
}
