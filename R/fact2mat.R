#' @title Turn factor into multiple column
#' @description
#' Turns a factor with several levels into a matrix with several columns composed of zeros and ones.
#' @param x a vector.
#' @return Columns with zeros and ones.
#' @examples
#' x = sample(1:3,100,replace = TRUE)
#' fact2mat(x)
#' @export

fact2mat = function(x)
{
  return(sapply(sort(unique(x)), function(al) as.numeric(x==al)))
}
