#' @title Statistic for non-numeric factor tests
#' @description
#' Statistic for non-numeric factor tests (same statistic as H-test).
#' @param permu the response vector.
#' @param al the factor.
#' @return the statistic.
#' @examples
#' permu = runif(100,-10,10)
#' al = as.factor(sample(1:3,100,replace=TRUE))
#' T_stat_discr(permu, al)
#' @export

T_stat_discr = function(permu, al)
{
  ind_na = is.na(al)
  al = al[!ind_na]
  permu = rank(permu[!ind_na])
  m = mean(permu)
  #nlvel = levels(al)
  nlvel = unique(al)
  T_stat = 0
  count = 1
  for (l1 in 1:(length(nlvel)))
  {
    nq = length(permu[al %in% nlvel[l1]])
    T_stat = T_stat + nq*(mean(permu[al %in% nlvel[l1]])-m)^2
  }
  T_stat = T_stat/(length(permu)*var(permu))
  return(T_stat)
}
