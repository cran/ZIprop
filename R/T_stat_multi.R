#' @title Statistic for non-numeric factor multiple tests
#' @description
#' Statistic for non-numeric factor multiple tests (difference in mean ranks).
#' @param permu the response vector.
#' @param al the factor.
#' @return the means difference of two levels for a discrete factor.
#' @examples
#' permu = runif(100,-10,10)
#' al = as.factor(sample(1:3,100,replace=TRUE))
#' T_stat_multi(permu, al)
#' @export

T_stat_multi = function(permu, al)
{
  ind_na = is.na(al)
  al = al[!ind_na]
  permu = rank(permu[!ind_na])
  #nlvel = levels(al)
  nlvel = sort(unique(al))
  T_stat = rep(NA, length(nlvel) - 1)
  count = 1
  for (l1 in 1:(length(nlvel) - 1))
  {
    for (l2 in (l1 + 1):length(nlvel))
    {
      T_stat[count] = (mean(permu[al %in% nlvel[l1]]) - mean(permu[al %in% nlvel[l2]]))
      count = count + 1
    }
  }
  names(T_stat) = apply(utils::combn(nlvel,2),2,function(al) paste(al,collapse =" - "))
  return(T_stat)
}
