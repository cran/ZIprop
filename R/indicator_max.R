#' @title The max performance indicator
#' @description
#' Search for the set of parameters that maximize the indicator (equivalent to Spearman correlation).
#' For a given set of factors scaled between 0 and 1 and a
#' zero-inflated proportions response.
#' @param DT a data table contains the factors and the response.
#' @param ColNameFactor a char vector with the name of the selected factor.
#' @param ColNameWeight a char with the name of the ZI response.
#' @param bounds default is $[-10;10]$. Upper and Lower bounds.
#' @param max_generations default is 200 see \link[rgenoud]{genoud} for more information.
#' @param hard_limit default is TRUE see \link[rgenoud]{genoud} for more information.
#' @param wait_generations default is 50 see \link[rgenoud]{genoud} for more information.
#' @param other_class a char vector with the name of other classes than numeric (factor or char).
#' @return Return a list of two elements with the value of the indicator and the associate set of parameters (beta).
#' @examples
#' library(data.table)
#' data(example_data)
#' # For real cases increase max_generations and wait_generations
#' I_max = indicator_max(example_data,
#' names(example_data)[c(4:8, 14:18)],
#' ColNameWeight = "proba",
#' max_generations = 20,
#' wait_generations = 5)
#' print(I_max)
#' @export

indicator_max = function(DT,
                         ColNameFactor,
                         ColNameWeight = "weight",
                         bounds = c(-10,10),
                         max_generations = 200,
                         hard_limit = TRUE,
                         wait_generations = 50,
                         other_class = NULL)
{
  if(length(ColNameFactor)<2)
  {
    stop("Please give more than one factor")
  }

  matrix_model = model_matrix(DT, ColNameFactor,other_class)
  p = ncol(matrix_model)
  domain = cbind(rep(bounds[1], p), rep(bounds[2], p))
  invisible(capture.output(res <- genoud(
    function(x)    abs(cor(matrix_model%*%x,DT[[ColNameWeight]],method = "spearman")),
    p,
    max.generations = max_generations,
    max = TRUE,
    hard.generation.limit = hard_limit,
    wait.generations = wait_generations,
    Domains = domain
  )))

  cor.s = cor(matrix_model%*%res$par,DT[[ColNameWeight]],method = "spearman")
  I_max = list()
  I_max$beta = as.numeric(res$par)
  if(cor.s<0)
  {
    I_max$beta = -I_max$beta
  }
  I_max$indicator = indicator(matrix_model%*%res$par,DT[[ColNameWeight]])

  names(I_max$beta) = colnames(matrix_model)
  return (I_max)
}



