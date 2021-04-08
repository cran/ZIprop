#' @title Construct Design Matrix
#' @description
#' Creates a design matrix
#' by expanding factors to a set of dummy variables.
#' @param DT a data table contains the factors and the response.
#' @param ColNameFactor a char vector with the name of the selected factor.
#' @param other_class a char vector with the name of other classes than numeric (factor or char).
#' @return return the value.
#' @examples
#' library(data.table)
#' data(example_data)
#' m = model_matrix (example_data,
#' colnames(example_data)[-c(1:3)],
#' other_class = colnames(example_data)[14:23])
#' print(m)
#' @export

model_matrix = function(DT, ColNameFactor,
                        other_class)
{
  matrix_model = NULL
  m_names = NULL
  for ( i in 1:length(ColNameFactor))
  {
    if(ColNameFactor[i]%in%other_class)
    {
      matrix_model = cbind(matrix_model,fact2mat(DT[[ColNameFactor[i]]]))
      m_names=c(m_names,paste(ColNameFactor[i],sort(unique(DT[[ColNameFactor[i]]])),sep = "_") )
    }else
    {
      matrix_model = cbind(matrix_model,scale_01(DT[[ColNameFactor[i]]]))
      m_names=c(m_names,ColNameFactor[i])
    }
  }
  colnames(matrix_model) = m_names
  return(matrix_model)
}
