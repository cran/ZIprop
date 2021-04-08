#' @title Permutations tests
#' @description
#' Permutations tests to identify factor correlated
#' to a zero-inflated proportions response. The statistic are the Spearman's
#' correlation for numeric factor and mean by level for other factor.
#' @param DT a data table contains the factors and the response.
#' @param ColNameFactor a char vector with the name of the selected factor.
#' @param B number of permutations (use at least B=1000 permutations to get a correct accuracy of the p-value.)
#' @param nclust number of proc for parallel computation.
#' @param ColNameWeight a char with the name of the ZI response.
#' @param ColNameRecep colname of the column with the target names
#' @param ColNameSource colname of the column with the contributor names
#' @param seed vector with the seed for the permutations: size(\code{seed})=\code{B}
#' @param no_const FALSE for receiver block constraint for permutations: TRUE no constraint.
#' @param num_class a char vector with the name of numeric factor.
#' @param other_class a char vector with the name of other classes than numeric (factor or char).
#' @param multiple_test useful option only for discrete factors: Set TRUE to calculate multiple tests.
#' @param adjust_method  p-values adjusted methods (default "none" ). c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none").
#' @param alpha significant level (default 0.05).
#' @return A data frame with two columns. One for the statistics and the other one for the p-value.
#' @examples
#' library(data.table)
#' data(example_data)
#' res = permDT (example_data,
#' colnames(example_data)[c(4,10,14,20)],
#' B = 10,
#' nclust = 1,
#' ColNameWeight = "y",
#' ColNameRecep = "ID.recep",
#' ColNameSource = "ID.source",
#' seed = NULL,
#' num_class = colnames(example_data)[c(4,10)],
#' other_class = colnames(example_data)[c(14,20)])
#' print(res)
#' @export
#'
permDT = function(DT,
                  ColNameFactor,
                  B = 1000 ,
                  nclust = 1,
                  ColNameWeight = "weight",
                  ColNameRecep = "ID.recep",
                  ColNameSource = "ID.source",
                  seed = NULL,
                  no_const = FALSE,
                  num_class = ColNameFactor,
                  other_class = NULL,
                  multiple_test = FALSE,
                  adjust_method = "none",
                  alpha = 0.05)
{
  permu = weight = ID.recep = NULL
  DT2 = DT
  if ("weight" %in% names(DT2) &
      names(DT2)[names(DT2) == ColNameWeight] != "weight")
  {
    warning("weight rename weightS")
    names(DT2)[names(DT2) == "weight"] = "weightS"
  }


  if (length(seed) != B)
  {
    if (length(seed) > 0)
    {
      warning(
        "seed size must be equal to B \n take seed 1:B"
      )
    }
      seed = sample(1:1000000, B)
  }

  names(DT2)[names(DT2) == ColNameWeight] = "weight"
  names(DT2)[names(DT2) == ColNameRecep] = "ID.recep"
  names(DT2)[names(DT2) == ColNameSource] = "ID.source"

  ### Verification de la table
  if (min(DT2$weight) < 0)
  {
    stop("proba has to be positive")
  }
  if (max(DT2$weight) > 1)
  {
    stop("proba has to be lower than or equal to one")
  }


  if (all.equal(sort(unique(DT2$weight)), c(0, 1)) == TRUE)
  {
    stop("if the probabilities are 0 or 1, this method is not appropriate")
  }



  DT3 = DT2

  #num_class = names(which(sapply(DT3[, ColNameFactor, with = FALSE], class) == "numeric"))
  #other_class = names(which(sapply(DT3[, ColNameFactor, with = FALSE], class) != "numeric"))


  res_permu = mclapply(seed, function(b)
  {
    #DT3 = na.omit(DT2[,c("weight",ColNameFactor,"ID.recep","ID.source"),with = FALSE])

    set.seed(b)
    DT3[, permu := sample(weight), by = ID.recep]

    if (no_const)
    {
      set.seed(b)
      DT3[, permu := sample(weight)]
    }




    if (DT3[1, 1] == "permImp")
    {
      return(NA)
    } else
    {
      test = sum(DT3[, sum(permu), by = ID.recep]$V1 - DT3[, sum(weight), by = ID.recep]$V1)
      if (test > 1e-10)
      {
        print("permu error")
      }

      PT = NULL
      PT_multi = NULL
      if (length(num_class) > 0)
      {
        PT = c(PT,
               lapply(num_class, function(al)
                 cor(DT3$permu, DT3[[al]], method = "spearman")))
      }

      if (length(other_class) > 0)
      {
        PT = c(PT,
               lapply(other_class, function(al)
                 T_stat_discr(DT3$permu, DT3[[al]])))
        if(multiple_test)
        {
          PT_multi = c(PT_multi,
                         lapply(other_class, function(al)
                           T_stat_multi(DT3$permu, DT3[[al]])))
        }
      }

      names(PT) = c(num_class, other_class)

      return(list(PT=PT, PT_multi = PT_multi))

    }

  }, mc.cores = nclust)

  PT = NULL
  PT_multi = NULL
  if (length(num_class) > 0)
  {
    PT = c(PT,
           lapply(num_class, function(al)
             cor(DT3$weight, DT3[[al]], method = "spearman")))
  }

  if (length(other_class) > 0)
  {
    PT = c(PT,
           lapply(other_class, function(al)
             T_stat_discr(DT3$weight, DT3[[al]])))
    if(multiple_test)
    {
      PT_multi = c(PT_multi,
                   lapply(other_class, function(al)
                     T_stat_multi(DT3$weight, DT3[[al]])))
    }
  }

  names(PT) = c(num_class, other_class)





  PT = c(list(PT), lapply(res_permu,function(x) x$PT))
  PT = purrr :: transpose(PT)


  df_stat = unlist(sapply(ColNameFactor,function(I) PT[[I]][[1]]))



  df_pval = sapply(ColNameFactor, function(I)
  {
    if(I %in% num_class)
    {
      return(c(mean(abs(unlist(PT[[I]][-1]))>=abs(PT[[I]][[1]])),
                   mean(unlist(PT[[I]][-1])<=PT[[I]][[1]]),
                   mean(unlist(PT[[I]][-1])>=PT[[I]][[1]])))
    }else
    {
      return(c(mean(abs(unlist(PT[[I]][-1]))>=abs(PT[[I]][[1]])),
               NA,
               NA))
    }

  })

  dt_row_names = names(df_stat)
  dt = data.frame(stat = df_stat, pv_diff = df_pval[1,], pv_neg = df_pval[2,], pv_pos = df_pval[3,])
  rownames(dt) = dt_row_names

  if(length(other_class) > 0 & multiple_test)
  {
    names(PT_multi) = c(other_class)
    PT_multi = c(list(PT_multi), lapply(res_permu,function(x) x$PT_multi))
    PT_multi = purrr :: transpose(PT_multi)
    #signif = which(dt[rownames(dt) %in% other_class,2]<alpha)
    #list_tot = lapply(PT_multi[signif], function(al)
    list_tot = lapply(PT_multi, function(al)
      {
      stat = al[[1]]
      pval = apply(matrix(unlist(al),ncol = length(al[[1]]),byrow = T),2,function(x)
      {
        return(c(mean(abs(x[-1])>=abs(x[1])),
               mean(x[-1]<=x[1]),
               mean(x[-1]>=x[1])))

      })
      pval = apply(pval,1,function(pv) stats::p.adjust(pv,method = adjust_method))
      dt = data.frame(cbind(stat, matrix(pval,ncol=3)))
      names(dt) = c("stat", "pv_diff","pv_neg","pv_pos")
      return(dt)
    })
    return(list(dt=dt, dt_multi = list_tot))
  }
  return(dt)
}
