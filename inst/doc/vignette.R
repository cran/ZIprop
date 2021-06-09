## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  install.packages("ZIprop")
#  install.packages("knitr")
#  install.packages("kableExtra")
#  install.packages("ggplot2")
#  install.packages("ggrepel")
#  install.packages("ggthemes")
#  install.packages("stringr")

## ---- echo=TRUE, results='asis', warning=FALSE--------------------------------
suppressPackageStartupMessages(library(ZIprop))
library(knitr)
library(kableExtra)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(stringr)

## -----------------------------------------------------------------------------
data(equineDiffFactors)
summary(equineDiffFactors)

## -----------------------------------------------------------------------------
res_equine = permDT (
  equineDiffFactors,
  ColNameFactor = colnames(equineDiffFactors)[-c(1:3)],
  B = 1000,
  nclust = 1,
  ColNameWeight = "weight",
  ColNameRecep = "ID.recep",
  ColNameSource = "ID.source",
  num_class = "DistYard",
  other_class = colnames(equineDiffFactors)[-c(1:3,7)],
  multiple_test = TRUE
)

## -----------------------------------------------------------------------------
kbl(res_equine$dt[,c(1,2)], caption = "ALL") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

## -----------------------------------------------------------------------------
kbl(res_equine$dt_multi$SameYard[,-c(3:4)], caption = "SameYard") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

## -----------------------------------------------------------------------------
kbl(res_equine$dt_multi$SameSex[,-c(3:4)], caption = "SameSex") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

## -----------------------------------------------------------------------------
kbl(res_equine$dt_multi$TransSex[,-c(3:4)], caption = "TransSex") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

## -----------------------------------------------------------------------------
DT_omitna = na.omit(equineDiffFactors[,c(1:3,4,5,7,8),with=F])
summary(DT_omitna )

## -----------------------------------------------------------------------------
system.time({I_max = indicator_max(
  DT_omitna,
  ColNameFactor = colnames(DT_omitna)[-c(1:3)],
  ColNameWeight = "weight",
  bounds = c(-10, 10),
  max_generations = 50,
  hard_limit = TRUE,
  wait_generations = 10,
  other_class = colnames(DT_omitna)[-c(1:3,6)]
)})

I_max$indicator

## -----------------------------------------------------------------------------
data(diffFactors)
summary(diffFactors)

## -----------------------------------------------------------------------------
res = permDT (
  diffFactors,
  ColNameFactor = colnames(diffFactors)[-c(1:3)],
  B = 1000,
  nclust = 1,
  ColNameWeight = "proba",
  ColNameRecep = "ID.recep",
  ColNameSource = "ID.source",
  multiple_test = TRUE
)

## ---- warning=FALSE, fig.width= 6.5, fig.height= 4----------------------------
res$stat2 = res$stat
res$stat2[res$pv_neg>0.05] = NA
res$Factor = str_remove(rownames(res),"_diff")
res$Factor = factor(res$Factor, levels = res$Factor)
res$dec = rep(c(0.15,-0.15),50)[1:nrow(res)]
res$dec[res$pv_neg>0.05] = NA

p = ggplot(res,aes(Factor,pv_neg)) + geom_point(colour = "black") + 
  ylim(-0.1,1) +
  xlab("") + 
  ylab("P-value") +
  theme_classic() +
  geom_rangeframe() + 
  geom_hline(yintercept = 0.05, color ="red") +
  #geom_text_repel(data = res, aes(label = round(stat2,2)),
  #                         point.padding = 1)  +
  geom_label_repel(data = res, aes(label = round(stat2,2)), fill = "white", 
                   nudge_y = na.omit(res$dec) )+
  ggtitle("Permutation test results")

p + theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5))

## ---- eval=FALSE--------------------------------------------------------------
#  ind_fact_select = which(colnames(diffFactors) %in%  rownames(res[res$pv_neg<0.05,]) )
#  
#  system.time({I_max = indicator_max(
#    diffFactors,
#    ColNameFactor = colnames(diffFactors)[c(ind_fact_select)],
#    ColNameWeight = "proba",
#    bounds = c(-10, 10),
#    max_generations = 200,
#    hard_limit = TRUE,
#    wait_generations = 50
#  )})
#  
#  I_max$indicator

