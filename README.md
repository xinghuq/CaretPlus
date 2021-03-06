[![Build Status](https://travis-ci.org/topepo/caret.svg?topepo=master)](https://travis-ci.org/topepo/caret)
[![Coverage Status](https://coveralls.io/repos/topepo/caret/badge.svg?branch=master)](https://coveralls.io/r/topepo/caret?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/caret)](http://cran.r-project.org/web/packages/caret)
[![Downloads](http://cranlogs.r-pkg.org/badges/caret)](http://cran.rstudio.com/package=caret)
[![R build status](https://github.com/topepo/caret/workflows/R-CMD-check/badge.svg)](https://github.com/topepo/caret/actions)
  
This repository is a modified version of "caret" for adapting DeepGenomeScan, which includes CNN and it's importance estimation, as well as some updates on adaptive resampling. 
The original caret package can't accommodate some functions for DeepGenomeScan, please install this version if you use DeepGenomeScan. 

Users should refer to the original package caret from CRAN or topepc/caret. For more informatio about the caret pcakge, below is the link to the introduction of caret package.
Miscellaneous functions for training and plotting classification and regression models.  Detailed documentation is at https://xinghuq.github.io/CaretPlus/.

## Installation of current version

``````{r}
# install this version from github
remotes::install_github("xinghuq/CaretPlus/pkg/caret")

``````

## Citation of caret

Kuhn, M. (2015). Caret: classification and regression training. ascl, ascl-1505.
