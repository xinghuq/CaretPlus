[![Build Status](https://travis-ci.org/topepo/caret.svg?topepo=master)](https://travis-ci.org/topepo/caret)
[![Coverage Status](https://coveralls.io/repos/topepo/caret/badge.svg?branch=master)](https://coveralls.io/r/topepo/caret?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/caret)](http://cran.r-project.org/web/packages/caret)
[![Downloads](http://cranlogs.r-pkg.org/badges/caret)](http://cran.rstudio.com/package=caret)
[![R build status](https://github.com/topepo/caret/workflows/R-CMD-check/badge.svg)](https://github.com/topepo/caret/actions)
  
This repository is a modified version of "caret" for adapting DeepVariantScan, which added CNN and it's importance estimation, as well as some updates on adaptive resamplings. 
The original caret package can't accomodate some smooth runing functions for DeepVariantScan becasue of some bugs. 

Users should refer to the original package caret from CRAN or topepc/caret. Below is the link to the introduction of caret package.
Miscellaneous functions for training and plotting classification and regression models.  Detailed documentation is at http://topepo.github.io/caret/index.html
## Installation 
``````{r}
# install this version from github
remotes::install_github("xinghuq/CaretPlus/pkg/caret")
``````
