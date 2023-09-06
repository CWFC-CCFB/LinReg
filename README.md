The LinRegTrunc package
=======================

<!-- badges: start -->
[![R-CMD-check](https://github.com/CWFC-CCFB/LinRegTrunc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CWFC-CCFB/LinRegTrunc/actions/workflows/R-CMD-check.yaml)
[![License: LGPL v3](https://img.shields.io/badge/License-LGPL_v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
<!-- badges: end -->

An R package for linear regression with residual errors following a truncated normal distribution.

## Introduction

It may happen that the response variable is subject to natural constraints. For instance, in forestry, allometric relationships and tree growth observations cannot be negative by definition. 
This results in a truncated distribution for the residual errors. The LinRegTrunc package implements a maximum likelihood estimator accounting for an eventual truncation of the distribution of
the residual error terms. 

The source code of the SIMEXGLM package is freely available at https://github.com/CWFC-CCFB/LinRegTrunc .

The LinRegTrunc package is licensed under the GNU Lesser General Public License v3 (LGPL-3.0).

Tickets can be created at https://github.com/CWFC-CCFB/LinRegTrunc/issues .

Mathieu Fortin
e-mail: mathieu.fortin.re@gmail.com

## How to install the package

The LinRegTrunc package can be installed directly from GitHub using the remotes package:

~~~R
library(remotes)
install_github("CWFC-CCFB/LinRegTrunc") ### install LinRegTrunc directly from GitHub
~~~

## Example of code

A data.frame object is available within the package: 

~~~R
require(LinRegTrunc)
data("datasetSingleObs")
~~~

It contains the diameter increment data for black spruce trees in the Province of Québec. These data 
were extracted from the provincial database of permanent sample plot which is licensed under CC-BT 4.0 and
made freely available by Quebec Ministry of Natural Resources and Forests (Ministère des Ressources naturelles et des
Forêts du Québec) at https://www.donneesquebec.ca/recherche/dataset/placettes-echantillons-permanentes-1970-a-aujourd-hui .

The subset available in the 'datasetSingleObs' data.frame object contains the following fields:

* y: the diameter increment (cm)
* yTrans: the log transformed response variable, i.e. ln(y + 1)
* year0: the year at the beginning of the growth interval
* j: the tree index
* BAL: the basal area of trees with DBH larger than the subject tree
* dbhCM: the diameter at breast height (DBH, 1.3 m in height)
* lnDt_corr: the logarithm of the interval duration (yr)
* newID_PE: the plot id

A reasonable linear model with error terms following a truncated normal distribution can be fitted as follows: 

~~~R 
fit <- LinRegTrunc("yTrans ~ lnDt_corr + dbhCm + BAL", datasetSingleObs, 0) # truncation below 0
summary(fit)
plot(fit)

shutdownClient()
~~~

The call to the <code>shutdownClient</code> function shuts down the client and the Java server as well avoiding having an idle process. 

