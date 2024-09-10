The LinReg package
=======================

**IMPORTANT**: This package was formerly called LinRegTrunc. 

<!-- badges: start -->
[![R-CMD-check](https://github.com/CWFC-CCFB/LinRegTrunc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CWFC-CCFB/LinRegTrunc/actions/workflows/R-CMD-check.yaml)
[![License: LGPL v3](https://img.shields.io/badge/License-LGPL_v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
<!-- badges: end -->

An R package for linear regression with residual errors following a normal distribution or a truncated normal distribution.

## Introduction

This package implements a linear model with or without log-transformed responses. It may happen that the response variable is subject to natural constraints. For instance,
in forestry, allometric relationships and tree growth observations cannot be negative by definition. This results in a truncated distribution for the residual errors. 
The LinReg package implements a maximum likelihood estimator accounting for an eventual truncation of the distribution of the residual error terms. 

The source code of the LinReg package is freely available at https://github.com/CWFC-CCFB/LinReg .

The LinReg package is licensed under the GNU Lesser General Public License v3 (LGPL-3.0).

The backend of this R package is composed of two open-source Java libraries:
* repicea (LGPL-3.0) available at https://github.com/CWFC-CCFB/repicea .
* repicea-mathstats (LGPL-3.0) available at https://github.com/CWFC-CCFB/repicea-mathstats .

Tickets can be created at https://github.com/CWFC-CCFB/LinReg/issues.

Mathieu Fortin
e-mail: mathieu.fortin.re@gmail.com

## How to install the package

The LinReg package depends on [J4R](https://github.com/CWFC-CCFB/J4R/wiki), which requires Java 8. Please see the instruction at https://github.com/CWFC-CCFB/J4R/wiki#requirements . 

Once Java 8 has been installated, the LinReg package can be installed directly from GitHub using the remotes package:

~~~R
library(remotes)
install_github("CWFC-CCFB/LinReg") ### install LinReg directly from GitHub - the J4R package should be automatically installed as well
~~~

## Example of code

A data.frame object is available within the package: 

~~~R
require(LinReg)
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
fit <- LinRegTrunc("yTrans ~ lnDt_corr + dbhCm + BAL", datasetSingleObs, 0, isLogTransformed = T, constant = 1) # truncation below 0
summary(fit)
plot(fit)
~~~

The fitted function has been modified to provide predictions on the original scale. For instance,

~~~R
fitted(fit, type = "original")
~~~

provides a two-column matrix with the predictions and their estimated variances on the original scale. In this particular case, the constant one
has been automatically subtracted from the predictions, since the transformation was ln(y+1). If this constant is different from one, the constant argument
in the LinRegTrunc function should be set accordingly.

Finally, the call to the <code>shutdownClient</code> function shuts down the client and the Java server to avoid leaving an idle process in memory: 
~~~R
shutdownClient()
~~~




