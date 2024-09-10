#
# Class for LinRegTrunc
#



new_LinRegTrunc <- function(MMLFit,
                            formula,
                            truncation,
                            isLogTransformed,
                            constant) {
  me <- new_LinReg(MMLFit, formula, isLogTransformed, constant)
  class(me) <- c("LinRegTrunc", "LinReg")
  me$truncation <- truncation
  return(me)
}

#'
#' Fit a Linear Model with Truncated Gaussian Error Term
#'
#' The function first fits a regular linear model. The parameter
#' estimates are then used as starting values for the regression
#' with truncated Gaussian error term.
#'
#' @param formula a formula (e.g. "y ~ x")
#' @param data a data.frame object
#' @param truncation a numeric that sets the lower bound of the truncated distribution.
#' If the variable has been log transformed, the lower bound must represent the value
#' on the transformed scale.
#' @param isLogTransformed a logical, it is assumed the response variable has been
#' log transformed
#' @param constant a constant that has been added to the response variable before
#' the log transformation. It is assumed that this constant is 1.
#' @return an instance of the S3 LinRegTrunc class
#'
#' @export
LinRegTrunc <- function(formula,
                        data,
                        truncation,
                        isLogTransformed = T,
                        constant = 1) {
  .loadLibrary()
  message("LinRegTrunc: Converting data.frame instance to Java object...")
  jDataSet <- .convertDataIfNeeded(formula, data)
  message("LinRegTrunc: Fitting preliminary OLS model (without consideration for truncation)...")
  linMod <- J4R::createJavaObject("repicea.stats.model.lm.LinearModel", jDataSet, formula)
  linMod$doEstimation()
  if (!linMod$getEstimator()$isConvergenceAchieved()) {
    stop("Convergence could not be achieved!")
  }
  coef <- linMod$getParameters()
  sigma2 <- linMod$getResidualVariance()
  newCoef <- J4R::createJavaObject("repicea.math.Matrix", as.integer(coef$m_iRows + 1), as.integer(1))
  newCoef$setSubMatrix(coef, as.integer(0), as.integer(0))
  newCoef$setValueAt(as.integer(newCoef$m_iRows - 1), as.integer(0), sigma2)

  message("LinRegTrunc: Fitting model with truncated error distribution...")
  linRegTrunc <- J4R::createJavaObject("repicea.stats.model.lm.LinearModelWithTruncatedGaussianErrorTerm",
                                       jDataSet,
                                       formula,
                                       newCoef,
                                       truncation)
  linRegTrunc$doEstimation()
  if (!linRegTrunc$getEstimator()$isConvergenceAchieved()) {
    stop("Convergence could not be achieved!")
  }

  output <- new_LinRegTrunc(linRegTrunc, formula, truncation, isLogTransformed, constant)
  return(output)
}



