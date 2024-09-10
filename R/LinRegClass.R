#
# Class for LinReg
#

#'
#' Provide a Summary of the fit
#'
#' @param object an instance of the S3 class LinReg
#' @param ... absolutely useless, just to keep the signature consistent for inheritance
#'
#' @export
summary.LinReg <- function(object, ...) {
  cat(object$summary)
}


#'
#' Provide the Coefficient (Parameter Estimates) of the Model
#'
#' @param object an instance of the S3 class LinReg
#' @return a named vector
#'
#' @export
coef.LinReg <- function(object) {
  return(object$coef)
}


#'
#' Provide the Estimated Variance-Covariance of the Model Coefficients
#'
#' @param object an instance of the S3 class LinReg
#'
#' @return a matrix
#'
#' @export
vcov.LinReg <- function(object) {
  return(object$vcov)
}


#'
#' Produce a Graph of Residuals Against Predicted Values
#'
#' If a log transformation was used, the model predictions are
#' back transformed to the original scale.
#'
#' @param x an instance of the S3 class LinReg
#' @param ... completely useless. Just for inheritance
#' @return a graph
#'
#' @export
plot.LinReg <- function(x, ...) {
  base::plot(x = x$fitted,
             y = x$resid,
             ylab = "Residuals",
             xlab = "Predicted values")
  graphics::abline(0,0)
}

#'
#' Provide the Model Predictions
#'
#' @param object an instance of the S3 class LinReg
#' @param type either transformed or original (if there was a log transformation)
#' @param includeVariance a logical
#' @return a vector (or a matrix if the variance is included)
#'
#' @export
fitted.LinReg <- function(object, type="transformed", includeVariance=T) {
  if (type == "transformed") {
    return(object$fitted)
  } else if (type == "original") {
    if (includeVariance) {
      out <- cbind(object$predictedOriginalScale, object$varianceOriginalScale)
      colnames(out) <- c("pred", "varPred")
      return(out)
    } else {
      return(object$predictedOriginalScale)
    }
  }
}

#'
#' Provide Predictions on the Original Scale.
#'
#' The method assumes that the responses are log transformed.
#' @param object a LinReg S3 instance
#' @param newdata a data.frame object with all the fields required by the model plus the y field set to NA
#' @param estimator a character string among these ones: Baskerville, BeauchampAndOlson, MonteCarlo, or ComplexMonteCarlo
#'
#' @export
predict.LinReg <- function(object, newdata, estimator) {
  if (methods::is(object, "LinRegTrunc")) {
    stop("This method has not been implemented for the LinRegTrunc class yet!")
  }
  if (object$isLogTransformed) {
    jEstimator <- J4R::createJavaObject("repicea.stats.model.lm.LogBackTransformation$Estimator", estimator)
    jDataSet <- .convertDataIfNeeded(object$formula, newdata)
    jDataStructure <- J4R::createJavaObject("repicea.stats.data.GenericStatisticalDataStructure", jDataSet)
    jDataStructure$setModelDefinition(object$formula)
    xMat <- jDataStructure$constructMatrixX()
    pred <- J4R::callJavaMethod("repicea.stats.model.lm.LogBackTransformation", "getMeanPredictedValuesOnOriginalScale", object$JavaModel, xMat, object$constant, jEstimator)
    pred <- .convertJavaMatrixToR(pred)
    return(pred)
  } else {
    stop("The model is apparently not based on log-transformed responses.")
  }
}

new_LinReg <- function(MMLFit,
                            formula,
                            isLogTransformed,
                            constant) {
  me <- new.env(parent = emptyenv())
  class(me) <- c("LinReg", "LinModel")
  me$JavaModel <- MMLFit
  me$formula <- formula

  me$summary <- MMLFit$getSummary()
  me$coef <- .convertJavaMatrixToR(MMLFit$getParameters())
  me$vcov <- .convertJavaMatrixToR(MMLFit$getEstimator()$getParameterEstimates()$getVariance())
  me$isLogTransformed <- isLogTransformed
  me$constant <- constant

  predicted <- MMLFit$getPredicted()
  range <- 0:(predicted$m_iRows-1)
  me$fitted <- predicted$getValueAt(range, as.integer(0))
  me$resid <- .convertJavaMatrixToR(MMLFit$getResiduals())
  if (isLogTransformed) {
    predAndVariance <- .convertJavaMatrixToR(MMLFit$getPredOnLogBackTransformedScale(constant, TRUE))
    me$predictedOriginalScale <- predAndVariance[,1]
    me$varianceOriginalScale <- predAndVariance[,2]
  } else {
    me$predictedOriginalScale <- me$predicted
    me$varianceOriginalScale <- MMLFit$getResidualVariance()
  }
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
#' @param isLogTransformed a logical, it is assumed the response variable has been
#' log transformed
#' @param constant a constant that has been added to the response variable before
#' the log transformation. It is assumed that this constant is 1.
#' @return an instance of the S3 LinReg class
#'
#' @export
LinReg <- function(formula,
                   data,
                   isLogTransformed = F,
                   constant = 1) {
  .loadLibrary()
  message("LinReg: Converting data.frame instance to Java object...")
  jDataSet <- .convertDataIfNeeded(formula, data)
  message("LinReg: Fitting OLS model...")
  linMod <- J4R::createJavaObject("repicea.stats.model.lm.LinearModel", jDataSet, formula)
  linMod$doEstimation()
  if (!linMod$getEstimator()$isConvergenceAchieved()) {
    stop("Convergence could not be achieved!")
  }
  output <- new_LinReg(linMod, formula, isLogTransformed, constant)
  return(output)
}



