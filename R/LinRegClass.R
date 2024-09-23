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
  cat(object$JavaModel$getSummary())
}


#'
#' Provide the Coefficient (Parameter Estimates) of the Model
#'
#' @param object an instance of the S3 class LinReg
#' @return a named vector
#'
#' @export
coef.LinReg <- function(object) {
  return(.convertJavaMatrixToR(object$JavaModel$getParameters()))
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
  return(.convertJavaMatrixToR(object$JavaModel$getEstimator()$getParameterEstimates()$getVariance()))
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
  fitted <- fitted.LinReg(x)
  resid <- residuals.LinReg(x)
  base::plot(x = fitted,
             y = resid,
             ylab = "Residuals",
             xlab = "Predicted values")
  graphics::abline(0,0)
}

#'
#' Provide the Model Predictions
#'
#' @param object an instance of the S3 class LinReg
#' @return a vector of fitted value
#'
#' @export
fitted.LinReg <- function(object) {
  return(.convertJavaMatrixToR(object$JavaModel$getPredicted()))
}

#'
#' Provide the Model Residuals
#'
#' @param object an instance of the S3 class LinReg
#' @return a vector of residuals
#'
#' @export
residuals.LinReg <- function(object) {
  jResiduals <- object$JavaModel$getResiduals()
  resVect <- .convertJavaMatrixToR(jResiduals)
  return(resVect)
}


#'
#' Provide Predictions on the Original Scale.
#'
#' If the response variable has not been log transformed, this function
#' is equivalent to fitted.LinReg.
#'
#' @param object a LinReg S3 instance
#' @param type either response or original. Original means the predictions are back transformed.
#' @param newdata a data.frame object with all the independent variables required by the model.
#' If null, then the original data are taken.
#' @param estimator a character string among these ones: Naive, BeauchampAndOlson, MonteCarlo, or ComplexMonteCarlo
#' @param addResidualVariance a logical (TRUE: add the residual variance, is set to FALSE by default)
#'
#' @export
predict.LinReg <- function(object, newdata = NULL, type = "response", estimator = "Naive", addResidualVariance = F) {
  if (type == "response" | !object$isLogTransformed) {
    xMat <- .getXMatrixFromNewdata(object, newdata)
    pred <- object$JavaModel$getPredicted(xMat)
    nrowPred <- pred$m_iRows
    pred <- .convertJavaMatrixToR(pred)
    if (addResidualVariance) {
      s2 <- object$JavaModel$getResidualVariance()
      pred.tmp <- matrix(nrow = nrow(pred), ncol = 2)
      pred.tmp[,1] <- pred
      pred.tmp[,2] <- s2
      pred <- pred.tmp
    }
    return(pred)
  } else if (type == "original") {
    xMat <- .getXMatrixFromNewdata(object, newdata)
    jEstimator <- J4R::createJavaObject("repicea.stats.model.lm.LogBackTransformation$Estimator", estimator)
    pred <- J4R::callJavaMethod("repicea.stats.model.lm.LogBackTransformation", "getMeanPredictedValuesOnOriginalScale", object$JavaModel, xMat, object$constant, jEstimator)
    nrowPred <- pred$m_iRows
    pred <- .convertJavaMatrixToR(pred)
    if (addResidualVariance) {
      s2 <- J4R::callJavaMethod("repicea.stats.model.lm.LogBackTransformation", "getResidualVariancesOnOriginalScale", object$JavaModel, xMat, jEstimator)
      s2 <- .convertJavaMatrixToR(s2)
      pred.tmp <- matrix(nrow = nrowPred, ncol = 2)
      pred.tmp[,1] <- pred
      pred.tmp[,2] <- s2
      pred <- pred.tmp
    }
    return(pred)
  }
}

.getXMatrixFromNewdata <- function(object, newdata) {
  if (is.null(newdata)) {
    return(J4R::createJavaObject("repicea.math.Matrix", as.integer(1), as.integer(1), isNullObject = T))
  } else {
    yVar <- trimws(strsplit(as.character(object$formula), "~")[[1]][1])
    if (!yVar %in% colnames(newdata)) {
      newdata[,yVar] <- NA
    }
    jDataSet <- .convertDataIfNeeded(object$formula, newdata)
    jDataStructure <- J4R::createJavaObject("repicea.stats.data.GenericStatisticalDataStructure", jDataSet)
    jDataStructure$setModelDefinition(object$formula)
    return(jDataStructure$constructMatrixX())
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

#  me$summary <- MMLFit$getSummary()
#  me$coef <- .convertJavaMatrixToR(MMLFit$getParameters())
#  me$vcov <- .convertJavaMatrixToR(MMLFit$getEstimator()$getParameterEstimates()$getVariance())
  me$isLogTransformed <- isLogTransformed
  me$constant <- constant

#  predicted <- MMLFit$getPredicted()
#  range <- 0:(predicted$m_iRows-1)
#  me$fitted <- predicted$getValueAt(range, as.integer(0))
#  me$resid <- .convertJavaMatrixToR(MMLFit$getResiduals())
  # if (isLogTransformed) {
  #   predAndVariance <- .convertJavaMatrixToR(MMLFit$getPredOnLogBackTransformedScale(constant, TRUE))
  #   me$predictedOriginalScale <- predAndVariance[,1]
  #   me$varianceOriginalScale <- predAndVariance[,2]
  # } else {
  #   me$predictedOriginalScale <- me$predicted
  #   me$varianceOriginalScale <- MMLFit$getResidualVariance()
  # }
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



