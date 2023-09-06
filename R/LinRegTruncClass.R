#
# Class for LinRegTrunc
#

#'
#' Provide a Summary of the fit
#'
#' @param object an instance of the S3 class linregtrunc
#' @param ... absolutely useless, just to keep the signature consistent for inheritance
#'
#' @export
summary.linregtrunc <- function(object, ...) {
  cat(object$summary)
}


#'
#' Provide the Coefficient (Parameter Estimates) of the Model
#'
#' @param object an instance of the S3 class linregtrunc
#' @return a named vector
#'
#' @export
coef.linregtrunc <- function(object) {
  return(object$coef)
}


#'
#' Provide the Model Predictions
#'
#' @param object an instance of the S3 class linregtrunc
#' @return a vector
#'
#' @export
fitted.linregtrunc <- function(object) {
  return(object$fitted)
}

#'
#' Provide the Estimated Variance-Covaraince of the Model Coefficients
#'
#' @param object an instance of the S3 class linregtrunc
#'
#' @return a matrix
#'
#' @export
vcov.linregtrunc <- function(object) {
  return(object$vcov)
}


#'
#' Provide the Model Predictions on the Original Scale
#'
#' If a log transformation was used, the model predictions are
#' back transformed to the original scale.
#'
#' @param object an instance of the S3 class linregtrunc
#' @return a vector
#'
#' @export
fittedOriginalScale <- function(object) {
  return(object$predictedOriginalScale)
}


#'
#' Produce a Graph of Residuals Against Predicted Values
#'
#' If a log transformation was used, the model predictions are
#' back transformed to the original scale.
#'
#' @param x an instance of the S3 class linregtrunc
#' @param ... completely useless. Just for inheritance
#' @return a graph
#'
#' @export
plot.linregtrunc <- function(x, ...) {
  base::plot(x = x$fitted,
             y = x$resid,
             ylab = "Residuals",
             xlab = "Predicted values")
  graphics::abline(0,0)
}

new_LinRegTrunc <- function(MMLFit,
                            formula,
                            truncation,
                            isLogTransformed,
                            constant) {
  me <- new.env(parent = emptyenv())
  class(me) <- c("linregtrunc")

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
  me$truncation <- truncation
  if (isLogTransformed) {
    me$predictedOriginalScale <- .convertJavaMatrixToR(MMLFit$getPredictedOriginalScale()) - constant
  } else {
    me$predictedOriginalScale <- me$predicted
  }

  return(me)
}




