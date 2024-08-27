########################################################
# Basic R function for the package.
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: April 2024
########################################################


jarFilenames <- c("repicea-1.10.4.jar", "repicea-mathstats-1.4.4.jar")

.welcomeMessage <- function() {
  packageStartupMessage("Welcome to LinRegTrunc!")
  packageStartupMessage("The LinRegTrunc package implements the linear regression with residual error terms")
  packageStartupMessage("following a truncated normal distribution.")
  packageStartupMessage("Please, make sure that Java (version 8 or later) is installed on your computer.")
  packageStartupMessage("For more information, visit https://github.com/CWFC-CCFB/LinRegTrunc .")
}


.onAttach <- function(libname, pkgname) {
  .welcomeMessage()
}

.onUnload <- function(libpath) {
  shutdownClient()
}

.onDetach <- function(libpath) {
  shutdownClient()
}


#'
#' A data.frame object for an example of regression with
#' residual error terms following a truncated normal distribution.
#'
#' @docType data
#'
#' @usage data(datasetSingleObs)
#'
#' @keywords datasets
#'
#' @examples
#' data(datasetSingleObs)
"datasetSingleObs"

#'
#' Extends the shutdownJava function of the J4R package
#'
#' @export
shutdownClient <- function() {
  if (J4R::isConnectedToJava()) {
    J4R::shutdownClient()
  }
}


.loadLibrary <- function(memSize = NULL) {
  if (J4R::isConnectedToJava()) {
    for (jarName in jarFilenames) {
      if (!J4R::checkIfClasspathContains(jarName)) {
        stop(paste("It seems J4R is running but the class path does not contain this library: ", jarName, ". Shut down J4R using the shutdownClient function first and then re-run your code."))
      }
    }
  } else {
    path <- system.file(jarFilenames, package = "LinRegTrunc", mustWork = T)
    J4R::connectToJava(extensionPath = path, memorySize = memSize)
    for (jarName in jarFilenames) {
      if (!J4R::checkIfClasspathContains(jarName)) {
        stop(paste("It seems J4R has not been able to load the", jarName, "library."))
      }
    }
    loggerName <- J4R::getJavaField("repicea.stats.estimators.MaximumLikelihoodEstimator", "LOGGER_NAME")
    logger <- J4R::callJavaMethod("repicea.util.REpiceaLogManager", "getLogger", loggerName)
    level <- J4R::getJavaField("java.util.logging.Level", "WARNING")
    logger$setLevel(level)
  }
}

.createDataSet <- function(formula, data) {
  .loadLibrary()

  formattedString <- gsub("\n", "", formula)
  formattedString <- gsub(" ", "", formattedString)
  firstSplit <- strsplit(formattedString, "~")[[1]]
  secondSplit <- strsplit(firstSplit[2], "\\+")[[1]]
  thirdSplit <- unlist(strsplit(secondSplit, "\\*"))
  fourthSplit <- unlist(strsplit(thirdSplit, "\\:"))
  uncorrectedFieldNames <- c(firstSplit[1], fourthSplit)
  fieldNames <- unique(uncorrectedFieldNames)

  data.tmp <- data

  myDataSet <- J4R::createJavaObject("repicea.stats.data.DataSet")

  for (f in fieldNames) {
    myObjectArray <- J4R::createJavaObject("java.lang.Object", length(data.tmp[,1]), isArray = TRUE)
    if (f %in% colnames(data.tmp)) {
      J4R::setValueInArray(myObjectArray, data.tmp[, f])
      myDataSet$addField(f, myObjectArray)
    }
  }
  return(myDataSet)
}

.convertDataIfNeeded <- function(formula, data) {
  if ("java.object" %in% class(data) && data$.class == "repicea.stats.data.DataSet") {
    dataSet <- data
  } else {
    dataSet <- .createDataSet(formula, data)
  }
  return(dataSet)
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



.convertJavaMatrixToR <- function(jObject) {
  jMatrixClass <- J4R::callJavaMethod("java.lang.Class", "forName", "repicea.math.Matrix")
  cl <- jObject$getClass()
  if (!jMatrixClass$isAssignableFrom(cl)) {
    stop("The jObject argument should be a jObject pointing to a repicea.math.Matrix instance")
  }
  nrows <- jObject$m_iRows
  ncols <- jObject$m_iCols
  m <- matrix(nrow = nrows, ncol = ncols)
  for (i in 0:(jObject$m_iCols - 1)) {
    index <- 0:(jObject$m_iRows - 1)
    m[index + 1, i + 1] <- jObject$getValueAt(index , i)
  }
  if (nrows == 1 || ncols == 1) {
    return(as.vector(m))
  } else {
    return(m)
  }
}





