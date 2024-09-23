########################################################
# Basic R function for the package.
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: April 2024
########################################################


jarFilenames <- c("repicea-1.10.4.jar", "repicea-mathstats-1.5.1.jar")

.welcomeMessage <- function() {
  packageStartupMessage("Welcome to LinReg!")
  packageStartupMessage("The LinReg package implements the linear regression with residual error terms")
  packageStartupMessage("following either a normal distribution or a truncated normal distribution.")
  packageStartupMessage("Please, make sure that Java (version 8 or later) is installed on your computer.")
  packageStartupMessage("For more information, visit https://github.com/CWFC-CCFB/LinReg .")
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
    path <- system.file(jarFilenames, package = "LinReg", mustWork = T)
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
