#'
#' Simple tests
#'

rm(list = ls())
require(LinReg)

data("datasetSingleObs")

fit <- LinReg("yTrans ~ lnDt_corr + dbhCm + BAL", datasetSingleObs, isLogTransformed = T)
summary(fit)

expectedIntercept <- -0.7229070890685245
expectedInterceptStd <-  0.05541272279088098
actualInterceptStd <- vcov(fit)[1,1]^.5
expectedPred <- 0.809628844760482
expectedPredOrigScale <- 1.512218464819957
expectedVarianceOrigScale <- 1.57726754340144559307
test_that("Testing linear regression with Gaussian errors", {
  expect_equal(coef(fit)[1], expectedIntercept, tolerance = 1E-8)
  expect_equal(actualInterceptStd, expectedInterceptStd, tolerance = 1E-8)
  expect_equal(fitted(fit)[1], expectedPred, tolerance = 1E-8)
  expect_equal(unname(fitted(fit, type = "original"))[1,1], expectedPredOrigScale, tolerance = 1E-8)
  expect_equal(unname(fitted(fit, type = "original"))[1,2], expectedVarianceOrigScale, tolerance = 1E-8)
})


fit <- LinRegTrunc("yTrans ~ lnDt_corr + dbhCm + BAL", datasetSingleObs, 0)
summary(fit)

expectedIntercept <- -1.82587355
expectedInterceptStd <-  0.09970163
actualInterceptStd <- vcov(fit)[1,1]^.5
expectedPred <- 0.7767616755318115
expectedPredOrigScale <- 1.4429778488
expectedVarianceOrigScale <- 1.8053520069
test_that("Testing linear regression with truncated Gaussian errors 2", {
  expect_equal(coef(fit)[1], expectedIntercept, tolerance = 1E-8)
  expect_equal(actualInterceptStd, expectedInterceptStd, tolerance = 1E-8)
  expect_equal(fitted(fit)[1], expectedPred, tolerance = 1E-8)
  expect_equal(unname(fitted(fit, type = "original"))[1,1], expectedPredOrigScale, tolerance = 1E-8)
  expect_equal(unname(fitted(fit, type = "original"))[1,2], expectedVarianceOrigScale, tolerance = 1E-8)
})


fit <- LinRegTrunc("yTrans ~ lnDt_corr + dbhCm*BAL", datasetSingleObs, 0)
summary(fit)
expectedIntercept <- -1.78115064
expectedInterceptStd <-  0.10225128
actualInterceptStd <- vcov(fit)[1,1]^.5
expectedPred <- 0.776408367931
expectedPredOrigScale <- 1.4418324681
test_that("Testing linear regression with truncated Gaussian errors 2", {
  expect_equal(coef(fit)[1], expectedIntercept, tolerance = 1E-8)
  expect_equal(actualInterceptStd, expectedInterceptStd, tolerance = 1E-8)
  expect_equal(fitted(fit)[1], expectedPred, tolerance = 1E-8)
  expect_equal(unname(fitted(fit, type = "original"))[1,1], expectedPredOrigScale, tolerance = 1E-8)
})


fit <- LinReg("yTrans ~ lnDt_corr + BAL", datasetSingleObs[1:100,], isLogTransformed = T)
summary(fit)
newdata <- data.frame(yTrans = rep(NA,3), lnDt_corr = rep(log(10), 3), dbhCm = rep(15, 3), BAL = c(10,20,30))
xMat <- matrix(nrow = 3, ncol = 3)
xMat[,1] <- 1
xMat[,2] <- newdata$lnDt_corr
xMat[,3] <- newdata$BAL
xBeta <- xMat %*% fit$coef
sigma2 <- fit$JavaModel$getResidualVariance()
baskerville_ref <- as.numeric(exp(xBeta + 0.5 * sigma2) - 1)
baskerville <- predict(fit, newdata, "Baskerville")

test_that("Testing log backtransformation estimators", {
  expect_true(all(abs(baskerville_ref-baskerville) <= 1E-8))
})

beauchamp <- predict(fit, newdata, "BeauchampAndOlson")
monteCarlo <- predict(fit, newdata, "MonteCarlo")
cMonteCarlo <- predict(fit, newdata, "ComplexMonteCarlo")

shutdownClient()
