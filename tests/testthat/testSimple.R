#'
#' Simple tests
#'

rm(list = ls())
require(LinRegTrunc)

data("datasetSingleObs")

fit <- LinRegTrunc("yTrans ~ lnDt_corr + dbhCm + BAL", datasetSingleObs, 0)
summary(fit)

expectedIntercept <- -1.82587355
expectedInterceptStd <-  0.09970163
actualInterceptStd <- vcov(fit)[1,1]^.5
expectedPred <- 0.7767616755318115
expectedPredOrigScale <- 2.441898354427898
test_that("Checking if a DataSet instance can be instantiated on Java end", {
  expect_equal(coef(fit)[1], expectedIntercept, tolerance = 1E-8)
  expect_equal(actualInterceptStd, expectedInterceptStd, tolerance = 1E-8)
  expect_equal(fitted(fit)[1], expectedPred, tolerance = 1E-8)
  expect_equal(fittedOriginalScale(fit)[1], expectedPredOrigScale - 1, tolerance = 1E-8)
})

fit <- LinRegTrunc("yTrans ~ lnDt_corr + dbhCm*BAL", datasetSingleObs, 0)
summary(fit)
expectedIntercept <- -1.78115064
expectedInterceptStd <-  0.10225128
actualInterceptStd <- vcov(fit)[1,1]^.5
expectedPred <- 0.776408367931
expectedPredOrigScale <- 2.44075217793
test_that("Checking if a DataSet instance can be instantiated on Java end", {
  expect_equal(coef(fit)[1], expectedIntercept, tolerance = 1E-8)
  expect_equal(actualInterceptStd, expectedInterceptStd, tolerance = 1E-8)
  expect_equal(fitted(fit)[1], expectedPred, tolerance = 1E-8)
  expect_equal(fittedOriginalScale(fit)[1], expectedPredOrigScale - 1, tolerance = 1E-8)
})



#plot(fit)

shutdownClient()
