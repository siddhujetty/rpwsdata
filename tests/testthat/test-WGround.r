#' Test script for WGround class.
#' @import stringr
#' @importFrom testthat matches equals
#'


#source("WGround_class.R")

## Test S4 class WGround
context("Testing WGround")

# Create object and run PWSbyCityState
obj1=new("WGround", city='Fremont',state = 'CA',runmode="offline") %>%
  getPWSinfo() #%>%

# Test that object of WGround class created
test_that("object of WGround class created", {
  expect_that(obj1, is_a("WGround"))
})

# Test that obj1@pws is a data frame
test_that("obj1@pws is a data frame", {
  expect_that(obj1@pws, is_a("data.frame"))
})

# Test that obj1@pws has columns needed
test_that("obj1@pws has columns needed", {
  expect_that(names(obj1@pws), testthat::matches("id",all = FALSE))
  expect_that(names(obj1@pws), testthat::matches("lat",all = FALSE))
  expect_that(names(obj1@pws), testthat::matches("lon",all = FALSE))
})

# Simulate user selects three PWS
obj1@selectedpws <- obj1@pws[obj1@pws$id == 'KCAFREMO83'|obj1@pws$id == 'KCAFREMO88',]
# Test that obj1@selectedpws has three or less rows
test_that("obj1@selectedpws has three or less rows",{
  expect_that(c("1","2","3"), testthat::matches(toString(nrow(obj1@selectedpws)),all = FALSE))
})

# Get current weather conditions for selected PWS
obj1 <- CurrentWeatherbyPWS(obj1)
# Test that obj1@conditions has same number of rows as obj1@selectedpws
test_that("obj1@conditions has same number of rows as obj1@selectedpws",{
  expect_that(nrow(obj1@conditions), testthat::equals(nrow(obj1@selectedpws)))
})

# Test that obj1@conditions has columns needed
test_that("obj1@conditions has columns needed", {
  expect_that(names(obj1@conditions), testthat::matches("temp_f",all = FALSE))
  expect_that(names(obj1@conditions), testthat::matches("relative_humidity",all = FALSE))
  expect_that(names(obj1@conditions), testthat::matches("wind_mph",all = FALSE))
  expect_that(names(obj1@conditions), testthat::matches("pressure_in",all = FALSE))
  expect_that(names(obj1@conditions), testthat::matches("feelslike_f",all = FALSE))
  expect_that(names(obj1@conditions), testthat::matches("precip_today_in",all = FALSE))
})

# Get historical weather data for selected PWS
dfhistdata <- getHistoricalWeather(obj1,startDt="2017/01/01",endDt="2017/01/01",maxPWScount = 2)

# Test that PWS in historical data are same as in obj1@selectedpws
test_that("PWS in historical data are same as in obj1@selectedpws",{
  expect_that(unique(dfhistdata$ID), testthat::equals(unique(obj1@selectedpws$id)))
})

# Test that dfhistdata has columns needed
test_that("dfhistdata has columns needed", {
  expect_that(names(dfhistdata), testthat::matches("tempm",all = FALSE))
  expect_that(names(dfhistdata), testthat::matches("tempi",all = FALSE))
  expect_that(names(dfhistdata), testthat::matches("dewptm",all = FALSE))
  expect_that(names(dfhistdata), testthat::matches("dewpti",all = FALSE))
  expect_that(names(dfhistdata), testthat::matches("hum",all = FALSE))
  expect_that(names(dfhistdata), testthat::matches("pressurem",all = FALSE))
  expect_that(names(dfhistdata), testthat::matches("pressurei",all = FALSE))
})