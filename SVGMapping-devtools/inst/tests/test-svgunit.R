require(testthat)

context("SVGUnit")

test_that("Accessors", {
  
  ## init.
  u <- SVGUnit.factory()
  
  ## empty unit
  expect_equal(uValue(u),0)
  expect_equal(uUnits(u),"")
  expect_equal(uDpi(u),90)
  expect_equal(uUser(u),0)
  expect_output(print(u),"0")
  expect_is(u,"SVGUnit")
  
  ## set fields
  uValue(u) <- 15
  uDpi(u) <- 72
  
  ## check
  expect_equal(uValue(u),15)
  expect_equal(uUnits(u),"")
  expect_equal(uDpi(u),72)
  expect_equal(uUser(u),15)
  expect_output(print(u),"15")
  expect_is(u,"SVGUnit")
  
})

test_that("Factory", {
  
  ## 1. Units set factory
  u <- SVGUnit.factory(10.16,"cm")
  expect_equal(uValue(u),10.16)
  expect_equal(uUnits(u),"cm")
  expect_equal(uDpi(u),90)
  expect_equal(uUser(u),360)
  expect_output(print(u),"10.16cm")
  expect_is(u,"SVGUnit")
  
  ## 2. DPI set factory
  
})