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
  u <- SVGUnit.factory(10,"in",dpi=75)
  expect_equal(uUser(u),750)
  
  ## 3. String factory
  u <- SVGUnit.factory("12.456pc")
  expect_equal(uValue(u),12.456)
  expect_equal(uUnits(u),"pc")
  
  ## 4. Target unit factory
  u <- SVGUnit.factory(10,"in",target.unit="px")
  expect_equal(uValue(u),900)
  expect_equal(uUnits(u),"px")

})

test_that("Operations", {
  
  ## 1. Arithmetic (only one ops)
  u <- SVGUnit.factory(1.5,"cm") + SVGUnit.factory(50,"mm")
  expect_is(u,"SVGUnit")
  expect_equal(uValue(u),6.5)
  expect_equal(uUnits(u),"cm")
  
  ## 2. Comparison (only one ops)
  expect_true(SVGUnit.factory(1,"cm") < SVGUnit.factory(1,"in"))
  
  ## 3. Logic (only one ops)
  expect_false(SVGUnit.factory(1,"cm") & SVGUnit.factory())
  expect_false(SVGUnit.factory(1,"cm") & SVGUnit.factory(0,"cm"))
  
})