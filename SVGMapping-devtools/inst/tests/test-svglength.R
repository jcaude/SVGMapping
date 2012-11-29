require(testthat)

context("SVGLength")

test_that("Accessors", {
  
  ## init.
  u <- SVGLength.factory()
  
  ## empty unit
  expect_equal(uValue(u),0)
  expect_equal(uUnits(u),"")
  expect_equal(uDpi(u),90)
  expect_equal(uUser(u),0)
  expect_output(print(u),"0")
  expect_false(isRelative(u))
  expect_is(u,"SVGLength")
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
  expect_false(isRelative(u))
  
})

test_that("Factory", {
  
  ## 1. Units set factory
  u <- SVGLength.factory(10.16,"cm")
  expect_equal(uValue(u),10.16)
  expect_equal(uUnits(u),"cm")
  expect_equal(uDpi(u),90)
  expect_equal(uUser(u),360)
  expect_output(print(u),"10.16cm")
  expect_is(u,"SVGLength")
  
  ## 2. DPI set factory
  u <- SVGLength.factory(10,"in",dpi=75)
  expect_equal(uUser(u),750)
  
  ## 3. String factory
  u <- SVGLength.factory("12.456pc")
  expect_equal(uValue(u),12.456)
  expect_equal(uUnits(u),"pc")
  
  ## 4. Target unit factory
  u <- SVGLength.factory(10,"in",target.unit="px")
  expect_equal(uValue(u),900)
  expect_equal(uUnits(u),"px")

  ## 5. Relative units test
  u <- SVGLength.factory(5,"%")
  expect_equal(uValue(u),5)
  expect_equal(uUnits(u),"%")
  expect_equal(uUser(u),NA)
  expect_true(isRelative(u))
  
})

test_that("Operations", {
  
  ## 1. Arithmetic (only one ops)
  u <- SVGLength.factory(1.5,"cm") + SVGLength.factory(50,"mm")
  expect_is(u,"SVGLength")
  expect_equal(uValue(u),6.5)
  expect_equal(uUnits(u),"cm")
  
  ## 2. Comparison (only one ops)
  expect_true(SVGLength.factory(1,"cm") < SVGLength.factory(1,"in"))
  
  ## 3. Logic (only one ops)
  expect_false(SVGLength.factory(1,"cm") & SVGLength.factory())
  expect_false(SVGLength.factory(1,"cm") & SVGLength.factory(0,"cm"))
  
  ## 4. Relative ops
  expect_error(SVGLength.factory(1.4,"cm") + SVGLength.factory(1,"em"))
  expect_equal(SVGLength.factory(190,"ex") < SVGLength.factory(1,"in"),NA)
  expect_false(SVGLength.factory(0.5,"%") & SVGLength.factory())
  expect_equal(SVGLength.factory(0.5,"%") & SVGLength.factory(1,"px"),NA)
})